;;;; fs.lisp — filesystem listing and the Miller-column model.
;;;;
;;;; A COLUMN is one directory's contents shown as a vertical list of ENTRY rows,
;;;; plus a selection index and a vertical scroll offset.  The browser is a list of
;;;; columns laid left to right; clicking a directory row appends the column for
;;;; that directory and discards everything to its right (see view/app).
(in-package #:warren)

(defvar *show-hidden* nil
  "When NIL (default), dotfiles (names beginning with `.') are hidden from listings.")

;;; ---------------------------------------------------------------------------
;;; Entries
;;; ---------------------------------------------------------------------------
(defstruct entry
  name        ; display string (last path component / file-namestring)
  path        ; the pathname (a directory pathname for dirs)
  dir-p)      ; T for a subdirectory, NIL for a file

(defun %dir-display-name (dir)
  "Last path component of a directory pathname, e.g. #p\"/a/b/\" -> \"b\"."
  (let ((d (pathname-directory dir)))
    (or (car (last d)) (namestring dir))))

(defun entry-size (entry)
  "File size in bytes, or NIL (directories, or an unreadable file)."
  (unless (entry-dir-p entry)
    (ignore-errors
     (with-open-file (s (entry-path entry) :element-type '(unsigned-byte 8))
       (file-length s)))))

(defun entry-hidden-p (name)
  (and (plusp (length name)) (char= (char name 0) #\.)))

;;; ---------------------------------------------------------------------------
;;; Listing
;;; ---------------------------------------------------------------------------
(defun %dir-readable-p (path)
  "Can this process actually list PATH?

   ASKED SEPARATELY, because the listing cannot answer it.  UIOP:SUBDIRECTORIES and
   UIOP:DIRECTORY-FILES both return NIL for a directory they are not allowed to read,
   WITHOUT signalling anything — so a HANDLER-CASE around them never fires, and an
   unreadable directory arrives looking exactly like an empty one.  That is the single
   distinction LIST-DIR exists to report, and it was quietly wrong: a folder somebody
   has no permission for rendered as a folder with nothing in it.

   Looked up by name so this file keeps its short dependency list, and answers T where
   the platform has no such notion — there, everything is readable until it is not."
  (let ((access (and (find-package "SB-POSIX") (find-symbol "ACCESS" "SB-POSIX")))
        (r-ok (and (find-package "SB-POSIX") (find-symbol "R-OK" "SB-POSIX")))
        (x-ok (and (find-package "SB-POSIX") (find-symbol "X-OK" "SB-POSIX"))))
    (if (and access (fboundp access) r-ok (boundp r-ok) x-ok (boundp x-ok))
        (handler-case
            (progn (funcall access (string-right-trim "/" (namestring path))
                            (logior (symbol-value r-ok) (symbol-value x-ok)))
                   t)
          (error () nil))
        t)))

(defun list-dir (path)
  "List directory PATH.  Returns (values entries readable-p): a list of ENTRY
   structs — directories first, then files, each group case-insensitively sorted
   by name — and a flag that is NIL when the directory could not be read.  Dotfiles
   are hidden unless *SHOW-HIDDEN*.  Never signals; an unreadable dir yields NIL
   entries and a NIL flag."
  (unless (%dir-readable-p path)
    (return-from list-dir (values nil nil)))
  (handler-case
      (let ((dirs '()) (files '()))
        (dolist (d (uiop:subdirectories path))
          (let ((name (%dir-display-name d)))
            (when (or *show-hidden* (not (entry-hidden-p name)))
              (push (make-entry :name name :path d :dir-p t) dirs))))
        (dolist (f (uiop:directory-files path))
          (let ((name (file-namestring f)))
            (when (and (plusp (length name))
                       (or *show-hidden* (not (entry-hidden-p name))))
              (push (make-entry :name name :path f :dir-p nil) files))))
        (flet ((by-name (a b) (string-lessp (entry-name a) (entry-name b))))
          (values (nconc (sort dirs #'by-name) (sort files #'by-name)) t)))
    (error () (values nil nil))))

;;; ---------------------------------------------------------------------------
;;; Columns
;;; ---------------------------------------------------------------------------
;;; A :dir column lists PATH's contents.  A :info column is the trailing panel that
;;; describes a selected FILE (name / size / kind) — it holds the entry, not a list.
(defstruct column
  (kind :dir)   ; :dir (a directory listing) or :info (a file info panel)
  path          ; directory pathname (:dir) or the file's pathname (:info)
  entries       ; list of ENTRY (:dir only)
  entry         ; the ENTRY being described (:info only)
  (readable t)  ; NIL when the directory could not be read (:dir only)
  (sel nil)     ; selected row index, or NIL
  (scroll 0)    ; vertical scroll offset in px, >= 0
  ;; image-preview cache (:info only): decoded ONCE per selection, since a fresh
  ;; :info column is built on every selection change (see select-in-column).
  (preview nil)        ; a decoded pigment:img, or NIL
  (preview-status nil)); NIL untried / :ok / :not-image / :fail / :too-big

(defun make-dir-column (path)
  "A :dir column for directory PATH, contents listed and sorted."
  (multiple-value-bind (entries readable) (list-dir path)
    (make-column :kind :dir :path path :entries entries :readable readable)))

(defun make-info-column (entry)
  "A trailing :info panel describing the selected file ENTRY."
  (make-column :kind :info :path (entry-path entry) :entry entry))

(defun column-selected-entry (col)
  "The ENTRY currently selected in COL, or NIL."
  (let ((i (column-sel col)))
    (when (and i (eq (column-kind col) :dir) (< -1 i (length (column-entries col))))
      (nth i (column-entries col)))))

(defun human-size (n)
  "A short human-readable byte count, e.g. 4.2 KB."
  (cond ((null n) "—")
        ((< n 1024) (format nil "~D bytes" n))
        ((< n (* 1024 1024)) (format nil "~,1F KB" (/ n 1024d0)))
        ((< n (* 1024 1024 1024)) (format nil "~,1F MB" (/ n (* 1024d0 1024))))
        (t (format nil "~,1F GB" (/ n (* 1024d0 1024 1024))))))

(defun file-kind (entry)
  "A coarse kind label from the file's extension."
  (if (entry-dir-p entry)
      "Folder"
      (let ((type (pathname-type (entry-path entry))))
        (if (and type (plusp (length type)))
            (format nil "~:@(~A~) file" type)
            "Document"))))

;;; ---------------------------------------------------------------------------
;;; Image previews — decode a selected image file to a thumbnail (via pigment)
;;; ---------------------------------------------------------------------------
;;; Decoding is ASYNC (see app.lisp START-PREVIEW): the functions here run OFF the
;;; app lock on a worker thread — read bytes, cheap dimension pre-probe, full decode,
;;; then pre-scale to the pane box so the input/pump thread never touches a decode.
(defparameter +image-types+ '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")
  "File extensions warren tries to show as a decoded bitmap in the info pane.")

(defparameter *preview-max-bytes* (* 24 1024 1024)
  "Skip preview decode for image files larger than this (bytes) so a huge file can't
   burn a decode; those fall back to the doc glyph.")

(defparameter *preview-max-megapixels* 40
  "Reject (without a full decode) images whose header reports more than this many
   megapixels — the cheap dimension pre-probe guards the worst stalls.")

(defparameter +preview-box-w+ 198 "Preview thumbnail box width in px (+col-w+ - 32).")
(defparameter +preview-box-h+ 150 "Preview thumbnail box height in px.")

;;; A resolved :ok preview: a small PRE-SCALED thumbnail img + the ORIGINAL pixel
;;; dimensions (shown in the info text).  Failures are cached as bare keywords.
(defstruct pv img ow oh)

(defun image-file-p (entry)
  "True when ENTRY's extension is one warren previews as a bitmap."
  (and (not (entry-dir-p entry))
       (let ((type (pathname-type (entry-path entry))))
         (and type (member type +image-types+ :test #'string-equal)))))

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun probe-image-size (bytes)
  "A CHEAP (values W H) from a PNG (IHDR) or JPEG (SOF) header without decoding any
   pixels, or NIL when unknown — lets the worker reject absurd-megapixel images before
   paying a full decode."
  (cond
    ((and (>= (length bytes) 24) (= (aref bytes 0) 137) (= (aref bytes 1) 80)
          (= (aref bytes 12) 73) (= (aref bytes 13) 72)                 ; "IHDR"
          (= (aref bytes 14) 68) (= (aref bytes 15) 82))
     (flet ((u32 (o) (+ (ash (aref bytes o) 24) (ash (aref bytes (+ o 1)) 16)
                        (ash (aref bytes (+ o 2)) 8) (aref bytes (+ o 3)))))
       (values (u32 16) (u32 20))))
    ((and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
     (ignore-errors (pigment:jpeg-size bytes)))
    (t (values nil nil))))

(defun prescale-img (img box-w box-h)
  "A NEW small pigment:img, nearest-neighbour scaled so IMG fits BOX-W x BOX-H (aspect
   preserved, upscale capped at 4x).  Pre-scaling once here means the paint path never
   re-samples a huge source per frame."
  (let* ((iw (pigment:img-w img)) (ih (pigment:img-h img))
         (src (pigment:img-rgba img))
         (s (min (/ box-w iw) (/ box-h ih) 4d0))
         (dw (max 1 (round (* iw s)))) (dh (max 1 (round (* ih s))))
         (out (make-array (* dw dh 4) :element-type '(unsigned-byte 8))))
    (dotimes (y dh)
      (let ((sy (min (1- ih) (floor (* y ih) dh))))
        (dotimes (x dw)
          (let ((si (* 4 (+ (* sy iw) (min (1- iw) (floor (* x iw) dw)))))
                (di (* 4 (+ (* y dw) x))))
            (setf (aref out di)       (aref src si)
                  (aref out (+ di 1)) (aref src (+ si 1))
                  (aref out (+ di 2)) (aref src (+ si 2))
                  (aref out (+ di 3)) (aref src (+ si 3)))))))
    (pigment:make-img :w dw :h dh :rgba out)))

(defun decode-preview-thumb (path box-w box-h)
  "OFF-LOCK worker body: decode the image at PATH and pre-scale it to fit BOX-W x
   BOX-H.  Returns a PV (a resolved :ok thumbnail) or a keyword — :too-big (over the
   byte / megapixel caps) or :fail (unreadable / corrupt / unsupported).  Never
   signals — the caller only needs to store the result."
  (handler-case
      (let ((size (ignore-errors (with-open-file (s path :element-type '(unsigned-byte 8))
                                   (file-length s)))))
        (if (and size (> size *preview-max-bytes*))
            :too-big
            (let ((bytes (%read-file-bytes path)))
              (multiple-value-bind (pw ph) (probe-image-size bytes)
                (if (and pw ph (> (* pw ph) (* *preview-max-megapixels* 1000000)))
                    :too-big
                    (let ((img (pigment:decode-image-bytes bytes)))
                      (if (and img (plusp (pigment:img-w img)) (plusp (pigment:img-h img)))
                          (make-pv :img (prescale-img img box-w box-h)
                                   :ow (pigment:img-w img) :oh (pigment:img-h img))
                          :fail)))))))
    (error () :fail)))
