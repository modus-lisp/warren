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
(defun list-dir (path)
  "List directory PATH.  Returns (values entries readable-p): a list of ENTRY
   structs — directories first, then files, each group case-insensitively sorted
   by name — and a flag that is NIL when the directory could not be read.  Dotfiles
   are hidden unless *SHOW-HIDDEN*.  Never signals; an unreadable dir yields NIL
   entries and a NIL flag."
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
(defparameter +image-types+ '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")
  "File extensions warren tries to show as a decoded bitmap in the info pane.")

(defparameter *preview-max-bytes* (* 24 1024 1024)
  "Skip preview decode for image files larger than this (bytes) so selecting a huge
   file can't stall the repaint pump; those fall back to the doc glyph.")

(defun image-file-p (entry)
  "True when ENTRY's extension is one warren previews as a bitmap."
  (and (not (entry-dir-p entry))
       (let ((type (pathname-type (entry-path entry))))
         (and type (member type +image-types+ :test #'string-equal)))))

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun ensure-preview (col)
  "Lazily decode COL's (:info) file into a cached preview pigment:img — ONCE per
   selection (the column is rebuilt whenever the selection changes, so this never
   runs per-paint).  Robust: a non-image, an over-large file, or a corrupt/unsupported
   image all resolve to a status the drawer falls back on, never signalling.
   Returns the img (or NIL); sets (column-preview-status col)."
  (when (and (eq (column-kind col) :info) (null (column-preview-status col)))
    (let ((entry (column-entry col)))
      (if (not (image-file-p entry))
          (setf (column-preview-status col) :not-image)
          (handler-case
              (if (> (or (entry-size entry) 0) *preview-max-bytes*)
                  (setf (column-preview-status col) :too-big)
                  (let ((img (pigment:decode-image-bytes
                              (%read-file-bytes (entry-path entry)))))
                    (if (and img (plusp (pigment:img-w img)) (plusp (pigment:img-h img)))
                        (setf (column-preview col) img
                              (column-preview-status col) :ok)
                        (setf (column-preview-status col) :fail))))
            (error () (setf (column-preview-status col) :fail))))))
  (column-preview col))
