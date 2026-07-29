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
  (scroll 0))   ; vertical scroll offset in px, >= 0

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
