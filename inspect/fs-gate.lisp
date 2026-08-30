;;;; fs-gate.lisp — the listing rules, against a directory built for the purpose.
;;;;
;;;;     sbcl --load inspect/fs-gate.lisp
;;;;
;;;; LIST-DIR is where a file browser is most easily wrong in ways nobody notices:
;;;; the order is a promise (directories first, then files, each case-insensitively
;;;; sorted), dotfiles are hidden by a rule rather than by accident, and an
;;;; unreadable directory has to come back as "no entries, and I could not read it"
;;;; rather than as an empty directory — those two look identical to a caller that
;;;; only checks the list.
;;;;
;;;; So this builds a directory with a known-awkward mix, reads it, and asserts the
;;;; order and the flags.  Nothing here needs a window, a network or a fixture file:
;;;; the tree is created in a temp directory and removed afterwards.

(require :asdf)
(require :sb-posix)
(let ((here (make-pathname :name nil :type nil
                           :defaults (or *load-truename* *compile-file-truename*))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :warren)))

(in-package #:warren)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defparameter *root*
  (merge-pathnames (format nil "warren-fs-gate-~d/" (sb-posix:getpid)) #p"/tmp/"))

(defun touch (name)
  (with-open-file (s (merge-pathnames name *root*) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (write-string "x" s)))

;; A deliberately awkward mix: mixed case (so a case-SENSITIVE sort would order them
;; differently), a dotfile and a dot-directory, and both kinds of entry.
(ensure-directories-exist (merge-pathnames "Beta/" *root*))
(ensure-directories-exist (merge-pathnames "alpha/" *root*))
(ensure-directories-exist (merge-pathnames ".hidden-dir/" *root*))
(touch "Zebra.txt") (touch "apple.md") (touch ".hidden-file")

(format t "~&== what a directory listing promises ==~%")
(multiple-value-bind (entries readable) (list-dir *root*)
  (ok "a readable directory says so" readable)
  (let ((names (mapcar #'entry-name entries)))
    (ok "dotfiles are hidden by default"
        (notany (lambda (n) (char= (char n 0) #\.)) names)
        (format nil "~a" names))
    (ok "directories come before files"
        (let ((dirs (mapcar #'entry-name (remove-if-not #'entry-dir-p entries)))
              (files (mapcar #'entry-name (remove-if #'entry-dir-p entries))))
          (equal names (append dirs files)))
        (format nil "~a" names))
    ;; The point of the mixed case: "Beta" before "alpha" is a case-SENSITIVE sort,
    ;; which is the bug this asserts against.
    (ok "…and each group is sorted case-INSENSITIVELY"
        (equal names '("alpha" "Beta" "apple.md" "Zebra.txt"))
        (format nil "~a" names))))

(format t "~&== showing the hidden ones is a choice ==~%")
(let ((*show-hidden* t))
  (multiple-value-bind (entries readable) (list-dir *root*)
    (declare (ignore readable))
    (let ((names (mapcar #'entry-name entries)))
      (ok "*SHOW-HIDDEN* reveals the dotfile and the dot-directory"
          (and (member ".hidden-file" names :test #'string=)
               (member ".hidden-dir" names :test #'string=))
          (format nil "~a" names)))))

(format t "~&== a directory that cannot be read is not an empty one ==~%")
(let ((locked (merge-pathnames "locked/" *root*)))
  (ensure-directories-exist locked)
  (touch "locked/secret.txt")
  (sb-posix:chmod (namestring (string-right-trim "/" (namestring locked))) #o000)
  (multiple-value-bind (entries readable) (list-dir locked)
    ;; Running as root would make this readable anyway, and the check would be a lie.
    (if (zerop (sb-posix:getuid))
        (format t "  skip unreadable-directory check — running as root~%")
        (progn
          (ok "an unreadable directory reports NIL entries" (null entries))
          (ok "…AND says it could not be read, which is the different thing"
              (null readable)))))
  (sb-posix:chmod (namestring (string-right-trim "/" (namestring locked))) #o755))

(format t "~&== the small pure helpers ==~%")
(ok "a leading dot is what hidden means" (and (entry-hidden-p ".x") (not (entry-hidden-p "x"))))
(ok "an empty name is not hidden" (not (entry-hidden-p "")))
(ok "bytes stay bytes below 1K" (string= (human-size 512) "512 bytes") (human-size 512))
(ok "and become KB above it" (search "KB" (human-size 2048)) (human-size 2048))
(ok "a missing size is a dash, not a zero" (string= (human-size nil) "—") (human-size nil))
(ok "a folder knows it is one"
    (string= (file-kind (make-entry :name "d" :path *root* :dir-p t)) "Folder"))
(ok "an extension becomes the kind"
    (string= (file-kind (make-entry :name "a.md" :path (merge-pathnames "apple.md" *root*)))
             "MD file")
    (file-kind (make-entry :name "a.md" :path (merge-pathnames "apple.md" *root*))))

(ignore-errors (uiop:delete-directory-tree *root* :validate t))
(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
