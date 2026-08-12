;;;; tests.lisp — model assertions + visual-render harness (headless).
(require :asdf)
(asdf:load-system :warren)
(in-package :warren)

(defvar *fails* 0)
(defmacro check (form &optional (msg ""))
  `(if ,form
       (format t "  ok   ~a~%" ,msg)
       (progn (incf *fails*) (format t "  FAIL ~a  <= ~s~%" ,msg ',form))))

(defun col-names (col) (mapcar #'entry-name (column-entries col)))
(defun idx-of (col name) (position name (col-names col) :test #'string=))

(format t "~%== model tests ==~%")

;; Build a browser rooted at the workspace.
(defparameter *ws*                                ; the workspace holding the sibling repos
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*)))
(let* ((root *ws*)
       (app (make-browser :root root :width 1000 :height 640)))

  ;; 1. one column at start, listing the workspace
  (check (= 1 (length (app-columns app))) "starts with a single root column")
  (check (plusp (length (column-entries (first (app-columns app))))) "root column is non-empty")

  ;; directories sort before files (case-insensitive)
  (let ((es (column-entries (first (app-columns app)))))
    (check (let ((firstfile (position-if-not #'entry-dir-p es)))
             (or (null firstfile)
                 (notany #'entry-dir-p (nthcdr firstfile es))))
           "directories are grouped before files"))

  ;; 2. click the `reed' folder -> appends a column with its REAL contents
  (let ((ri (idx-of (first (app-columns app)) "reed")))
    (check ri "root lists the reed directory")
    (select-in-column app 0 ri)
    (check (= 2 (length (app-columns app))) "clicking a folder appends a 2nd column")
    (check (member "src" (col-names (second (app-columns app))) :test #'string=)
           "2nd column shows reed's real contents (src)"))

  ;; 3. click deeper: reed/src, then reed/src/mp3 -> 3+ columns
  (let ((si (idx-of (second (app-columns app)) "src")))
    (select-in-column app 1 si)
    (check (= 3 (length (app-columns app))) "clicking src builds a 3rd column")
    (let ((mi (idx-of (third (app-columns app)) "mp3")))
      (check mi "reed/src lists mp3")
      (select-in-column app 2 mi)
      (check (= 4 (length (app-columns app))) "clicking mp3 builds a 4th column")
      (check (member "decode.lisp" (col-names (fourth (app-columns app))) :test #'string=)
             "4th column shows mp3's real files")))

  ;; 4. click a FILE -> selects it + appends an :info column
  (let ((fi (idx-of (fourth (app-columns app)) "decode.lisp")))
    (select-in-column app 3 fi)
    (check (= 5 (length (app-columns app))) "clicking a file appends an info panel")
    (check (eq :info (column-kind (fifth (app-columns app)))) "trailing column is :info")
    (check (plusp (entry-size (column-entry (fifth (app-columns app)))))
           "info panel entry has a real byte size"))

  ;; 5. a click in a LEFT column truncates everything to its right
  (let ((ri2 (idx-of (first (app-columns app)) "reed")))
    (select-in-column app 0 ri2)
    (check (= 2 (length (app-columns app))) "clicking a left column truncates to the right"))

  ;; 6. wheel scroll changes offset and clamps at both ends
  (let ((col (first (app-columns app))))
    (setf (column-scroll col) 0)
    (scroll-under-pointer app 20 20 1)          ; wheel down under column 0
    (check (plusp (column-scroll col)) "wheel down increases scroll offset")
    (dotimes (i 200) (scroll-under-pointer app 20 20 1))
    (check (= (column-scroll col) (column-max-scroll col (app-vh app)))
           "scroll clamps at the bottom")
    (dotimes (i 200) (scroll-under-pointer app 20 20 -1))
    (check (zerop (column-scroll col)) "scroll clamps at the top (0)"))

  ;; 7. hit-test maps a device point back to (column,row) via on-pointer.
  ;; Row 0 is the first entry (a directory, since dirs sort first) and always visible.
  (let* ((app2 (make-browser :root *ws* :width 1000 :height 640))
         (e0 (first (column-entries (first (app-columns app2)))))
         (yc 12))                          ; vertical center of row 0
    (check (entry-dir-p e0) "row 0 of the root is a directory")
    (on-pointer app2 1 20 yc)              ; press left at column 0, row 0
    (on-pointer app2 0 20 yc)              ; release
    (check (= 2 (length (app-columns app2))) "on-pointer press at row 0 opens it")
    (check (eql 0 (column-sel (first (app-columns app2))))
           "synthetic click set the row-0 selection")))

(format t "~%== rendering PNGs ==~%")
(let ((shots (merge-pathnames "shots/"          ; beside this file, in the checkout
                              (make-pathname :name nil :type nil
                                             :defaults *load-truename*))))
  (ensure-directories-exist shots)
  ;; (a) initial home view
  (let ((app (make-browser :root *ws* :width 1000 :height 640)))
    (render-to-png app (merge-pathnames "home.png" shots))
    (format t "  wrote home.png~%"))
  ;; (b) 3-column deep navigation: <workspace> -> reed -> src
  (let ((app (make-browser :root *ws* :width 1000 :height 640)))
    (select-in-column app 0 (idx-of (first (app-columns app)) "reed"))
    (select-in-column app 1 (idx-of (second (app-columns app)) "src"))
    (render-to-png app (merge-pathnames "deep.png" shots))
    (format t "  wrote deep.png~%"))
  ;; (c) a file selected with its info column
  (let ((app (make-browser :root *ws* :width 1000 :height 640)))
    (select-in-column app 0 (idx-of (first (app-columns app)) "reed"))
    (select-in-column app 1 (idx-of (second (app-columns app)) "src"))
    (select-in-column app 2 (idx-of (third (app-columns app)) "mp3"))
    (select-in-column app 3 (idx-of (fourth (app-columns app)) "decode.lisp"))
    (render-to-png app (merge-pathnames "file.png" shots))
    (format t "  wrote file.png~%")))

(format t "~%== ~[ALL PASS~:;~:*~d FAILURE(S)~] ==~%" *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
