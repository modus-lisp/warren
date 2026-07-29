;;;; desktop-5903.lisp — a fresh persistent OPEN LOOK glass desktop on :5903.
;;;;
;;;; A FULL fresh load of the current on-disk stack (hardened glass WM, current gesso
;;;; with real DRAW-IMAGE-RGBA — no live shim, warren with async image previews),
;;;; mirroring glass/backend/inspect/serve-desktop.lisp but on port 5903 with the
;;;; warren "File Browser" registered in the root menu.  Point a VNC client at :5903.
;;;;   sbcl --control-stack-size 256 --dynamic-space-size 4096 --load desktop-5903.lisp
(require :asdf)
(load "~/quicklisp/setup.lisp")
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (ql:quickload '(:glass :glass/vncauth :mcclim :mcclim-render :sb-concurrency
                    :pigment :clim-examples :clim-listener))   ; vncauth = DES via seal
    (ignore-errors (asdf:load-system :loom/glass))            ; the browser (optional)
    (ignore-errors (asdf:load-system :warren))                ; the file browser (async previews)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)))

(setf glass:*desktop-name* "modus-lisp :: glass desktop (:3)")

;;; Register warren as a generic :surface app so "File Browser" appears in the root
;;; menu (found by name — a no-op if warren didn't load).
(when (find-package :warren)
  (ignore-errors
   (clim-glass:register-app "File Browser"
     (list :surface (symbol-function (find-symbol "DESKTOP-SURFACE" :warren))
           :title "Files" :width 1000 :height 640))))

;;; Bare-TCP control/eval socket on 127.0.0.1:4013 — read one form, eval it in the
;;; clim-glass package, write the printed result (same protocol as :5901's 4009).
(defun start-control-socket (&optional (port 4013))
  (sb-thread:make-thread
   (lambda ()
     (let ((listen (glass:tcp-listen port :address "127.0.0.1")))
       (loop
         (handler-case
             (let ((s (sb-bsd-sockets:socket-make-stream
                       (sb-bsd-sockets:socket-accept listen)
                       :input t :output t :element-type 'character :buffering :full)))
               (unwind-protect
                    (let ((*package* (find-package :clim-glass))
                          (form (read s nil nil)))
                      (when form
                        (write-string
                         (handler-case (princ-to-string (eval form))
                           (error (e) (format nil "ERROR: ~a" e)))
                         s)
                        (terpri s) (force-output s)))
                 (ignore-errors (close s))))
           (error () nil)))))
   :name "glass-control-5903"))

;; VNC password: if ~/.glass-vnc-pass exists, require + verify it; else open posture.
(let ((pwfile (merge-pathnames ".glass-vnc-pass" (user-homedir-pathname))))
  (when (probe-file pwfile)
    (let ((pw (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (with-open-file (in pwfile) (or (read-line in nil "") "")))))
      (when (plusp (length pw)) (setf glass:*vnc-password* pw)))))

(let ((wp (namestring (merge-pathnames "assets/wallpaper.svg"
                                       (asdf:system-source-directory :mcclim-glass)))))
  (start-control-socket 4013)
  (format *error-output* "~&@@ glass desktop serving on 0.0.0.0:5903 (~a)~%" wp)
  (format *error-output* "@@ control socket on 127.0.0.1:4013~%")
  (finish-output *error-output*)
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port 5903 :width 1280 :height 800
                     :background wp :background-mode :cover))
