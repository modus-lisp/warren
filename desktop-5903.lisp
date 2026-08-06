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
    (ignore-errors (asdf:load-system :glass/audio-stream))    ; the session's sound (optional)
    (ignore-errors (asdf:load-system :glass/speech))          ; and its voice, via chord (optional)
    (asdf:load-asd "/home/claude/glass/backend/mcclim-glass.asd")
    (asdf:load-system :mcclim-glass)
    (ignore-errors (asdf:load-system :mcclim-glass/speak))    ; type-and-say window (optional)
    (ignore-errors (asdf:load-system :spool/app))))           ; the podcast client (optional)

(setf glass:*desktop-name* "modus-lisp :: glass desktop (:3)")

;;; Register warren as a generic :surface app so "File Browser" appears in the root
;;; menu (found by name — a no-op if warren didn't load).
(when (find-package :warren)
  (ignore-errors
   (clim-glass:register-app "File Browser"
     (list :surface (symbol-function (find-symbol "DESKTOP-SURFACE" :warren))
           :title "Files" :width 1000 :height 640))))

;;; Same for the podcast client, which registers itself (SPOOL.APP:REGISTER looks
;;; CLIM-GLASS up by name, so the system also loads in an image without a desktop).
(when (find-package :spool.app)
  (ignore-errors (funcall (find-symbol "REGISTER" :spool.app))))

;;; And the window that types into the desktop's voice — also self-registering, and
;;; absent when glass/speech didn't load (a desktop with no voice, not a broken one).
(when (find-package :glass-speak)
  (ignore-errors (funcall (find-symbol "REGISTER" :glass-speak))))

;;; Nothing about speech is registered here any more, because the WM offers both halves
;;; of it in the place each one belongs, and it does so on every glass desktop rather
;;; than only on this one.
;;;
;;; Right-click INSIDE a window that has text highlighted and "Speak selection" comes up
;;; under the pointer, over the words, and says exactly what is highlighted right now
;;; (glass's WM-OPEN-SELECTION-MENU).  Dismiss the highlight and the menu is gone with
;;; it — there is no selection, so there is nothing to offer about one.
;;;
;;; Right-click the workspace and the root menu ends with "Speak clipboard", which reads
;;; whatever was last COPIED (glass's WM-CLIPBOARD-MENU-ITEMS).  That one is the
;;; session's rather than any window's, which is why it is reachable from the bare
;;; background where there is no window to ask — and why it still has something to say
;;; after the highlight it came from is gone.

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
                    ;; let* — not let: the form must be READ with *package* already bound to
                    ;; clim-glass, otherwise symbols resolve in whatever package the thread has
                    ;; (CL-USER) and every unqualified clim-glass symbol fails to intern.
                    (let* ((*package* (find-package :clim-glass))
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
  ;; The session's sound, on its own port beside the screen: one mixer here, and any number of
  ;; listeners in other processes (the WebRTC gateway is one) subscribing to the same mix.  It
  ;; belongs on THIS side because this is the process the desktop's applications run in — a
  ;; transport that made its own mix would carry only what that transport's clients caused.
  ;; Found by name, so a build without :glass/audio-stream still starts a desktop.
  (let ((start (find-symbol "START-SESSION-AUDIO" :glass)))
    (if (and start (fboundp start))
        ;; :FILE NIL — the desktop's sound is whatever an app plays into the mix (spool, say),
        ;; not a track the startup loops on its own.  START-SESSION-AUDIO otherwise defaults
        ;; :FILE to $GLASS_AUDIO_MP3, which would resurrect the loop on any restart that
        ;; inherited it.
        (funcall start :port 5913 :address "127.0.0.1" :file nil)
        (format *error-output* "~&@@ audio: :glass/audio-stream not loaded — no session sound~%")))
  ;; The voice (glass/src/speech.lisp): chord as one long-lived source on the same mix, so a
  ;; SPEAK over the control socket is heard by every listener the session already has.  The path
  ;; is filled in only if :glass/speech loaded AND the voice is on this box; GLASS_VOICE still
  ;; wins, and leaving it NIL is what makes SPEAK say "no voice" instead of failing obscurely.
  (let ((var (find-symbol "*SPEECH-VOICE*" :glass))
        (here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (and var (boundp var) (null (symbol-value var)) (probe-file here))
      (setf (symbol-value var) here)))
  (format *error-output* "~&@@ glass desktop serving on 0.0.0.0:5903 (~a)~%" wp)
  (format *error-output* "@@ control socket on 127.0.0.1:4013~%")
  (format *error-output* "@@ voice: ~:[none — set GLASS_VOICE~;~:*~a~]~%"
          (let ((var (find-symbol "*SPEECH-VOICE*" :glass)))
            (and var (boundp var) (symbol-value var))))
  (finish-output *error-output*)
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port 5903 :width 1280 :height 800
                     :background wp :background-mode :cover))
