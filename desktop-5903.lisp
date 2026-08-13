;;;; desktop-5903.lisp — a fresh persistent OPEN LOOK glass desktop on :5903.
;;;;
;;;; A FULL fresh load of the current on-disk stack (hardened glass WM, current gesso
;;;; with real DRAW-IMAGE-RGBA — no live shim, warren with async image previews),
;;;; mirroring glass/backend/inspect/serve-desktop.lisp but on port 5903 with the
;;;; warren "File Browser" registered in the root menu.  It serves no PORT: the seat's wire
;;;; is a socket file under ~/.glass/run/, reached by the gateway over loopback-less UNIX.
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
    (ignore-errors (asdf:load-system :glass/mic-stream))      ; ...and a peer's microphone, inbound
    (ignore-errors (asdf:load-system :glass/speech))          ; and its voice, via chord (optional)
    (ignore-errors (asdf:load-system :glass/hearing))         ; and its ear, via stave (optional)
    (ignore-errors (asdf:load-system :glass/nostr))           ; the box's identity + the DM bot
    ;; glass's McCLIM backend: a sibling repo, found relative to this file.
    (asdf:load-asd (merge-pathnames "../glass/backend/mcclim-glass.asd"
                                    (make-pathname :name nil :type nil
                                                   :defaults *load-truename*)))
    (asdf:load-system :mcclim-glass)
    ;; The root menu offers "Editor (Climacs)", and WM-EDIT resolves it by name at
    ;; click time — so if nobody loads it the menu item is a promise the image
    ;; cannot keep, and the only sign of it is one line in the log.  It is a menu
    ;; entry: load what it opens.  Via mcclim-glass/climacs rather than :climacs
    ;; directly, because bare climacs comes up as a white box you cannot type in
    ;; and then dies a second after the first thing it says; that system is
    ;; climacs plus the upstream fixes for exactly that.
    (ignore-errors (asdf:load-system :mcclim-glass/climacs))
    (ignore-errors (asdf:load-system :mcclim-glass/speak))    ; type-and-say window (optional)
    (ignore-errors (asdf:load-system :mcclim-glass/listen))   ; ...and the one that fills itself in
    (ignore-errors (asdf:load-system :spool/app))             ; the podcast client (optional)
    (ignore-errors (asdf:load-system :mcclim-glass/remote))))  ; other desktops, as windows here

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

;;; And its mirror: press Listen and the box fills up with what the session is saying.  Same
;;; mixer as the voice, from the other end — which is why this desktop can hear itself talk, on a
;;; box whose only audio hardware is four HDMI playbacks and no capture device at all.
(when (find-package :glass-listen)
  (ignore-errors (funcall (find-symbol "REGISTER" :glass-listen))))

;;; Other desktops on this box, each as a window here (glass/src/rfb-client.lisp behind
;;; backend/remote-app.lisp).  Nothing is connected until the menu item is chosen, so an
;;; entry whose far end is not running costs this desktop nothing but a line in the menu —
;;; which is why both are registered unconditionally rather than probed for first.
;;;
;;; :5905 is cortez (the TI Explorer compatibility layer) and is worth one caveat: it is
;;; served by a GATE process (sbcl --non-interactive --load inspect/window-gate.lisp), not
;;; by a persistent desktop.  So that window will show "reconnecting..." whenever the gate
;;; between runs, and fill back in when the next one starts.  That is the client working,
;;; not the link failing.
(when (find-package :glass-remote)
  (ignore-errors
   (funcall (find-symbol "REGISTER" :glass-remote)
            :label "Remote desktop" :host "127.0.0.1" :port 5901))
  (ignore-errors
   (funcall (find-symbol "REGISTER" :glass-remote)
            :label "TI Explorer (:5)" :host "127.0.0.1" :port 5905
            :width 1100 :height 700)))

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

;;; A UNIX SOCKET, not a port, and this is the one that most needed it: read-one-form-and-EVAL
;;; is unauthenticated by construction, and on 127.0.0.1 its only protection was that you had to
;;; already be on the box — every uid on it could drive this desktop.  A socket file at 0600 is
;;; checked by the kernel, and GLASS's accept loop additionally refuses a peer whose uid is not
;;; ours (SO_PEERCRED), hanging up before the READ rather than after it.
;;;
;;;   nc -U ~/.glass/run/seat-0.control
(defun start-control-socket (&optional (path (glass:socket-path "seat-0.control")))
  (sb-thread:make-thread
   (lambda ()
     (let ((listen (glass:open-listener :unix :path path)))
       (loop
         (handler-case
             ;; ACCEPT-STREAM sets :INPUT/:OUTPUT itself and takes only ELEMENT-TYPE, BUFFERING
             ;; and TIMEOUT — passing the other two is an error that the HANDLER-CASE below
             ;; swallows, so the symptom is a socket that accepts and then says nothing at all.
             (let ((s (glass:accept-stream listen :element-type 'character :buffering :full)))
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
                                       (asdf:system-source-directory :mcclim-glass))))
      ;; One binding for the address, used by both the banner and RUN-WM.  They were two
      ;; literals and the banner kept saying 0.0.0.0 after the bind moved to loopback -- a log
      ;; line that misreports the posture is worse than no log line.
      ;; The seat's wire is a FILE now, not a port.  Everything else derives from it by name
      ;; (SOCKET-SIBLING), which is the socket-file reading of the old 5903 -> 5913/5914/5915
      ;; arithmetic -- kept in glass so the two ends cannot drift apart.
      (rfb (glass:socket-path "seat-0.rfb")))
  (start-control-socket (glass:socket-sibling rfb "control"))
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
        (funcall start :path (glass:socket-sibling rfb "audio") :file nil)
        (format *error-output* "~&@@ audio: :glass/audio-stream not loaded — no session sound~%")))
  ;; The other direction (glass/src/mic-stream.lisp): a peer's microphone, arriving from the
  ;; WebRTC gateway on the port beside the mix's.  It is NOT put in the mix — it would be played
  ;; back out of the desktop's own audio and down 5913 to the phone that said it — it is offered
  ;; to whoever asks, which today is the ear.  Found by name, so a build without it still starts.
  (let ((start (find-symbol "START-SESSION-MIC" :glass)))
    (if (and start (fboundp start))
        (funcall start :path (glass:socket-sibling rfb "mic"))
        (format *error-output* "~&@@ mic: :glass/mic-stream not loaded — no peer microphone~%")))
  ;; The box's identity and who may reach it (glass/src/nostr.lisp), on the port after the
  ;; microphone's.  This used to live in the WebRTC gateway, which was the wrong process for the
  ;; same reason a mixer there would have been: a transport can only speak for the sessions IT
  ;; carries, and the gateway is the disposable half — it respawns on every config change and
  ;; crashloops by design without an identity, taking the DM bot with it exactly when you would
  ;; want to ask the box what is wrong.  This process stays up for days.
  ;;
  ;; So the desktop answers the DMs and holds the enrolments, and the gateway asks it.  Which
  ;; means the failure here is not cosmetic like the ones above: with no admission service the
  ;; gateway fails CLOSED and nobody can connect at all.  Hence a louder line.
  (let ((start (find-symbol "START-SESSION-NOSTR" :glass)))
    (if (and start (fboundp start))
        (funcall start :path (glass:socket-sibling rfb "admit"))
        (format *error-output* "~&@@ nostr: :glass/nostr not loaded — NOBODY CAN CONNECT~%")))
  ;; The voice (glass/src/speech.lisp): chord as one long-lived source on the same mix, so a
  ;; SPEAK over the control socket is heard by every listener the session already has.  The path
  ;; is filled in only if :glass/speech loaded AND the voice is on this box; GLASS_VOICE still
  ;; wins, and leaving it NIL is what makes SPEAK say "no voice" instead of failing obscurely.
  (let ((var (find-symbol "*SPEECH-VOICE*" :glass))
        (here "/mnt/lisp/chord/export/en_US-lessac-medium.graph"))
    (when (and var (boundp var) (null (symbol-value var)) (probe-file here))
      (setf (symbol-value var) here)))
  ;; The ear (glass/src/hearing.lisp): stave as one SINK on that same mix.  Filled in the same
  ;; way and for the same reason — GLASS_EARS wins, and NIL is what makes the Listen window say
  ;; "no ear installed" instead of failing obscurely.  Nothing is loaded until something listens.
  (let ((var (find-symbol "*HEARING-MODELS*" :glass))
        (here "/mnt/lisp/stave/export/"))
    (when (and var (boundp var) (null (symbol-value var)) (probe-file here))
      (setf (symbol-value var) here)))
  (format *error-output* "~&@@ glass desktop serving on ~a (~a)~%" rfb wp)
  (format *error-output* "@@ control socket on ~a~%" (glass:socket-sibling rfb "control"))
  (format *error-output* "@@ voice: ~:[none — set GLASS_VOICE~;~:*~a~]~%"
          (let ((var (find-symbol "*SPEECH-VOICE*" :glass)))
            (and var (boundp var) (symbol-value var))))
  (format *error-output* "@@ ears:  ~:[none — set GLASS_EARS~;~:*~a~]~%"
          (let ((var (find-symbol "*HEARING-MODELS*" :glass)))
            (and var (boundp var) (symbol-value var))))
  (finish-output *error-output*)
  ;; LOOPBACK.  This used to bind 0.0.0.0, which meant an unauthenticated desktop on every
  ;; interface — and not by choice: a VNC password is the only auth RFB offers here, and setting
  ;; one BREAKS VIDEO, because glass-capture.lisp speaks security type None and would fail to
  ;; connect while the browser's bridged RFB kept working.  The secure configuration was the
  ;; broken one, so the posture was open.
  ;;
  ;; It closes now because nothing needs it open: the gateway connects over loopback
  ;; (GLASS_HOST=127.0.0.1), and the WebRTC path has been the only way in for months.  Anything
  ;; that wants a wire from elsewhere should be a transport a SEAT opens, with a credential the
  ;; seat chose — see glass/docs/seats-and-transports.md.
  (clim-glass:run-wm '((:terminal :cols 80 :rows 24 :ppem 14))
                     :port 5903 :width 1280 :height 800
                     :kind :rfb-unix :path rfb
                     :background wp :background-mode :cover))
