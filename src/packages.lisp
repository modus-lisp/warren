;;;; packages.lisp — warren package definition.
(defpackage #:warren
  (:use #:cl)
  (:export
   ;; public entry points
   #:run #:render-to-png
   ;; embeddable surface (host-owned framebuffer; e.g. a glass WM window)
   #:desktop-surface
   ;; user-tunable
   #:*show-hidden* #:*preview-fn*))
