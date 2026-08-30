;;;; warren.asd — a Finder-style Miller-column file browser in pure Common Lisp.
(asdf:defsystem :warren
  :description "A macOS Finder-style Miller (cascading) column file browser,
rendered with gesso (2D vector drawing) onto a glass framebuffer and served over
VNC.  Pure Common Lisp, no FFI: gesso paints an antialiased UI, glass ships it to
any VNC client, and single-click navigation builds cascading directory columns."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  ;; sb-posix for ACCESS(2): a directory the process may not read comes back from
  ;; UIOP as empty rather than as an error, and telling those apart is the whole job
  ;; of LIST-DIR's second return value.  Looked up by name in fs.lisp, so this is a
  ;; declaration that it will be there rather than a hard call into it.
  :depends-on ("gesso" "glass" "scribe" "pigment" #+unix "sb-posix")
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "packages")
     (:file "fs")      ; directory listing + column model
     (:file "icons")   ; folder / file / chevron gesso glyphs
     (:file "view")    ; layout, drawing, hit-testing
     (:file "app")))))  ; glass-app struct, input callbacks, pump loop, run
