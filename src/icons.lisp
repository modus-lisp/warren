;;;; icons.lisp — tiny vector glyphs drawn straight into a gesso context.
;;;;
;;;; No image assets: a folder, a document, and a row chevron, each described as a
;;;; path and filled.  All take the context, a top-left (X,Y), and an (r g b) color
;;;; list so the caller can flip to white for a selected row.
(in-package #:warren)

(defun %poly (ctx color points)
  "Fill the closed polygon POINTS (a list of (x . y)) with COLOR."
  (gesso:set-fill ctx color)
  (gesso:begin-path ctx)
  (let ((first t))
    (dolist (p points)
      (if first
          (progn (gesso:move-to ctx (car p) (cdr p)) (setf first nil))
          (gesso:line-to ctx (car p) (cdr p)))))
  (gesso:close-path ctx)
  (gesso:fill-path ctx))

(defparameter +folder-color+ '(94 160 240)
  "Soft macOS folder blue.")
(defparameter +folder-tab-color+ '(120 178 246))
(defparameter +file-color+ '(150 150 150))
(defparameter +file-fold-color+ '(120 120 120))

;;; Icons are designed in a ~16 x 16 box; X,Y is its top-left.
(defun draw-folder (ctx x y &optional (color +folder-color+) (tab-color +folder-tab-color+))
  "A folder: a body rectangle with a raised tab on the upper-left."
  ;; tab
  (%poly ctx tab-color
         (list (cons (+ x 0.5) (+ y 2)) (cons (+ x 6) (+ y 2))
               (cons (+ x 7.5) (+ y 4)) (cons (+ x 0.5) (+ y 4))))
  ;; body
  (%poly ctx color
         (list (cons (+ x 0.5) (+ y 3.5)) (cons (+ x 15.5) (+ y 3.5))
               (cons (+ x 15.5) (+ y 13.5)) (cons (+ x 0.5) (+ y 13.5)))))

(defun draw-file (ctx x y &optional (color +file-color+) (fold-color +file-fold-color+))
  "A document: a page with a folded upper-right corner."
  (let ((x0 (+ x 2.5)) (x1 (+ x 13.5)) (y0 (+ y 1.5)) (y1 (+ y 14.5)) (fold 4))
    ;; page outline with the corner cut
    (%poly ctx color
           (list (cons x0 y0) (cons (- x1 fold) y0) (cons x1 (+ y0 fold))
                 (cons x1 y1) (cons x0 y1)))
    ;; the folded corner triangle, a touch darker
    (%poly ctx fold-color
           (list (cons (- x1 fold) y0) (cons x1 (+ y0 fold))
                 (cons (- x1 fold) (+ y0 fold))))))

(defun draw-chevron (ctx x y &optional (color '(150 150 150)))
  "A small right-pointing filled triangle (the `descend' affordance).  X,Y is the
   top-left of its ~5x9 box."
  (%poly ctx color
         (list (cons x y) (cons (+ x 5) (+ y 4.5)) (cons x (+ y 9)))))
