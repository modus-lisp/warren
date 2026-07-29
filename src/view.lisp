;;;; view.lisp — Miller-column layout, drawing (gesso), and hit-testing.
;;;;
;;;; Pure geometry over a list of COLUMNs: DRAW-VIEW repaints the whole UI into a
;;;; gesso context; HIT-TEST maps a device point back to (column, row).  No app
;;;; state or glass here — app.lisp wires these to the framebuffer and input.
(in-package #:warren)

;;; ---- metrics ---------------------------------------------------------------
(defparameter +col-w+ 230 "Column width in px.")
(defparameter +row-h+ 24 "Row height in px.")
(defparameter +pathbar-h+ 26 "Bottom path-bar height in px.")
(defparameter +text-size+ 13d0)
(defparameter +row-pad+ 10 "Left padding inside a column, in px.")
(defparameter *font-family* '("Liberation Sans" "DejaVu Sans" "Helvetica" "Arial"))

;;; ---- colors ----------------------------------------------------------------
(defparameter +window-bg+   '(246 246 246))
(defparameter +column-bg+   '(255 255 255))
(defparameter +hairline+    '(220 220 220))
(defparameter +text-color+  '(40 40 40))
(defparameter +muted-color+ '(150 150 150))
(defparameter +sel-active+  '(10 132 255) "Blue fill for the selection on the active column.")
(defparameter +sel-inactive+ '(216 216 216) "Gray fill for a path selection on an inactive column.")
(defparameter +white+       '(255 255 255))
(defparameter +pathbar-bg+  '(238 238 238))

;;; ---- helpers ---------------------------------------------------------------
(defun content-height (view-h) (max 0 (- view-h +pathbar-h+)))

(defun active-column-index (columns)
  "Index of the rightmost column that carries a selection (the active/focus
   column), or NIL when nothing is selected anywhere."
  (let ((idx nil))
    (loop for i from 0 for col in columns
          when (column-sel col) do (setf idx i))
    idx))

(defun column-row-count (col)
  (if (eq (column-kind col) :dir) (length (column-entries col)) 0))

(defun column-max-scroll (col view-h)
  (max 0 (- (* (column-row-count col) +row-h+) (content-height view-h))))

(defun column-x (i hscroll) (- (* i +col-w+) hscroll))

(defun fit-text (ctx str maxw)
  "STR trimmed with a trailing ellipsis so it fits in MAXW device px."
  (gesso:set-font ctx +text-size+ :family *font-family*)
  (if (<= (gesso:measure-text ctx str) maxw)
      str
      (let ((ell "…"))
        (loop for n from (1- (length str)) downto 1
              for cand = (concatenate 'string (subseq str 0 n) ell)
              when (<= (gesso:measure-text ctx cand) maxw) do (return cand)
              finally (return ell)))))

(defun %baseline (row-top)
  "Baseline y that vertically centers +text-size+ text in a row starting ROW-TOP."
  (+ row-top (/ +row-h+ 2d0) (* +text-size+ 0.35d0)))

;;; ---- drawing ---------------------------------------------------------------
(defun draw-row (ctx col-x row-top entry active-p selected-p view-h)
  "One directory row: optional selection fill, icon, name, chevron for dirs."
  (declare (ignore view-h))
  (let* ((fill (cond ((and selected-p active-p) +sel-active+)
                     (selected-p +sel-inactive+)
                     (t nil)))
         (white-fg (and selected-p active-p))
         (fg (if white-fg +white+ +text-color+))
         (icon-glyph-color (if white-fg +white+ nil)))
    (when fill
      (gesso:set-fill ctx fill)
      (gesso:fill-rect ctx (+ col-x 4) (+ row-top 1) (- +col-w+ 8) (- +row-h+ 2)))
    ;; icon
    (let ((ix (+ col-x +row-pad+)) (iy (+ row-top 4)))
      (if (entry-dir-p entry)
          (if icon-glyph-color
              (draw-folder ctx ix iy +white+ +white+)
              (draw-folder ctx ix iy))
          (if icon-glyph-color
              (draw-file ctx ix iy +white+ +white+)
              (draw-file ctx ix iy))))
    ;; name
    (let* ((tx (+ col-x +row-pad+ 22))
           (right-limit (+ col-x +col-w+ (- +row-pad+) -12))
           (name (fit-text ctx (entry-name entry) (- right-limit tx))))
      (gesso:set-fill ctx fg)
      (gesso:set-font ctx +text-size+ :family *font-family*)
      (gesso:fill-text ctx name tx (%baseline row-top)))
    ;; chevron for directories
    (when (entry-dir-p entry)
      (draw-chevron ctx (+ col-x +col-w+ -14) (+ row-top 8)
                    (if white-fg +white+ +muted-color+)))))

(defun draw-dir-column (ctx col col-x active-p view-h)
  "Draw a :dir column's background, rows (clipped + scrolled), and its selection."
  (let* ((ch (content-height view-h)))
    ;; background
    (gesso:set-fill ctx +column-bg+)
    (gesso:fill-rect ctx col-x 0 +col-w+ ch)
    ;; clip to the column's content box so scrolled rows never bleed
    (gesso:save ctx)
    (gesso:begin-path ctx)
    (gesso:rect ctx col-x 0 +col-w+ ch)
    (gesso:clip ctx)
    (if (column-readable col)
        (loop with scroll = (column-scroll col)
              for i from 0 for entry in (column-entries col)
              for row-top = (- (* i +row-h+) scroll)
              when (and (> (+ row-top +row-h+) 0) (< row-top ch))
                do (draw-row ctx col-x row-top entry active-p
                             (eql (column-sel col) i) view-h))
        ;; unreadable directory
        (progn
          (gesso:set-fill ctx +muted-color+)
          (gesso:set-font ctx +text-size+ :family *font-family*)
          (gesso:fill-text ctx "— unreadable —" (+ col-x +row-pad+) (%baseline 0))))
    (gesso:restore ctx)))

(defun draw-info-column (ctx col col-x view-h)
  "Draw a trailing :info panel for a selected file (name / kind / size)."
  (let* ((ch (content-height view-h))
         (entry (column-entry col))
         (x (+ col-x +row-pad+ 4)))
    (gesso:set-fill ctx +column-bg+)
    (gesso:fill-rect ctx col-x 0 +col-w+ ch)
    ;; big centered document glyph
    (let ((gx (+ col-x (/ (- +col-w+ 48) 2d0))))
      (gesso:save ctx)
      (gesso:translate ctx gx 34)
      (gesso:scale ctx 3d0 3d0)
      (draw-file ctx 0 0)
      (gesso:restore ctx))
    (let ((y 118))
      (gesso:set-fill ctx +text-color+)
      (gesso:set-font ctx 14d0 :family *font-family* :weight 600)
      (gesso:fill-text ctx (fit-text ctx (entry-name entry) (- +col-w+ 24))
                       x y)
      (incf y 26)
      (flet ((line (label value)
               (gesso:set-fill ctx +muted-color+)
               (gesso:set-font ctx 11d0 :family *font-family*)
               (gesso:fill-text ctx label x y)
               (gesso:set-fill ctx +text-color+)
               (gesso:set-font ctx +text-size+ :family *font-family*)
               (gesso:fill-text ctx value x (+ y 16))
               (incf y 40)))
        (line "Kind" (file-kind entry))
        (line "Size" (human-size (entry-size entry)))))))

(defun current-path-string (columns)
  "The full path of the active selection (for the path bar), or the root path."
  (let ((idx (active-column-index columns)))
    (if idx
        (let* ((col (nth idx columns))
               (e (column-selected-entry col)))
          (cond (e (namestring (entry-path e)))
                ((eq (column-kind col) :info) (namestring (column-path col)))
                (t (namestring (column-path col)))))
        (if columns (namestring (column-path (first columns))) ""))))

(defun draw-view (ctx columns hscroll view-w view-h)
  "Repaint the whole browser into CTX: window backdrop, every visible column with
   its hairline separator, and the bottom path bar."
  (let ((active (active-column-index columns)))
    ;; backdrop (shows through past the last column)
    (gesso:set-fill ctx +window-bg+)
    (gesso:fill-rect ctx 0 0 view-w view-h)
    ;; columns
    (loop for i from 0 for col in columns
          for cx = (column-x i hscroll)
          when (and (> (+ cx +col-w+) 0) (< cx view-w)) do
            (if (eq (column-kind col) :info)
                (draw-info-column ctx col cx view-h)
                (draw-dir-column ctx col cx (eql active i) view-h))
            ;; right-edge hairline
            (gesso:set-fill ctx +hairline+)
            (gesso:fill-rect ctx (+ cx +col-w+ -1) 0 1 (content-height view-h)))
    ;; path bar
    (let ((y (content-height view-h)))
      (gesso:set-fill ctx +pathbar-bg+)
      (gesso:fill-rect ctx 0 y view-w +pathbar-h+)
      (gesso:set-fill ctx +hairline+)
      (gesso:fill-rect ctx 0 y view-w 1)
      (gesso:set-fill ctx '(90 90 90))
      (gesso:set-font ctx 11d0 :family *font-family*)
      (gesso:fill-text ctx (fit-text ctx (current-path-string columns) (- view-w 20))
                       10 (+ y (/ +pathbar-h+ 2d0) 3.5d0)))))

;;; ---- hit-testing -----------------------------------------------------------
(defun hit-test (columns hscroll view-w view-h x y)
  "Map device point (X,Y) to (values column-index row-index), or NIL when the
   point is outside any column's content area or in the path bar.  ROW-INDEX may
   exceed the entry count (caller ignores empty rows)."
  (declare (ignore view-w))
  (when (< y (content-height view-h))
    (let ((i (floor (+ x hscroll) +col-w+)))
      (when (and (>= i 0) (< i (length columns)))
        (let* ((col (nth i columns)))
          (when (eq (column-kind col) :dir)
            (let ((row (floor (+ y (column-scroll col)) +row-h+)))
              (values i row))))))))
