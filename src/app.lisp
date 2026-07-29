;;;; app.lisp — the glass app: framebuffer, gesso canvas, input, pump loop, run.
;;;;
;;;; Mirrors loom's glass-shell: a gesso context is repainted whenever a DIRTY flag
;;;; is set by input, packed into a glass framebuffer, and served over VNC.  A
;;;; single mutex guards the column model against the paint loop.  Single-click
;;;; navigation lives in SELECT-IN-COLUMN: set the row, drop columns to the right,
;;;; and append the child directory (or a file-info panel).
(in-package #:warren)

(defstruct app
  fb ctx
  (columns '())            ; left-to-right list of COLUMN
  (root nil)               ; root directory pathname
  (hscroll 0)              ; horizontal scroll offset in px, >= 0
  (vw 0) (vh 0)
  (px -1) (py -1)          ; last pointer position
  (buttons 0)              ; last RFB button mask (low 3 bits)
  (lock (sb-thread:make-mutex :name "warren"))
  (dirty t)
  (running t))

;;; ---------------------------------------------------------------------------
;;; Horizontal auto-scroll
;;; ---------------------------------------------------------------------------
(defun total-columns-width (app) (* (length (app-columns app)) +col-w+))

(defun clamp-hscroll (app)
  (setf (app-hscroll app)
        (max 0 (min (app-hscroll app)
                    (max 0 (- (total-columns-width app) (app-vw app)))))))

(defun ensure-column-visible (app idx)
  "Scroll horizontally so column IDX is fully in view (Finder keeps the active
   columns visible, letting earlier ones scroll off the left)."
  (let* ((left (* idx +col-w+))
         (right (+ left +col-w+))
         (h (app-hscroll app)))
    (cond ((< left h) (setf (app-hscroll app) left))
          ((> right (+ h (app-vw app))) (setf (app-hscroll app) (- right (app-vw app)))))
    (clamp-hscroll app)))

;;; ---------------------------------------------------------------------------
;;; The core interaction: single-click select / truncate / append
;;; ---------------------------------------------------------------------------
(defun select-in-column (app col-index row-index)
  "Select ROW-INDEX in column COL-INDEX: set the selection, discard every column to
   the right, then append the child column — the directory's contents if the row is
   a folder, or a file-info panel if it is a file.  Finally auto-scroll so the new
   rightmost column is visible.  A click on an empty/out-of-range row is ignored."
  (let* ((columns (app-columns app))
         (col (and (< -1 col-index (length columns)) (nth col-index columns))))
    (when (and col (eq (column-kind col) :dir))
      (let ((entries (column-entries col)))
        (when (and (>= row-index 0) (< row-index (length entries)))
          (let ((entry (nth row-index entries)))
            (setf (column-sel col) row-index)
            ;; drop everything to the right of this column
            (let ((kept (subseq columns 0 (1+ col-index))))
              (setf (app-columns app)
                    (nconc kept
                           (list (if (entry-dir-p entry)
                                     (make-dir-column (entry-path entry))
                                     (make-info-column entry))))))
            (ensure-column-visible app (1- (length (app-columns app))))
            (setf (app-dirty app) t)
            t))))))

(defun move-selection (app col-index new-row)
  "Clamp NEW-ROW into COL-INDEX's entries and select it (keyboard navigation)."
  (let* ((columns (app-columns app))
         (col (and (< -1 col-index (length columns)) (nth col-index columns))))
    (when (and col (eq (column-kind col) :dir) (column-entries col))
      (let ((n (length (column-entries col))))
        (select-in-column app col-index (max 0 (min (1- n) new-row)))))))

(defun active-index (app)
  (or (active-column-index (app-columns app)) 0))

;;; ---------------------------------------------------------------------------
;;; Painting: gesso canvas -> glass framebuffer
;;; ---------------------------------------------------------------------------
(defun repaint-canvas (app)
  "Draw the current model into the gesso context (no framebuffer touch)."
  (draw-view (app-ctx app) (app-columns app) (app-hscroll app)
             (app-vw app) (app-vh app)))

(defun blit (app)
  "Pack the gesso RGB8 canvas into the glass 0x00RRGGBB framebuffer."
  (let* ((cv (gesso:context-canvas (app-ctx app)))
         (px (scribe:canvas-pixels cv))
         (w (scribe:canvas-width cv))
         (h (scribe:canvas-height cv))
         (fb (app-fb app))
         (fbpx (glass:fb-pixels fb))
         (fbw (glass:fb-width fb))
         (cols (min w fbw)))
    (glass:with-fb-locked (fb)
      (dotimes (y (min h (glass:fb-height fb)))
        (let ((srow (* y w 3)) (drow (* y fbw)))
          (dotimes (x cols)
            (let ((o (+ srow (* x 3))))
              (setf (aref fbpx (+ drow x))
                    (logior (ash (aref px o) 16)
                            (ash (aref px (+ o 1)) 8)
                            (aref px (+ o 2)))))))))))

(defun paint (app)
  "Repaint the gesso canvas and blit it to the framebuffer.  Caller holds the lock."
  (repaint-canvas app)
  (blit app))

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------
(defun scroll-under-pointer (app x y delta)
  "Vertically scroll (by DELTA rows) the :dir column under device point (X,Y)."
  (let ((i (floor (+ x (app-hscroll app)) +col-w+))
        (columns (app-columns app)))
    (when (and (>= i 0) (< i (length columns)) (< y (content-height (app-vh app))))
      (let ((col (nth i columns)))
        (when (eq (column-kind col) :dir)
          (setf (column-scroll col)
                (max 0 (min (column-max-scroll col (app-vh app))
                            (+ (column-scroll col) (* delta +row-h+ 3)))))
          (setf (app-dirty app) t))))))

(defun on-pointer (app mask x y)
  (sb-thread:with-mutex ((app-lock app))
    (when (logtest mask 8)  (scroll-under-pointer app x y -1))  ; wheel up
    (when (logtest mask 16) (scroll-under-pointer app x y  1))  ; wheel down
    (let ((press-edge (and (logtest mask 1) (not (logtest (app-buttons app) 1)))))
      (setf (app-buttons app) (logand mask 7)
            (app-px app) x (app-py app) y)
      (when press-edge
        (multiple-value-bind (ci ri)
            (hit-test (app-columns app) (app-hscroll app) (app-vw app) (app-vh app) x y)
          (when ci (select-in-column app ci ri)))))))

(defun on-key (app down keysym)
  (when down
    (sb-thread:with-mutex ((app-lock app))
      (let* ((ai (active-index app))
             (columns (app-columns app))
             (acol (and (< ai (length columns)) (nth ai columns))))
        (case keysym
          (#xff54                       ; Down
           (move-selection app ai (1+ (or (and acol (column-sel acol)) -1))))
          (#xff52                       ; Up
           (move-selection app ai (1- (or (and acol (column-sel acol)) 1))))
          (#xff51                       ; Left — focus the parent column
           (when (and (> ai 0) acol)
             (setf (column-sel acol) nil)
             (ensure-column-visible app (1- ai))
             (setf (app-dirty app) t)))
          ((#xff53 #xff0d)              ; Right / Enter — descend into a selected dir
           (let ((e (and acol (column-selected-entry acol))))
             (when (and e (entry-dir-p e) (< (1+ ai) (length columns)))
               (move-selection app (1+ ai) 0)))))))))

;;; ---------------------------------------------------------------------------
;;; Pump loop
;;; ---------------------------------------------------------------------------
(defun stop (app) (setf (app-running app) nil))

(defun pump-loop (app &key max-iterations)
  (loop with i = 0
        while (and (app-running app) (or (null max-iterations) (< i max-iterations)))
        do (sb-thread:with-mutex ((app-lock app))
             (when (app-dirty app)
               (paint app)
               (setf (app-dirty app) nil)))
           (incf i)
           (unless max-iterations (sleep 1/60))))

;;; ---------------------------------------------------------------------------
;;; Construction + public entry points
;;; ---------------------------------------------------------------------------
(defun make-browser (&key root (width 1000) (height 640) background)
  "Build an app with a gesso canvas + a seeded root column, but no server."
  (let* ((root (or root (user-homedir-pathname)))
         (bg (or background +window-bg+))
         (ctx (gesso:make-context width height :background bg))
         (app (make-app :ctx ctx :root root :vw width :vh height)))
    (setf (app-columns app) (list (make-dir-column root)))
    app))

(defun render-to-png (app path)
  "Repaint the current state and dump the gesso canvas to PATH (a PNG).  For
   headless visual testing — no framebuffer or server needed."
  (sb-thread:with-mutex ((app-lock app))
    (repaint-canvas app)
    (gesso:write-png (app-ctx app) path))
  path)

(defun run (&key (port 5900) root (width 1000) (height 640) max-iterations background)
  "Open a Finder-style column file browser rooted at ROOT (default: the home
   directory) and serve it over VNC on PORT.  Runs the pump loop on the calling
   thread; with BACKGROUND it runs in a new thread and RUN returns the app.
   MAX-ITERATIONS bounds the pump loop (headless smoke tests)."
  (let* ((app (make-browser :root root :width width :height height))
         (fb (glass:make-framebuffer width height (apply #'glass:rgb +window-bg+))))
    (setf (app-fb app) fb)
    (sb-thread:with-mutex ((app-lock app)) (paint app))
    (sb-thread:make-thread
     (lambda ()
       (glass:serve fb port
                    :on-key     (lambda (d k)   (on-key app d k))
                    :on-pointer (lambda (m x y) (on-pointer app m x y))
                    :name "warren"))
     :name "warren-rfb")
    (if background
        (progn (sb-thread:make-thread (lambda () (pump-loop app)) :name "warren-pump") app)
        (progn (pump-loop app :max-iterations max-iterations) app))))
