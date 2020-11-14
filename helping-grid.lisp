(defpackage multiplication/helping-grid
  (:use :cl)
  (:export #:helping-grid))
(in-package multiplication/helping-grid)


(defclass helping-grid (capi:drawn-pinboard-object)
  ((color :initform :gray
          :initarg :color)
   (size :initform 5
         :initarg :size))
  (:default-initargs :display-callback
   #'draw-helping-grid))


(defun draw-helping-grid (pane self x y width height)
  (with-slots (color size) self
    (loop for iter-x from x below (+ x width) by size
          do (gp:draw-line pane iter-x y iter-x (+ y height)
                           :foreground color))

    (loop for iter-y from y below (+ y height) by size
          do (gp:draw-line pane x iter-y (+ x width) iter-y
                           :foreground color))))
