(defpackage multiplication/results
  (:use :cl)
  (:import-from #:multiplication/helping-grid))
(in-package multiplication/results)


(defclass shim (capi:drawn-pinboard-object)
  ((opened :initform t
           :initarg :opened
           :accessor opened))
  (:default-initargs
   :display-callback 'draw-shim
   :visible-min-width 40 :visible-min-height 40)

(defun draw-shim (pane self x y width height)
  (unless (opened self)
    (gp:draw-rectangle pane x y width height
                       :filled t
                       :foreground :black)))


(defclass highligher (capi:drawn-pinboard-object)
  ()
  (:default-initargs
   :display-callback 'draw-highligher))


(defun draw-highligher (pane self x y width height)
  (gp:draw-rectangle pane x y width height
                     :foreground :red
                     :thickness 2))


(defclass picture (capi:drawn-pinboard-object)
  ((image :initform nil)
   (path :initform nil
         :initarg :path))
  (:default-initargs
   :width 600
   :height 600
   :display-callback 'draw-picture))


(defun draw-picture (pane picture x y width height)
  (declare (ignorable pinboard x y width height))

  (setf *debug* (list pane picture x y width height))

  (with-slots (image path) picture
      ;;;     (when image
      ;;;       (gp:free-image picture image))
      (unless image
        (let* ((external-image
                (gp:read-external-image path))
               (internal-image (gp:convert-external-image pane external-image)))
          
          (setf image
                (gp:make-scaled-sub-image pane internal-image width height))
          (gp:invalidate-rectangle picture)
          ))

    (gp:draw-image pane
                   image
                   x y)))
  

(defclass results (capi:pinboard-layout)
  ((picture :accessor results-picture
          :initform nil)
   (shims :initform (make-array '(9 9) :initial-element nil)
          :reader shims)
   (shim-size :initform nil
              :accessor shim-size)
   (shim-gap :initform nil
             :accessor shim-gap)
   (picture-x :initform nil
              :accessor picture-x)
   (picture-y :initform nil
              :accessor picture-y)
   (column-highlighter :initform nil)
   (row-highlighter :initform nil)
   (left :initform nil)
   (right :initform nil))
  (:default-initargs
   :visible-min-width 700
   :visible-min-height 700
   :visible-max-width 700
   :visible-max-height 700))


(defun choose-random-picture ()
  (if (< (random 100) 50)
      "images/pexels-pixabay-45201.jpg"
    "images/dog-5723334_1920.jpg"))


(defmethod initialize-instance :after ((pane results) &key &allow-other-keys)
  (let* ((picture-width 600)
         (picture-height 600)
         (picture-x 70)
         (picture-y 70)
         (picture  (make-instance 'picture
                                  :path (choose-random-picture)
                                  :x picture-x
                                  :y picture-y
                                  :width picture-width
                                  :height picture-height))
         (shim-gap 2)
         (shim-size (/ (- picture-width (* shim-gap 8))
                          9))
         (shims (uiop/utility:while-collecting (collect)
                  (dotimes (column 9)
                    (dotimes (row 9)
                      (let ((shim (make-instance 'shim
                                                 :x (+ picture-x
                                                       (* column (+ shim-size shim-gap)))
                                                 :y (+ picture-y
                                                       (* row (+ shim-size shim-gap)))
                                                 :width shim-size
                                                 :height shim-size
                                                 :opened
                                                 (when (< (random 100)
                                                                  30)
                                                           t)
                                                 )))
                        (setf (aref (shims pane) column row)
                              shim)
                        (collect shim))))))
         (h-num-x (+ picture-x 15))
         (h-num-y (+ (- picture-y shim-size)
                     5))
         (horizontal-numbers
          (loop for number below 9
                collect(make-instance 'capi:title-pane
                                      :text (format nil "~A"
                                                    (1+ number))
                                      :font (multiplication/font:make-small-font)
                                      :x (+ h-num-x (* number (+ shim-size
                                                                 shim-gap)))
                                      :y h-num-y)))
         (v-num-x (- picture-x 35))
         (v-num-y picture-y)
         (vertical-numbers
          (loop for number below 9
                collect(make-instance 'capi:title-pane
                                      :text (format nil "~A"
                                                    (1+ number))
                                      :font (multiplication/font:make-small-font)
                                      :y (+ v-num-y (* number (+ shim-size
                                                                 shim-gap)))
                                      :x v-num-x)))
         
         )
    (with-slots (column-highlighter row-highlighter)
        pane
      (setf (shim-size pane) shim-size
            (shim-gap pane) shim-gap
            (picture-x pane) picture-x
            (picture-y pane) picture-y)
      (setf column-highlighter (make-instance 'highligher
                                              :x picture-x :y picture-y
                                              :width shim-size
                                              :height picture-height))
      (setf row-highlighter (make-instance 'highligher
                                           :x picture-x :y picture-y
                                           :width picture-width
                                           :height shim-size))
      (setf (capi:layout-description pane)
            (append
             ;;;            (list (make-instance 'multiplication/helping-grid:helping-grid
             ;;;                                 :width 700
             ;;;                                 :height 700))
             (list picture)
             horizontal-numbers
             vertical-numbers
             shims
             (list column-highlighter
                   row-highlighter))))))


(defun update-results (pane left right answer)
  (let ((shim (aref (shims pane)
                    (1- left)
                    (1- right))))
    (setf (opened shim)
          (= answer
             (* left right)))
    (gp:invalidate-rectangle pane)))


(defun set-question (pane new-left new-right)
  (with-slots (left right column-highlighter row-highlighter)
      pane
    (setf left new-left
          right new-right)
    (multiple-value-bind (x y)
        (capi:static-layout-child-position column-highlighter)
      (declare (ignorable x))
      (setf (capi:static-layout-child-position column-highlighter)
            (values
             (+ (picture-x pane)
                (* (+ (shim-size pane)
                      (shim-gap pane))
                   (1- left)))
             y)))
    (multiple-value-bind (x y)
        (capi:static-layout-child-position row-highlighter)
      (declare (ignorable y))
      (setf (capi:static-layout-child-position row-highlighter)
            (values
             x
             (+ (picture-y pane)
                (* (+ (shim-size pane)
                      (shim-gap pane))
                   (1- right))))))))