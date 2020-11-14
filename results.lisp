s(defpackage multiplication/results
  (:use :cl)
  (:import-from #:multiplication/helping-grid))
(in-package multiplication/results)


(defclass shim (capi:drawn-pinboard-object)
  ((probability :initform (random 1.0)
                :initarg :probability
                :accessor shim-probability))
  (:default-initargs
   :display-callback 'draw-shim
   :visible-min-width 40 :visible-min-height 40))

(defmethod print-object ((shim shim) stream)
  (print-unreadable-object (shim stream :type t)
    (format stream "prob: ~A"
            (shim-probability shim))))

(defun get-color (shim)
  (color:make-rgb 0.0
                  0.0
                  (shim-probability shim)))

(defun draw-shim (pane self x y width height)
  (when (> (shim-probability self) 0.0)
    (gp:draw-rectangle pane x y width height
                       :filled t
                       :foreground (get-color self)))))


(defclass highligher (capi:drawn-pinboard-object)
  ()
  (:default-initargs
   :display-callback 'draw-highligher))


(defun draw-highligher (pane self x y width height)
  (gp:draw-rectangle pane x y width height
                     :foreground :red
                     :thickness 4))


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
                      (let* ((prob
                              (+ 0.2
                                 (* 0.9
                                    (/ (expt (- 81 (* column row))
                                             3)
                                       (expt 81
                                             3)))))
                             (shim (make-instance 'shim
                                                  :probability prob
                                                  :x (+ picture-x
                                                       (* column (+ shim-size shim-gap)))
                                                  :y (+ picture-y
                                                        (* row (+ shim-size shim-gap)))
                                                  :width shim-size
                                                  :height shim-size)))
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
                                      :x v-num-x))))
    (with-slots (column-highlighter row-highlighter)
        pane
      (setf (shim-size pane) shim-size
            (shim-gap pane) shim-gap
            (picture-x pane) picture-x
            (picture-y pane) picture-y)
      (setf column-highlighter (make-instance 'highligher
                                              :x picture-x
                                              :y (- picture-y
                                                    shim-size)
                                              :width shim-size
                                              :height (+ picture-height
                                                         shim-size)))
      (setf row-highlighter (make-instance 'highligher
                                           :x (- picture-x
                                                 shim-size)
                                           :y picture-y
                                           :width (+ picture-width
                                                     shim-size)
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


(defun give-answer (pane answer)
  (flet ((change-prob (left right prob-increment)
           (when (and (<= 1 left 9)
                      (<= 1 right 9))
             (let* ((shim (aref (shims pane)
                                (1- left)
                                (1- right)))
                    (new-value
                   (+ (shim-probability shim)
                      prob-increment)))
             
             (setf (shim-probability shim)
                   (min (max new-value 0.0)
                        1.0))))))
    (with-slots (left right) pane
      (let ((correct (= answer
                        (* left right))))
        (cond
         (correct
          (change-prob left right -100))
         (t
          (change-prob left right 0.5)
          (change-prob (1- left) right 0.25)
          (change-prob (1+ left) right 0.25)
          (change-prob left (1- right) 0.25)
          (change-prob left (1+ right) 0.25)))
        
        (gp:invalidate-rectangle pane)))))

(defvar *current-shim* nil)


(defun get-next-question (pane)
  (loop with value = 0.0
        for shim across (make-array 81 :displaced-to (shims pane))
        collect (incf value
                      (shim-probability shim)) into values
        finally (return
                 (loop with max = (car (last values))
                       with rand = (random max)
                       for idx upfrom 0
                       for value in values
                       when (<= rand value)
                       do (return (let* ((offset idx)
                                         (new-left (1+ (truncate offset 9)))
                                         (new-right (1+ (rem offset 9))))
                                    (setf *current-shim*
                                          (aref (shims pane)
                                                (1- new-left)
                                                (1- new-right)))
                                    (log:info "New question was selected"
                                              new-left new-right *current-shim*
                                              rand
                                              value)
                                    (values
                                     new-left
                                     new-right)))))))


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
                (truncate (shim-gap pane) 2)
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
                (truncate (shim-gap pane) 2)
                (* (+ (shim-size pane)
                      (shim-gap pane))
                   (1- right))))))))
