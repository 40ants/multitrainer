(defpackage multiplication/results
  (:use :cl))
(in-package multiplication/results)


(defclass shim (capi:drawn-pinboard-object)
  ((opened :initform t
           :initarg :opened
           :accessor opened))
  (:default-initargs
   :display-callback 'draw-shim
   :visible-min-width 40 :visible-min-height 40))


(defun draw-shim (pane self x y width height)
  (unless (opened self)
    (gp:draw-rectangle pane x y width height
                       :filled t
                       :foreground :black)))


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
   (shims :initform (make-array '(10 10) :initial-element nil)
          :reader shims))
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
         (picture-x 90)
         (picture-y 90)
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
                                                 :opened nil
;;;                                                  (when (< (random 100)
;;;                                                                   30)
;;;                                                            t)
                                                 )))
                        (setf (aref (shims pane) column row)
                              shim)
                        (collect shim))))))
         (h-num-x (+ picture-x 0))
         (h-num-y (- picture-y shim-size 40))
         (horizontal-numbers
          (loop for number below 9
                collect(make-instance 'capi:title-pane
                                      :text (format nil "~A"
                                                    (1+ number))
                                      :font (multiplication/font:make-big-font)
                                      :x (+ h-num-x (* number (+ shim-size
                                                                 shim-gap)))
                                      :y h-num-y)))
         (v-num-x (- picture-x shim-size))
         (v-num-y (- picture-y 33))
         (vertical-numbers
          (loop for number below 9
                collect(make-instance 'capi:title-pane
                                      :text (format nil "~A"
                                                    (1+ number))
                                      :font (multiplication/font:make-big-font)
                                      :y (+ v-num-y (* number (+ shim-size
                                                                 shim-gap)))
                                      :x v-num-x))))
    (setf (capi:layout-description pane)
          (append (list picture)
                  horizontal-numbers
                  vertical-numbers
                  shims)
          )
    )
  )


;;; (defparameter *display-called* 0)


;;; (defun draw-results-grid (results)
;;;   (capi:with-geometry results
;;;     (gp:draw-rectangle results 0 0
;;;                        (/ capi:%width% 2)
;;;                        (/ capi:%height% 2)
;;;                        :filled :red)
;;;     ))


(defun update-results (pane left right answer)
  (let ((shim (aref (shims pane)
                    (1- left)
                    (1- right))))
    (setf (opened shim)
          (= answer
             (* left right)))
    (gp:invalidate-rectangle pane)))


;;; (defun update-results (results left right answer)
;;;   (unless (results-image results)
;;;     (let* ((external-image
;;;             (gp:read-external-image "images/pexels-pixabay-45201.jpg"))
;;;            (image (gp:convert-external-image results external-image))
;;;            (scaled
;;;             (gp:make-scaled-sub-image results image 500 500)
;;;             )
;;;            )
;;;       (setf (results-image results)
;;;             scaled)
;;;       (gp:invalidate-rectangle results)))
;;;   
;;; ;;;   (let* ((color (if (= (* left right)
;;; ;;;                        answer)
;;; ;;;                     :green
;;; ;;;                   :red)))
;;; ;;;     (format t "Setting color ~A for ~A * ~A = ~A~%"
;;; ;;;             color left right answer)
;;; ;;;     (capi:apply-in-pane-process
;;; ;;;      results
;;; ;;;      #'(setf capi:simple-pane-background)
;;; ;;;      color
;;; ;;;      results)
;;; ;;;     (show-next-question (find-app results)))

;;;   )
