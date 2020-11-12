(defpackage multiplication/core
  (:use :cl))
(in-package multiplication/core)


(defparameter *font-size* 100)


(defclass results (capi:output-pane)
  ((image :accessor results-image
          :initform nil)
   (cells :initform (make-array '(9 9) :initial-element nil)
          :reader results-cells))
  (:default-initargs
   :display-callback #'display-results
   :visible-min-width 700
   :visible-min-height 700
   :visible-max-width 700
   :visible-max-height 700
   ;:background :red
   ))


(defparameter *display-called* 0)


(defun draw-results-grid (results)
  (capi:with-geometry results
    (gp:draw-rectangle results 0 0
                       (/ capi:%width% 2)
                       (/ capi:%height% 2)
                       :filled :red)))


(defun display-results (results x y width height)
  (incf *display-called*)
  (let ((image (results-image results)))
    (when image
      (when (gp:rectangle-overlap x y (+ x width) (+ y height)
                                  0 0 (gp:image-width image) (gp:image-height image))
        #|
       (gp:draw-image results
                       image
                       0 0)
       |#
        )
      (draw-results-grid results)

      (gp:invalidate-rectangle results)
      
;;;       (capi:set-horizontal-scroll-parameters
;;;        results
;;;        :min-range 0
;;;        :max-range (if image
;;;                       (gp:image-width image)
;;;                     0))
;;;       (capi:set-vertical-scroll-parameters
;;;        results
;;;        :min-range 0
;;;        :max-range (if image
;;;                       (gp:image-height image)
;;;                     0))
;;;       
      )))


(defun update-results (results left right answer)
  (unless (results-image results)
    (let* ((external-image
            (gp:read-external-image "images/pexels-pixabay-45201.jpg"))
           (image (gp:convert-external-image results external-image))
           (scaled
            (gp:make-scaled-sub-image results image 500 500)
            )
           )
      (setf (results-image results)
            scaled)
      (gp:invalidate-rectangle results)))
  
;;;   (let* ((color (if (= (* left right)
;;;                        answer)
;;;                     :green
;;;                   :red)))
;;;     (format t "Setting color ~A for ~A * ~A = ~A~%"
;;;             color left right answer)
;;;     (capi:apply-in-pane-process
;;;      results
;;;      #'(setf capi:simple-pane-background)
;;;      color
;;;      results)
;;;     (show-next-question (find-app results)))

  )


(defun make-font ()
  (gp:make-font-description :family "courier" :weight :medium :slant :roman :size *font-size*))


(defun on-enter (answer app)
  (let ((answer (parse-integer answer :junk-allowed t)))
    (when answer
      (update-results
       (results-pane app)
       *left*
       *right*
       answer))))


(capi:define-interface multiplication ()
  ()
  (:panes
   (question
    capi:title-pane
    :reader question
    :font (make-font)
    )
   (answer
    capi:text-input-pane
    :font (make-font)
    :max-characters 2
    :visible-min-width 130
    :callback 'on-enter
    :reader answer
    )
   (results
    results
    :reader results-pane
    )
   (question-mark
    capi:title-pane
    :text "?"
    :font (make-font)
    ))
  (:layouts
   (main-layout
    capi:column-layout
    '(results
      question-row)
    )
   (question-row
    capi:row-layout
    '(question
      answer
      question-mark)
;;    :visible-min-height 400

    ))
  (:default-initargs
   :title "Multiplication Trainer"
   ))


(defun find-app (element)
  "Returns the main app element"
  (if (typep element 'multiplication)
      element
    (find-app (capi:element-parent element))))


(defvar *left*)

(defvar *right*)


(defun show-next-question (app)
  (setf *left* (1+ (random 9))
        *right* (1+ (random 9)))
 
  (capi:apply-in-pane-process
     (question app)
     #'(setf capi:title-pane-text)
     (format nil "~A * ~A = "
             *left*
             *right*)
     (question app))

  (capi:apply-in-pane-process
   (answer app)
   #'(setf capi:text-input-pane-text)
   ""
   (answer app)))


(defvar *app* nil)

(defvar *update-thread* nil)


(defun redraw-results-for-debug ()
  "We need this for debug only."
  (loop
   do (when *app*
        (gp:invalidate-rectangle (results-pane *app*)))
      (sleep 0.5))
  )


(defun start ()
  (let ((app (make-instance 'multiplication)))
    (setf *app* app)
    (capi:display app)
    (update-results (results-pane app) 1 1 1)
    (show-next-question app))

  (unless *update-thread*
    (bt:make-thread #'redraw-results-for-debug
                    :name "App Results Debug Updater")))