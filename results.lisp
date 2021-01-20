(defpackage multiplication/results
  (:use :cl)
  (:import-from #:log4cl)
  (:import-from #:multiplication/helping-grid)
  (:import-from #:multiplication/font)
  (:import-from #:multiplication/sound
   #:play
   #:play-random)
  (:export #:give-answer
   #:has-more-questions
   #:reset-shims))
(in-package multiplication/results)


(defvar *images*
  (loop for pathname in (directory (asdf/system:system-relative-pathname
                                    :multiplication
                                    "images/"))
        collect (gp:read-external-image pathname)))


(defclass shim (capi:drawn-pinboard-object)
  ((probability :initform (random 1.0)
                :initarg :probability
                :accessor shim-probability))
  (:default-initargs
   :display-callback 'draw-shim
   :visible-min-width 40
   :visible-min-height 40))

(defmethod print-object ((shim shim) stream)
  (print-unreadable-object (shim stream :type t)
    (format stream "prob: ~A"
            (shim-probability shim))))

(defun get-color (shim)
  (let ((prob (shim-probability shim)))
    (color:make-rgb 0.0
                    (* prob 0.6)
                    prob)))

(defun draw-shim (pane self x y width height)
  (when (> (shim-probability self) 0.0)
    (gp:draw-rectangle pane x y width height
                       :filled t
                       :foreground (get-color self))))


(defclass highligher (capi:drawn-pinboard-object)
  ()
  (:default-initargs
   :display-callback 'draw-highligher))


(defun draw-highligher (pane self x y width height)
  (declare (ignorable self))
  (gp:draw-rectangle pane x y width height
                     :foreground :red
                     :thickness 4))


(defclass picture (capi:drawn-pinboard-object)
  ((image :initform nil)
   (external-image :initform nil
                   :initarg :external-image))
  (:default-initargs
;;   :width 600
  ;; :height 600
   :display-callback 'draw-picture))


(defun draw-picture (pane picture x y width height)
  (declare (ignorable pane))
  (with-slots (image external-image) picture
    ;;;     (when image
    ;;;       (gp:free-image picture image))
    
    (unless image
      (let ((internal-image (gp:convert-external-image pane external-image)))
        
        (setf image
              (gp:make-scaled-sub-image pane internal-image width height))
        (gp:invalidate-rectangle picture)))
    
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
   (horizontal-numbers :initform nil
                       :accessor horizontal-numbers)
   (vertical-numbers :initform nil
                     :accessor vertical-numbers)
   (picture-x :initform nil
              :accessor picture-x)
   (picture-y :initform nil
              :accessor picture-y)
   (column-highlighter :initform nil)
   (row-highlighter :initform nil)
   (left :initform nil)
   (right :initform nil))
  (:default-initargs
   :visible-min-width 500
   :visible-min-height 500
   :visible-max-width 700
   :visible-max-height 700
   :resize-callback 'resize-results))


(defun choose-random-picture ()
  (elt *images*
        (random (length *images*))))

(defun reset-shims (pane)
  (dotimes (column 9)
    (dotimes (row 9)
      (let* (
;;;              (prob
;;;               (if (and (= column 0)
;;;                        (= row 0))
;;;                   0.5
;;;                 0.0))
            (prob
             (+ 0.2
                (* 0.9
                   (/ (expt (- 81 (* column row))
                            3)
                      (expt 81
                            3))))))
        (setf (shim-probability (aref (shims pane) column row))
              prob)))))


(defmethod initialize-instance :after ((pane results) &key &allow-other-keys)
  (let* ((picture-width 600)
         (picture-height 600)
         (picture-x 70)
         (picture-y 70)
         (picture  (make-instance 'picture
                                  :external-image (choose-random-picture)
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
                                           :height shim-size)))
                        (setf (aref (shims pane) column row)
                              shim)
                        (collect shim))))))
  
         (h-num-x (+ picture-x 15))
         (h-num-y (+ (- picture-y shim-size)
                     5))
         (horizontal-numbers
          (loop for number below 9
                collect (make-instance 'capi:title-pane
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
                collect (make-instance 'capi:title-pane
                                       :text (format nil "~A"
                                                     (1+ number))
                                       :font (multiplication/font:make-small-font)
                                       :y (+ v-num-y (* number (+ shim-size
                                                                  shim-gap)))
                                       :x v-num-x)))
         (helping-grid
          (make-instance 'multiplication/helping-grid:helping-grid
                         :width 700
                         :height 700)))
    (declare (ignorable helping-grid))
    
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
      
      (setf
       (results-picture pane) picture
       (horizontal-numbers pane) horizontal-numbers
       (vertical-numbers pane) vertical-numbers
       (capi:layout-description pane)
            (append
;;             (list helping-grid)
             (list picture)
             horizontal-numbers
             vertical-numbers
             shims
             (list column-highlighter
                   row-highlighter))))))


(defun resize-results (pane x y width height)
  (declare (ignorable x y))
  (let* ((size (min width height))
         (picture-x (+ 70 (/ (- width size)
                             2)))
         (picture-y 70)
         (right-padding 10)
         (picture-size (- size (+ picture-y right-padding)))
         (picture (results-picture pane))
         (shim-gap 2)
         (shim-size (/ (- picture-size (* shim-gap 8))
                       9))
         (font-scale (/ size 700))
         (numbers-font (multiplication/font:make-small-font font-scale))
         (h-num-x (+ picture-x 15))
         (h-num-y (+ (- picture-y shim-size)
                     5))
         (v-num-x (- picture-x 35))
         (v-num-y picture-y))


   (setf
    (x-position picture) picture-x
    (y-position picture) picture-y
    (picture-x pane) picture-x
    (picture-y pane) picture-y
    (capi:static-layout-child-size picture)
    (values picture-size
            picture-size)
    ;; Reset the cat's image to make a new one
    ;; which will fit desired square
    (slot-value picture 'image)
    nil
    (shim-size pane) shim-size
    (shim-gap pane) shim-gap)

   (with-slots (column-highlighter row-highlighter)
        pane
     (setf
      (x-position column-highlighter) (get-column-highlighter-x pane)
      (y-position column-highlighter) (- picture-y
                                         shim-size)
      (capi:static-layout-child-size column-highlighter)
           (values shim-size
                   (+ picture-size
                      shim-size)))
     (setf
      (x-position row-highlighter) (- picture-x
                                      shim-size)
      (y-position row-highlighter) (get-row-highlighter-y pane)
      (capi:static-layout-child-size row-highlighter)
           (values (+ picture-size
                      shim-size)
                   shim-size)))

   (loop for num in (vertical-numbers pane)
         for number below 9
         do (setf (capi:simple-pane-font num) numbers-font
                  (x-position num) v-num-x
                  (y-position num) (+ v-num-y (* number (+ shim-size
                                                           shim-gap)))))

   (loop for num in (horizontal-numbers pane)
         for number below 9
         do (setf (capi:simple-pane-font num) numbers-font
                  (x-position num) (+ h-num-x (* number (+ shim-size
                                                           shim-gap)))
                  (y-position num) h-num-y))
   
   (dotimes (column 9)
     (dotimes (row 9)
       (let ((shim (aref (shims pane) column row)))
         (setf (x-position shim)
               (+ picture-x
                  (* column (+ shim-size shim-gap))))
         (setf (y-position shim)
               (+ picture-y
                  (* row (+ shim-size shim-gap))))
         (setf (capi:static-layout-child-size shim)
               (values shim-size shim-size)))))))


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
          (change-prob left right -100)
          (play-random :purring))
         (t
          (change-prob left right 0.5)
          (change-prob (1- left) right 0.25)
          (change-prob (1+ left) right 0.25)
          (change-prob left (1- right) 0.25)
          (change-prob left (1+ right) 0.25)
          (play-random :meow)))
        
        (gp:invalidate-rectangle pane)))))


(defun has-more-questions (pane)
  (let ((shims (shims pane)))
    (dotimes (j 9)
      (dotimes (i 9)
        (when (> (shim-probability
                  (aref shims i j))
                 0.0)
          (return-from has-more-questions t))))))


(defvar *current-shim* nil)


(defun get-next-question (pane)
  (loop with value = 0.0
        for shim across (make-array 81 :displaced-to (shims pane))
        collect (incf value
                      (* (shim-probability shim)
                         (shim-probability shim))) into values
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

(defparameter *trans-lock* (mp:make-lock))
(defparameter *trans* nil)
(defvar *timer* (mp:make-timer 'process-transitions))
(defparameter *timer-interval* 0.001)
(defparameter *default-duration* 0.2)


(defun x-position (obj)
  (nth-value 0
             (capi:static-layout-child-position obj)))

(defun (setf x-position) (new-x obj)
  (setf (capi:static-layout-child-position obj)
        (values
         new-x
         (y-position obj))))

(defun y-position (obj)
  (nth-value 1
             (capi:static-layout-child-position obj)))


(defun (setf y-position) (new-y obj)
  (setf (capi:static-layout-child-position obj)
        (values
         (x-position obj)
         new-y)))


(defun make-transition (pane from-value to-value set-func &optional (duration *default-duration*))
  (let* ((started-at (get-internal-real-time))
         (until (+ started-at
                   (* duration
                      internal-time-units-per-second))))
    (lambda ()
      (ignore-errors
        (let* ((now (get-internal-real-time))
               (new-value (+ from-value
                             (* (- to-value from-value)
                                (/ (- now started-at)
                                 (- until started-at))))))
          (cond
           ((<= now until)
            (capi:apply-in-pane-process pane
                                        set-func
                                        new-value)
            nil)
           (t (funcall set-func to-value)
              ;; Signal to remove transition
              ;; from the queue
              t)))))))


(defun change-x (pane obj new-x &optional (duration *default-duration*))
  (make-transition pane
                   (x-position obj)
                   new-x
                   (lambda (value)
                     (setf (x-position obj)
                           value)
                     (gp:invalidate-rectangle
                      (capi:element-parent obj)))
                   duration))


(defun change-y (pane obj new-y &optional (duration *default-duration*))
  (make-transition pane
                   (y-position obj)
                   new-y
                   (lambda (value)
                     (setf (y-position obj)
                           value)
                     (gp:invalidate-rectangle
                      (capi:element-parent obj)))
                   duration))

(defun start-transition (transition)
  (mp:with-lock (*trans-lock*)
    (push transition *trans*)
    (start-timer)))


(defun process-transitions ()
  (mp:with-lock (*trans-lock*)
    (loop with to-remove = nil
        for transition in *trans*
        for remove-p = (funcall transition)
        if remove-p
        do (push transition to-remove)
        finally (setf *trans*
                      (remove-if (lambda (tr)
                                   (member tr to-remove))
                                 *trans*)))
    (mp:schedule-timer-relative *timer*
                                *timer-interval*)))

(defun start-timer ()
  (mp:schedule-timer-relative *timer*
                              *timer-interval*))


(defun set-question (pane new-left new-right)
  (with-slots (left right column-highlighter row-highlighter)
      pane
    (setf left new-left
          right new-right)

    (start-transition
     (change-x
      pane
      column-highlighter
      (get-column-highlighter-x pane)
      0.3))

    (start-transition
     (change-y
      pane
      row-highlighter
      (get-row-highlighter-y pane)
      0.3))

    ))


(defun get-row-highlighter-y (pane)
  (+ (picture-y pane)
     (truncate (shim-gap pane) 2)
     (* (+ (shim-size pane)
           (shim-gap pane))
        (1- (slot-value pane 'right)))))


(defun get-column-highlighter-x (pane)
  (+ (picture-x pane)
     (truncate (shim-gap pane) 2)
     (* (+ (shim-size pane)
           (shim-gap pane))
        (1- (slot-value pane 'left)))))