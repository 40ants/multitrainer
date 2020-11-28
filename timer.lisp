(defpackage multiplication/timer
  (:use :cl)
  (:import-from #:local-time)
  (:export #:timer-pane
           #:reset)
  )
(in-package multiplication/timer)


(defclass timer-pane (capi:title-pane)
  ((started-at :initform (local-time:now)
               :accessor started-at)
   (timer-thread :initform nil
                 :accessor timer-thread))
  (:default-initargs :text "00:00"))


(defun format-time (total)
  (let ((seconds (floor (rem total 60)))
        (minutes (floor (rem (/ total 60 )
                               60)))
        (hours (floor (rem (/ total 60 60)
                             60))))
    (format nil "~@[~2,'0d:~]~2,'0d:~2,'0d"
            (when (> hours 0)
              hours)
            minutes
            seconds)))


(defun update-time (pane)
  (let ((since-start (local-time:timestamp-difference
                      (local-time:now)
                      (started-at pane))))
    (setf (capi:title-pane-text pane)
          (format-time since-start)))
  
  (mp:schedule-timer-relative (timer-thread pane)
                              1.0))


(defmethod initialize-instance :after ((pane timer-pane) &rest args)
  (declare (ignorable args))
  (setf (timer-thread pane)
        (mp:make-timer #'update-time pane))

  (mp:schedule-timer-relative (timer-thread pane)
                              1.0))

(defun reset (pane)
  (check-type pane timer-pane)
  (setf (started-at pane)
        (local-time:now)))
