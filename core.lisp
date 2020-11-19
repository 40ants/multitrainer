(defpackage multiplication/core
  (:use :cl)
  (:import-from #:multiplication/sound
                #:init-sounds)
  (:import-from #:multiplication/results))
(in-package multiplication/core)



(defvar *debug*)

(defparameter *num* 0)

(defun on-enter (answer app)
  (let ((answer (parse-integer answer :junk-allowed t)))
    (when answer
      (multiplication/results::give-answer
       (results-pane app)
       answer)
      (show-next-question app))))


(capi:define-interface multiplication ()
  ()
  (:panes
   (question
    capi:title-pane
    :reader question
    :font (multiplication/font:make-big-font)
    )
   (answer
    capi:text-input-pane
    :font (multiplication/font:make-big-font)
    :max-characters 2
    :visible-min-width 130
    :callback 'on-enter
    :reader answer
    )
   (results
    multiplication/results::results
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


;;; (defun find-app (element)
;;;   "Returns the main app element"
;;;   (if (typep element 'multiplication)
;;;       element
;;;     (find-app (capi:element-parent element))))


(defvar *left*)

(defvar *right*)


(defun show-next-question (app)
  (multiple-value-bind (new-left new-right)
      (multiplication/results::get-next-question (results-pane app))
    
    (multiplication/results::set-question
     (results-pane app)
     new-left
     new-right)
    
    (capi:apply-in-pane-process
     (question app)
     #'(setf capi:title-pane-text)
     (format nil "~A * ~A = "
             new-left
             new-right)
     (question app))

    (capi:apply-in-pane-process
     (answer app)
     #'(setf capi:text-input-pane-text)
     ""
     (answer app))))


(defvar *app* nil)

(defvar *update-thread* nil)


;;; (defun redraw-results-for-debug ()
;;;   "We need this for debug only."
;;;   (loop
;;;    do (when *app*
;;;         (gp:invalidate-rectangle (results-pane *app*)))
;;;       (sleep 0.5))
;;;   )


(defun start ()
  (let ((app (make-instance 'multiplication)))
    (setf *app* app)
    (capi:display app)
    (init-sounds)
    ;(update-results (results-pane app) 1 1 1)
    (show-next-question app))

;;;   (unless *update-thread*
;;;     (bt:make-thread #'redraw-results-for-debug
;;;                     :name "App Results Debug Updater"))
  )