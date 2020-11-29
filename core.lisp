(defpackage multiplication/core
  (:use :cl)
  (:import-from #:multiplication/sound
                #:init-sounds)
  (:import-from #:multiplication/results)
  (:import-from #:multiplication/timer)

  )
(in-package multiplication/core)



(defvar *debug*)

(defparameter *num* 0)


(defun on-end (app)
  (multiplication/sound:play :applause)

  (multiplication/timer:stop (clock app))
  
  (let ((new-screen (final-layout app))
        (main (main-layout app)))

    (capi:apply-in-pane-process
     main
     #'(setf capi:switchable-layout-visible-child)
     new-screen main)))


(defun on-enter (answer app)
  (let ((answer (parse-integer answer :junk-allowed t))
        (results (results-pane app)))
    (when answer
      (multiplication/results:give-answer
       results
       answer)
      (if (multiplication/results:has-more-questions results)
          (show-next-question app)
        (on-end app)))))


(capi:define-interface multiplication ()
  ()
  (:panes
   (clock
    multiplication/timer:timer-pane
    :font (multiplication/font:make-big-font)
    :reader clock)
   (question
    capi:title-pane
    :reader question
    :font (multiplication/font:make-big-font))
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
    :font (multiplication/font:make-big-font)
    )
   (start-game-button
    capi:button
;;    :visible-min-height 200
    :visible-max-height nil
    :text "Start"
;    :font (multiplication/font:make-big-font)
    :callback (lambda (_ app)
                (declare (ignore _))
                (start-game app))
    ))
  (:layouts
   (start-layout
    capi:row-layout
    '(nil start-game-button nil)
    :reader start-layout
    )
   (final-layout
    capi:grid-layout
    '(nil nil nil
      nil clock nil
      nil start-game-button nil
      nil nil nil)
    :columns 3
    :x-adjust :center
    :reader final-layout
    )
   (clock-row
    capi:row-layout
    '(nil clock nil))
   (game-layout
    capi:column-layout
    '(clock-row
      results
      question-row)
    :reader game-layout
    )
   (main-layout
    capi:switchable-layout
    '(start-layout
      game-layout
      final-layout)
    :reader main-layout
    :visible-child 'start-layout
    :combine-child-constraints t
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
   :layout 'main-layout
   ))


;;; (defun find-app (element)
;;;   "Returns the main app element"
;;;   (if (typep element 'multiplication)
;;;       element
;;;     (find-app (capi:element-parent element))))


;;(defvar *left*)

;;(defvar *right*)


(defun start-game (app)
  (let* ((new-screen (game-layout app))
         (main (main-layout app)))
    
    (multiplication/timer:reset
     (clock app))

    (multiplication/results:reset-shims
      (results-pane app))
    (show-next-question app)
    
    (capi:apply-in-pane-process
     main
     #'(setf capi:switchable-layout-visible-child)
     new-screen main)))


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


(defun test1 ()
  (capi:contain 
   (make-instance 
    'capi:grid-layout 
    :description
    (list
     nil nil nil
     nil
     (make-instance 'capi:button :text "Push me!"
;                    :visible-max-width t
                    )

     nil
     nil nil nil)
    :columns 3
    :x-adjust :center
    :y-adjust :center
    )
   :height 150 :width 150 :title "Resize Me")
  )


(defun test ()
  (capi:contain 
   (make-instance 
    'capi:row-layout 
    :description
    (list
     nil
     (make-instance 'capi:button :text "Push me!"
                    :visible-max-height nil
                    )
     nil
)
    )
   :height 150 :width 150 :title "Resize Me")
  )