(defpackage multiplication/tips
  (:use #:cl)
  (:import-from #:multiplication/i18n
   #:i18n)
  (:export #:make-tip))
(in-package multiplication/tips)


(defclass op ()
  ((left :initarg :left)
   (right :initarg :right)))


(defclass trivial (op)
  ())


(defclass plus (op)
  ())


(defclass mul (op)
  ())



(defmethod print-object ((op trivial) stream)
  (declare (ignorable op))
  (format stream (i18n "easy-tip")))


(defmethod print-object ((op plus) stream)
  (with-slots (left right) op
    (format stream "~A + ~A"
            left right)))


(defmethod print-object ((op mul) stream)
  (with-slots (left right) op
    (format stream "~A*~A"
            left right)))


(defun simplify (left right)
  (cond
   ((> left right)
    (simplify right left))
   ((= left 1)
    (make-instance 'trivial
                   :left left
                   :right right))
   ((oddp right)
    (let ((new-left
           (simplify left (1- right))))
      ;; TODO: тут надо провеять на тривиалльлность new-left
      (make-instance 'plus
                     :left new-left
                     :right left)))

;;;    ((oddp left)
;;;     (let ((new-right
;;;            (simplify (1- left) right)))
;;;       ;; TODO: тут надо провеять на тривиалльлность new-left
;;;       (make-instance 'plus
;;;                      :left new-right
;;;                      :right left)))

   ;; If we are here, then both numbers are even.
   ;; If some of them is too big, then we can divide it in two

   ((> right 4)
    (let ((new-op (make-instance 'mul
                                 :left left
                                 :right (/ right 2))))
      (make-instance 'plus
                   :left new-op
                   :right new-op)))
   
   (t (make-instance 'mul
                     :left left
                     :right right))))


(defun make-tip (left right)
  (let ((op (simplify left right)))
    (typecase op
      (trivial
       (format nil "~A"
               op))
      (t (format nil "~A = ?"
                 op)))))

;; Кандидаты
;; 3 * 4 -> 3 * 3 + 3
;; 2 * 2 -> trivial!
;; 4 * 4 -> trivial!
;; 8 * 9 -> 8 * 8 + 8 а не 8 * 4 + 8 * 4 + 8.
;; и то же самое для 6 * 7