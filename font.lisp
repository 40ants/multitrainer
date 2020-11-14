(defpackage multiplication/font
  (:use :cl)
  (:export #:make-big-font))
(in-package multiplication/font)


(defparameter *font-size* 100)


(defun make-big-font ()
  (gp:make-font-description :family "courier"
                            :weight :medium
                            :slant :roman
                            :size *font-size*))