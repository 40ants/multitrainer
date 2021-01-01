(defpackage multiplication/font
  (:use :cl)
  (:export #:make-big-font
           #:make-small-font))
(in-package multiplication/font)


(defparameter *font-size* 100)
(defparameter *small-font-size* 50)


(defun make-big-font ()
  (gp:make-font-description :family "courier"
                            :weight :medium
                            :slant :roman
                            :size *font-size*))


(defun make-small-font (&optional (scale 1.0))
  (gp:make-font-description :family "courier"
                            :weight :medium
                            :slant :roman
                            :size (* *small-font-size*
                                     scale)))