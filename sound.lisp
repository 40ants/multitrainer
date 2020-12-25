(defpackage multiplication/sound
  (:use :cl)
  (:export #:init-sounds
           #:play
           #:play-random))
(in-package multiplication/sound)


(defparameter *data*
  (flet ((f (name &optional (format "wav"))
           (capi:read-sound-file
            (asdf/system:system-relative-pathname :multiplication
                                                  (format nil "sounds/~A.~A"
                                                          name
                                                          format)))))
    (list
     :meow1 (f "meow1")
     :meow2 (f "meow2")
     :purring1 (f "mrrr1" "m4a")
     :purring2 (f "mrrr2" "m4a")
     :purring3 (f "mrrr3" "m4a")
     :applause (f "applause"))))


(defparameter *sounds* nil)

(defun init-sounds ()
  (unless *sounds*
    (setf *sounds*
          (loop for (name data) on *data* by #'cddr
                appending (list name
                                (capi:load-sound data))))))

(defun play (sound-name)
  (let ((sound (getf *sounds* sound-name)))
    (when sound
      (capi:stop-sound sound)
      (capi:play-sound sound))))


(defun play-random (sound-name-prefix)
  (when (keywordp sound-name-prefix)
    (setf sound-name-prefix
          (symbol-name sound-name-prefix)))

  (loop for name in *sounds* by #'cddr
        for name-prefix = (subseq (symbol-name name)
                                  0
                                  (min (length sound-name-prefix)
                                       (length (symbol-name name))))
        when (string-equal name-prefix
                           sound-name-prefix)
        collect name into sounds
        finally (when sounds
                  (play (elt sounds
                             (random (length sounds)))))))
        
  
  