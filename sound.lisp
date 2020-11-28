(defpackage multiplication/sound
  (:use :cl)
  (:export #:init-sounds
           #:play
           #:play-random))
(in-package multiplication/sound)


(defparameter *data*
  (list
   :meow1 (capi:read-sound-file "sounds/meow1.wav")
   :meow2 (capi:read-sound-file "sounds/meow2.wav")
   :purring (capi:read-sound-file "sounds/purring.wav")
   :applause (capi:read-sound-file "sounds/applause.wav")
   ))


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
        
  
  