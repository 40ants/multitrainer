(defpackage multiplication/sound
  (:use :cl)
  (:export #:init-sounds
           #:play))
(in-package multiplication/sound)


(defvar *data*
  (list
   :meo (capi:read-sound-file "sounds/meo.mp3")
   :mur (capi:read-sound-file "sounds/mur.wav")
   ))


(defvar *sounds* nil)

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
