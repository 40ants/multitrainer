(defpackage multiplication/i18n
  (:use #:cl)
  (:import-from #:cl-locale)
  (:import-from #:global-vars)
  (:import-from #:str)
  (:export #:i18n
           #:init))
(in-package multiplication/i18n)


(global-vars:define-global-var *initialized* nil)


#+cocoa
(defun default-language ()
  (let ((lang (objc:with-autorelease-pool ()
                (objc:invoke-into
                 'string
                 (objc:invoke
                  (objc:invoke "NSUserDefaults" "standardUserDefaults")
                  "objectForKey:" "AppleLanguages")
                 "objectAtIndex:" 0))))
    ;; 2019-04-29
    ;; lang can be either on the short form "sv" or the long form "sv-SE".
    ;; It may have changed in later versions of macOS.
    ;; Anyway, make sure that this function returns on the short form "sv".
    (if (and (= (length lang) 5)
             (eq (char lang 2) #\-))
        (subseq lang 0 2)
      lang)
    )
  )


#+win32
(fli:define-foreign-function (get-system-default-language-id "GetSystemDefaultLangID")
    () :result-type :int)


#+win32
(defun default-language ()
  ;; Map language id to code - a complete list is available in the Windows documentation of GetSystemDefaultLangID
  (case (mod (get-system-default-language-id) 1024) ; we are only interested in the primary language, which resides in the last 10 bits
    (#x16 "pt")
    (#x1D "sv")
    (t "default")))


(cl-locale:define-dictionary hello
  (:en '(("hello-world" . "Hello World!")
         ("easy-tip" . "This is easy!")
         ("start-button" . "Start!")
         ("tip-label" . "Hint:")))
  (:ru '(("hello-world" . "Привет Мир!")
         ("easy-tip" . "Тут всё просто!")
         ("start-button" . "Начать!")
         ("tip-label" . "Подсказка:"))))


(defun init ()
  ;; Without this, cl-locale returns our
  ;; sythetic keys without translation:
  (setf cl-locale:*default-locale* nil)

  (setf cl-locale:*locale*
        (let ((lang
               (default-language)))
          (cond
           ((string-equal lang "ru")
            :ru)
           (t :en))))

  (setf *initialized* t))


(defun i18n (key)
  (unless *initialized*
    (init))
  (cl-locale:i18n key))
