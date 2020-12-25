(in-package "CL-USER")

(load-all-patches)

(require 'asdf)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push "./" asdf:*central-registry*)

;; Without this, russian text is loaded incorrectly:

(defun force-utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

(set-default-character-element-type 'character)

(setf stream::*default-external-format* '(:utf-8 :eol-style :lf))

(setf system:*file-encoding-detection-algorithm*
      '(force-utf-8-file-encoding))


(asdf:load-system :multiplication/core)


(let* ((app-path (merge-pathnames #P"Multitrainer.app" (lispworks:current-pathname)))
       (bundle (create-macos-application-bundle
                app-path
                ;; Do not copy file associations...
                :document-types nil
                :application-icns (asdf/system:system-relative-pathname :multiplication
                                                                        "logo/logo.icns")
                ;; ...or CFBundleIdentifier from the LispWorks bundle
                :identifier "com.40ants.multitrainer"
                :version "0.1.0"
                )))

  (deliver 'multiplication/core::start 
           bundle
           ;; level of compression
           ;; from 0 to 5 where 5 is resulting the
           ;; smallest binary
           4
           :interface :capi
           :split :resources
           ;; To suppress LispWork's splash screen
           :startup-bitmap-file nil))