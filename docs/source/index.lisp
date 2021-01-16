(defpackage #:docs/index
  (:nicknames #:docs)
  (:use #:cl)
  (:import-from #:mgl-pax
                #:section
                #:defsection)
  (:export
   #:build-docs))
(in-package docs/index)


(defsection @index (:title "Multiplication Game!")
  "
Helps your child learn the multiplication table and improve their math skills.

<img src=\"static/demo.gif\" width=\"70%\"/>

In a playful way, it helps to learn the multiplication table.

Main Features:

- shows hints to make learning easier;
- goes from simple examples to complex ones;
- shows seals!
")


(defun build-docs ()
  (mgl-pax:update-asdf-system-readmes @index :docs)
  
  (mgl-pax:update-asdf-system-html-docs
   @index :docs
   :target-dir "docs/build/"
   :pages `((:objects (,docs:@index)
             :source-uri-fn ,(pax:make-github-source-uri-fn
                              :docs
                              "https://github.com/40ants/multiplication"))))

  (uiop:run-program "rm -fr docs/build/static")
  ;; We keep static in the root folder to make images accessable
  ;; from the README
  (uiop:run-program "cp -R static docs/build/static")
  (values))
