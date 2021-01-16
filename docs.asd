(defsystem docs
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("docs/index"))
