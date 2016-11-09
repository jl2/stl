;;;; stl.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:stl
  :description "Read STL triangle files."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:ieee-floats)
  :serial t
  :components ((:file "package")
               (:file "stl")))

