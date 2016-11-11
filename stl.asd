;;;; stl.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:stl
  :description "Load triangle data from binary stereolithography (STL) files."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on 
  #+(and :little-endian :ieee-floating-point :sbcl)
  ()
  #-(and :little-endian :ieee-floating-point :sbcl)
  (#:ieee-floats)
  :serial t
  :components ((:file "package")
               (:file "stl")))

