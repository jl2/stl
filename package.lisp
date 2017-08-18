;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:stl
  (:use #:cl)
  (:export

   #:read-stl
   #:stl-area

   #:point
   #:make-point
   #:point-x
   #:point-y
   #:point-z

   #:distance

   #:triangle
   #:make-triangle
   #:triangle-pt1
   #:triangle-pt2
   #:triangle-pt3

   #:triangle-area
   #:to-opengl
   ))
