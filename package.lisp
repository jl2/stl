;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:stl
  (:use #:cl #:geometry)
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
   #:triangle-normal
   #:triangle-pt1
   #:triangle-pt2
   #:triangle-pt3
   #:triangle-attribute
   #:pt1
   #:pt2
   #:pt3
   #:normal

   #:triangle-area
   #:to-opengl
   #:bounding-box
   ))
