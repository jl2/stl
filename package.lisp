;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:stl
  (:use #:cl #:3d-vectors)
  (:export

   #:read-stl

   #:distance

   #:triangle

   #:pt1
   #:pt2
   #:pt3
   #:normal
   #:attribute

   #:area
   #:to-opengl
   #:bounding-box
   ))
