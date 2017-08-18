;;;; stl.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:stl)

(defstruct point
  "Three single-floats that identify a location in 3D space."
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defun psub (p1 p2)
  (with-slots ((x1 x) (y1 y) (z1 z)) p1
    (with-slots ((x2 x) (y2 y) (z2 z)) p2
      (make-point :x (- x1 x2)
                  :y (- y1 y2)
                  :z (- z1 z2)))))

(defparameter *float-byte-size* 4                                 "Size of an STL float in bytes.")
(defparameter *point-byte-size* (* 3 *float-byte-size*)           "Size of an STL point in bytes.")
(defparameter *triangle-byte-size* (+ 2 (* 4 *point-byte-size*))  "Size of an STL triangle in bytes.")

(defstruct triangle
  "Three points, a normal, and an attribute"
  (normal (make-point) :type point)
  (pt1 (make-point) :type point)
  (pt2 (make-point) :type point)
  (pt3 (make-point) :type point)
  (attribute 0 :type fixnum))

(declaim (inline len triangle-area stl-area))
(defun distance (pt1 pt2)
  "Compute the distance between two points."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type point pt1 pt2))
  (let ((xd (- (point-x pt1) (point-x pt2)))
        (yd (- (point-y pt1) (point-y pt2)))
        (zd (- (point-z pt1) (point-z pt2))))
    (sqrt (+ (* xd xd) (* yd yd) (* zd zd)))))

(defun triangle-area (tri)
  "Compute the area of a triangle."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type triangle tri))
  (with-slots (pt1 pt2 pt3) tri
    (let* ((a (distance pt1 pt2))
           (b (distance pt1 pt3))
           (c (distance pt2 pt3))
           (s (* 0.5f0 (+ a b c))))
      (the single-float (sqrt (* s (- s a) (- s b) (- s c)))))))

(defun cross (v1 v2)
  (with-slots ((x1 x) (y1 y) (z1 z)) v1
    (with-slots ((x2 x) (y2 y) (z2 z)) v2
      (make-point :x (- (* y1 z2) (* z1 y2))
                  :y (- (* z1 x2) (* x1 z2))
                  :z (- (* x1 y2) (* y1 x2))))))

(defun triangle-normal (tri)
  "Compute the normal of a triangle."
  (with-slots (pt1 pt2 pt3 normal) tri
    (setf tri (cross (psub pt1 pt2) (psub pt1 pt3)))))

(defun stl-area (triangles)
  "Compute the area of a vector of triangles."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector triangle) triangles))
  (loop for tri across triangles
     summing (triangle-area tri)))

(defun get-u2 (arr)
  "Interpret two bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) 2) arr))
  (the (unsigned-byte 32) (+ (* (aref arr 1) 256) (aref arr 0))))

(defun get-u4 (arr)
  "Interpret the four bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) 4) arr))
  (the (unsigned-byte 32) (+ (* (+ (* (+ (* (aref arr 3) 256) (aref arr 2)) 256) (aref arr 1)) 256) (aref arr 0))))

(defun get-s4 (arr)
  "Interpret four bytes in arr as an '(signed-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) 4) arr))
  (the (signed-byte 32) (+ (* (+ (* (+ (* (aref arr 3) 256) (aref arr 2)) 256) (aref arr 1)) 256) (aref arr 0))))

(defun get-float (arr)
  "Interpret four bytes in arr as a single-float."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) 4) arr))
  (let ((x (get-u4 arr)))
    #+(and :little-endian :ieee-floating-point :sbcl)
    (if (>= x #x80000000)
        (sb-kernel:make-single-float (- x #x100000000))
        (sb-kernel:make-single-float x))
    #-(and :little-endian :ieee-floating-point :sbcl)
    (ieee-floats:decode-float32 45)))

(defun get-point (arr)
  "Create a point using x, y, and z values read from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) #.(* 3 4)) arr))
  (make-point :x (get-float (make-array 4
                                        :element-type '(unsigned-byte 8) 
                                        :displaced-to arr 
                                        :displaced-index-offset 0))
              :y (get-float (make-array 4
                                        :element-type '(unsigned-byte 8) 
                                        :displaced-to arr 
                                        :displaced-index-offset *float-byte-size*))
              :z (get-float (make-array 4
                                        :element-type '(unsigned-byte 8) 
                                        :displaced-to arr 
                                        :displaced-index-offset (* 2 *float-byte-size*)))))

(defun get-triangle (arr)
  "Read a triangle from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) #.(+ 2 (* 4 3 4))) arr))
  (make-triangle :normal (get-point (make-array #.(* 3 4)
                                                :element-type '(unsigned-byte 8)
                                                :displaced-to arr
                                                :displaced-index-offset 0))
                 :pt1 (get-point (make-array #.(* 3 4)
                                             :element-type '(unsigned-byte 8)
                                             :displaced-to arr
                                             :displaced-index-offset *point-byte-size*))
                 :pt2 (get-point (make-array #.(* 3 4)
                                             :element-type '(unsigned-byte 8)
                                             :displaced-to arr
                                             :displaced-index-offset (* 2 *point-byte-size*)))
                 :pt3 (get-point (make-array #.(* 3 4)
                                             :element-type '(unsigned-byte 8)
                                             :displaced-to arr
                                             :displaced-index-offset (* 3 *point-byte-size*)))
                 :attribute (get-u2 (make-array 2 :element-type '(unsigned-byte 8)))))

(defun read-stl (fname)
  "Read an STL file from fname and return the vector of triangles."
  (with-open-file (inf fname :element-type '(unsigned-byte 8))
    (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
          (triangle-count-buffer (make-array 4 :element-type '(unsigned-byte 8)))
          (triangle-count '(unsigned-byte 32)))
      (read-sequence header inf)
      (read-sequence triangle-count-buffer inf)
      (setf triangle-count (get-u4 triangle-count-buffer))
      (let ((buffer (make-array (* triangle-count *triangle-byte-size*) :element-type '(unsigned-byte 8)))
            (triangles (make-array triangle-count :element-type 'triangle )))
        (read-sequence buffer inf)
        (loop for idx below triangle-count
           for offset = 0 then (* idx *triangle-byte-size*)
           do
             (setf (aref triangles idx)
                   (get-triangle (make-array #.(+ 2 (* 4 3 4))
                                             :element-type '(unsigned-byte 8)
                                             :displaced-to buffer
                                             :displaced-index-offset offset))))
        triangles))))

(defun zero-point (pt)
  (with-slots (x y z) pt
    (and (= 0.0 x) (= 0.0 y) (= 0.0 z))))

(defun to-opengl (triangles &key(red 0.0) (green 1.0) (blue 0.0) (alpha 1.0))
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector triangle) triangles))
  (let* ((tri-count (length triangles))
         (pt-size 10)
         (rval (make-array
                (* pt-size 3 tri-count)
                :element-type 'single-float
                :initial-element 66.0f0))
         (idx 0)
         (indices (make-array
                   (* 3 tri-count)
                   :element-type 'fixnum
                   :initial-contents (loop for i below (* 3 tri-count) collecting i))))
    (format t "Tri-count: ~a array-size ~a~%" tri-count (* pt-size 3 tri-count))
    (flet ((show-point (idx pt normal red green blue alpha)
             (with-slots (x y z) pt
               (setf (aref rval (+ 0 idx)) (coerce x 'single-float))
               (setf (aref rval (+ 1 idx)) (coerce y 'single-float))
               (setf (aref rval (+ 2 idx)) (coerce z 'single-float)))
             (with-slots (x y z) normal
               (setf (aref rval (+ 3 idx)) (coerce x 'single-float))
               (setf (aref rval (+ 4 idx)) (coerce y 'single-float))
               (setf (aref rval (+ 5 idx)) (coerce z 'single-float)))
             (setf (aref rval (+ 6 idx)) (coerce red 'single-float))
             (setf (aref rval (+ 7 idx)) (coerce green 'single-float))
             (setf (aref rval (+ 8 idx)) (coerce blue 'single-float))
             (setf (aref rval (+ 9 idx)) (coerce alpha 'single-float))))
      (loop for tri across triangles
         do
           (with-slots (pt1 pt2 pt3 normal) tri
             (when (zero-point normal)
               (setf normal (triangle-normal tri)))
             (show-point idx pt1 normal red green blue alpha)
             (incf idx 10)
             (show-point idx pt2 normal red green blue alpha)
             (incf idx 10)
             (show-point idx pt3 normal red green blue alpha)
             (incf idx 10))))
    (values rval indices)))

(defun bounding-box (triangles)
  (let ((min-pt (make-point :x most-positive-single-float :y most-positive-single-float :z most-positive-single-float))
        (max-pt (make-point :x most-negative-single-float :y most-negative-single-float :z most-negative-single-float)))
    (loop for tri across triangles
       do
         (with-slots (pt1 pt2 pt3) tri
           (setf (point-x min-pt) (min (point-x pt1) (point-x pt2) (point-x pt3) (point-x min-pt)))
           (setf (point-y min-pt) (min (point-y pt1) (point-y pt2) (point-y pt3) (point-y min-pt)))
           (setf (point-z min-pt) (min (point-z pt1) (point-z pt2) (point-z pt3) (point-z min-pt)))
           (setf (point-x max-pt) (max (point-x pt1) (point-x pt2) (point-x pt3) (point-x max-pt)))
           (setf (point-y max-pt) (max (point-y pt1) (point-y pt2) (point-y pt3) (point-y max-pt)))
           (setf (point-z max-pt) (max (point-z pt1) (point-z pt2) (point-z pt3) (point-z max-pt)))))
    (list min-pt max-pt)))

