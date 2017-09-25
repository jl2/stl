;;;; stl.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:stl)

(defparameter *float-byte-size* 4                                 "Size of an STL float in bytes.")
(defparameter *point-byte-size* (* 3 *float-byte-size*)           "Size of an STL point in bytes.")
(defparameter *triangle-byte-size* (+ 2 (* 4 *point-byte-size*))  "Size of an STL triangle in bytes.")

(defclass triangle ()
  ((normal :initarg :normal :type vec3)
   (pt1 :initarg :pt1 :type vec3)
   (pt2 :initarg :pt2 :type vec3)
   (pt3 :initarg :pt3 :type vec3)
   (attribute :initarg :attribute :initform 0 :type fixnum)))

(declaim (inline len triangle-area stl-area))
(defun distance (pt1 pt2)
  "Compute the distance between two points."
  (vlength (v- pt1 pt2)))

(defmethod area (object))
  
(defmethod area ((tri triangle))
  "Compute the area of a triangle."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type triangle tri))
  (with-slots (pt1 pt2 pt3) tri
    (let* ((a (distance pt1 pt2))
           (b (distance pt1 pt3))
           (c (distance pt2 pt3))
           (s (* 0.5f0 (+ a b c))))
      (declare (type single-float a b c s))
      (sqrt (abs (* s (- s a) (- s b) (- s c)))))))

(defun compute-triangle-normal (tri)
  "Compute the normal of a triangle."
  (with-slots (pt1 pt2 pt3 normal) tri
    (setf normal (vc (v- pt1 pt2) (v- pt1 pt3)))))

(defmethod area ((triangles simple-vector))
  "Compute the area of a vector of triangles."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (simple-array triangle) triangles))
  (loop for tri across triangles
     summing (triangle-area tri) single-float))

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
  (vec3 (get-float (make-array 4
                               :element-type '(unsigned-byte 8) 
                               :displaced-to arr 
                               :displaced-index-offset 0))
        (get-float (make-array 4
                               :element-type '(unsigned-byte 8) 
                               :displaced-to arr 
                               :displaced-index-offset *float-byte-size*))
        (get-float (make-array 4
                               :element-type '(unsigned-byte 8) 
                               :displaced-to arr 
                               :displaced-index-offset (* 2 *float-byte-size*)))))

(defun get-triangle (arr)
  "Read a triangle from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 3) (debug 3))
   (type (vector (unsigned-byte 8) #.(+ 2 (* 4 3 4))) arr))
  (make-instance 'triangle :normal (get-point (make-array #.(* 3 4)
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
  (v= pt (vec3 0.0 0.0 0.0)))

(defun to-opengl (triangles &key (color (vec4 0.0 1.0 0.0 1.0)))
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
    (declare (type fixnum pt-size tri-count))
    (flet ((show-point (idx pt normal color)
             (setf (aref rval (+ 0 idx)) (vx pt))
             (setf (aref rval (+ 1 idx)) (vy pt))
             (setf (aref rval (+ 2 idx)) (vz pt))
             (setf (aref rval (+ 3 idx)) (vx normal))
             (setf (aref rval (+ 4 idx)) (vy normal))
             (setf (aref rval (+ 5 idx)) (vz normal))
             (setf (aref rval (+ 6 idx)) (vx color))
             (setf (aref rval (+ 7 idx)) (vy color))
             (setf (aref rval (+ 8 idx)) (vz color))
             (setf (aref rval (+ 9 idx)) (vw color))))
      (loop for tri across triangles
         do
           (with-slots (pt1 pt2 pt3 normal) tri
             (when (zero-point normal)
               (setf normal (compute-triangle-normal tri)))
             (show-point idx pt1 normal color)
             (incf idx 10)
             (show-point idx pt2 normal color)
             (incf idx 10)
             (show-point idx pt3 normal color)
             (incf idx 10))))
    (values rval indices)))

(defun bounding-box (triangles)
  (let ((min-pt (slot-value (aref triangles 0) 'pt1))
        (max-pt (slot-value (aref triangles 0) 'pt1)))
    (loop for tri across triangles
       do
         (with-slots (pt1 pt2 pt3) tri
           (setf (vx min-pt) (min (vx pt1) (vx pt2) (vx pt3) (vx min-pt)))
           (setf (vy min-pt) (min (vy pt1) (vy pt2) (vy pt3) (vy min-pt)))
           (setf (vz min-pt) (min (vz pt1) (vz pt2) (vz pt3) (vz min-pt)))
           (setf (vx max-pt) (max (vx pt1) (vx pt2) (vx pt3) (vx max-pt)))
           (setf (vy max-pt) (max (vy pt1) (vy pt2) (vy pt3) (vy max-pt)))
           (setf (vz max-pt) (max (vz pt1) (vz pt2) (vz pt3) (vz max-pt)))))
    (list min-pt max-pt)))

