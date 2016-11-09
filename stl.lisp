;;;; stl.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:stl)

(defstruct point
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(defparameter *triangle-byte-size* (+ 2 (* 4 3 4)))

(defstruct triangle
  (pts (make-array 4 :element-type 'point :adjustable nil :fill-pointer nil))
  (attribute 0 :type '(unsigned-byte 16)))

;; (defun get-u4 (arr)
;;   (declare
;;    (optimize (speed 3) (space 0) (safety 0) (debug 0))
;;    (type (simple-array (unsigned-byte 8) (4)) arr))
;;   (let* ((u4 0))
;;     (declare (type (unsigned-byte 32) u4))
;;     (setf (ldb (byte 8 0) u4) (aref arr 0))
;;     (setf (ldb (byte 8 8) u4) (aref arr 1))
;;     (setf (ldb (byte 8 16) u4) (aref arr 2))
;;     (setf (ldb (byte 8 24) u4) (aref arr 3))
;;     u4))

(defun get-u2 (arr offset)
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type fixnum offset)
   (type (simple-array (unsigned-byte 8) *) arr))
  (the (unsigned-byte 32) (+ (* (aref arr (+ offset 1)) 256) (aref arr offset))))

(defun get-u4 (arr offset)
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type fixnum offset)
   (type (simple-array (unsigned-byte 8) *) arr))
  (the (unsigned-byte 32) (+ (* (+ (* (+ (* (aref arr (+ offset 3)) 256) (aref arr (+ offset 2))) 256) (aref arr (+ offset 1))) 256) (aref arr offset))))

(make-array (list *triangle-byte-size*) :element-type '(unsigned-byte 8) :displaced-to buffer :displaced-index-offset offset)

(defun get-point (arr )

(defun get-triangle (arr)
  (let ((rval (make-triangle )))
    (
    (let ((buffer (make-array *triangle-byte-size* :element-type '(unsigned-byte 8))))
    (read-sequence inf buffer)))

(defun read-stl (fname)
  (format t "Reading file ~a~%" fname)
  (with-open-file (inf fname :element-type '(unsigned-byte 8))
    (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
          (triangle-count '(unsigned-byte 32)))
      (read-sequence header inf :start 0 :end 80)
      (setf triangle-count (read-u4 inf))
      (let ((buffer (make-array (* triangle-count *triangle-byte-size*) :element-type '(unsigned-byte 8)))
            (triangles (make-array triangle-count :element-type 'triangle )))
        (read-sequence buffer inf)
        (loop for idx upto triangle-count
           for offset = 0 then (* idx *triangle-byte-size*)
           do
             (setf (aref triangles idx)
                   (read-triangle (make-array (list *triangle-byte-size*) :element-type '(unsigned-byte 8) :displaced-to buffer :displaced-index-offset offset) )))
        (format t "File has ~a triangles!~%" triangle-count)
        
        triangles))))

