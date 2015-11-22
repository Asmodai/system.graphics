;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; point.lisp --- Point class.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 10:47:02 asmodai>
;;; Revision:   9
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 09:35:16
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

#-genera
(in-package #:system.graphics)

(declaim
 #+debug (optimize (speed 0)
                   (safety 3)
                   (debug 3))
 #-debug (optimize (speed 3)
                   (safety 0)
                   (debug 0)))


;; CLOS `print-object' method.
(defmethod print-object ((object point) stream)
  (print-unreadable-object (object stream
                            :type t
                            :identity t)
    (format stream "(~F,~F)"
            (point-x object)
            (point-y object))))

;; Is the geometry empty?  Not valid for points, so just dump `nil'
;; without caring.
(defmethod geom-empty-p ((object point))
  (declare (ignore object))
  nil)

;; Reader for the X slot.
(defmethod geom-x ((object point))
  (point-x object))

;; Writer for the X slot.
(defmethod (setf geom-x) (x (object point))
  (setf (point-x object) x))

;; Reader for the Y slot.
(defmethod geom-y ((object point))
  (point-y object))

;; Writer for the Y slot.
(defmethod (setf geom-y) (y (object point))
  (setf (point-y object) y))

;; Increment a point by a point.
(defmethod geom-incf ((object point) (delta point))
  (incf (point-x object) (point-x delta))
  (incf (point-y object) (point-y delta)))

;; Increment a point by a number.
(defmethod geom-incf ((object point) (delta number))
  (incf (point-x object) delta)
  (incf (point-y object) delta))

;; Decrement a point by a point.
(defmethod geom-decf ((object point) (delta point))
  (decf (point-x object) (point-x delta))
  (decf (point-y object) (point-y delta)))

;; Decrement a point by a number.
(defmethod geom-decf ((object point) (delta number))
  (decf (point-x object) delta)
  (decf (point-y object) delta))

;; Add two points.
(defmethod geom-add ((o1 point) (o2 point))
  (setf (point-x o1) (+ (point-x o1) (point-x o1))
        (point-y o1) (+ (point-y o1) (point-y o2))))

;; Add a point and a number.
(defmethod geom-add ((o1 point) (o2 number))
  (setf (point-x o1) (+ (point-x o1) o2)
        (point-y o1) (+ (point-y o1) o2)))

;; Add a point and a 2D vector.
(defmethod geom-add ((o1 point) (o2 euclidean-vector))
  (geom-add o1 (point<-vector o2)))

;; Subtract a point from a point.
(defmethod geom-sub ((o1 point) (o2 point))
  (setf (point-x o1) (- (point-x o1) (point-x o2))
        (point-y o1) (- (point-y o1) (point-y o2))))

;; Subtract a number from a point.
(defmethod geom-sub ((o1 point) (o2 number))
  (setf (point-x o1) (- (point-x o1) o2)
        (point-y o1) (- (point-y o1) o2)))

;; Subtract a 2D vector from a point.
(defmethod geom-sub ((o1 point) (o2 euclidean-vector))
  (geom-sub o1 (point<-vector o2)))

;; Multiply two points.
(defmethod geom-mul ((o1 point) (o2 point))
  (setf (point-x o1) (* (point-x o1) (point-x o2))
        (point-y o1) (* (point-y o1) (point-y o2))))

;; Multiply a point by a number.
(defmethod geom-mul ((o1 point) (o2 number))
  (setf (point-x o1) (* (point-x o1) (point-x o2))
        (point-y o1) (* (point-y o1) (point-y o2))))

;; Multiply a point by a 2D vector.
(defmethod geom-mul ((o1 point) (o2 euclidean-vector))
  (geom-mul o1 (point<-vector o2)))

;; Divide a point by a point.
(defmethod geom-div ((o1 point) (o2 point))
  (setf (point-x o1) (/ (point-x o1) (point-x o2))
        (point-y o1) (/ (point-y o1) (point-y o2))))

;; Divide a point by a number.
(defmethod geom-div ((o1 point) (o2 number))
  (setf (point-x o1) (/ (point-x o1) o2)
        (point-y o1) (/ (point-y o1) o2)))

;; Divide a point by a 2D vector.
(defmethod geom-div ((o1 point) (o2 euclidean-vector))
  (geom-div o1 (point<-vector o2)))

;; Are twp points equal?
(defmethod geom-eq ((o1 point) (o2 point))
  (or (eq o1 o2)
      (and (= (point-x o1) (point-x o2))
           (= (point-y o1) (point-y o2)))))

;; Are two points not equal?
(defmethod geom-neq ((o1 point) (o2 point))
  (not (geom-eq o1 o2)))

;; Compute the magnitude between two points and return a vector.
(defmethod geom-magnitude ((o1 point) (o2 point))
  (let ((x (- (point-x o2) (point-x o1)))
        (y (- (point-y o2) (point-y o1))))
    (make-euclidean-vector :x x
                           :y y)))

;; Does one point contain another?
(defmethod geom-contains-p ((o1 point) (o2 point)
                            &key (flipped nil))
  (if flipped
      (geom-neq o1 o2)
      (geom-eq o1 o2)))

;; Do the points intersect?
(defmethod geom-intersects-p ((o1 point) (o2 point))
  (geom-contains-p o1 o2))

;; Return the maximum X value.
(defmethod geom-max-x ((object point))
  (point-x object))

;; Return the middle X value.
(defmethod geom-mid-x ((object point))
  (point-x object))

;; Return the minimum X value.
(defmethod geom-min-x ((object point))
  (point-x object))

;; Return the maximum Y value.
(defmethod geom-max-y ((object point))
  (point-y object))

;; Return the middle Y value.
(defmethod geom-mid-y ((object point))
  (point-y object))

;; Return the minimum Y value.
(defmethod geom-min-y ((object point))
  (point-y object))

;; Return the bounds for a point.
;; Note that points cannot have bounding rects, so this method will
;; simply trigger an error.
(defmethod geom-bounds ((object point))
  (declare (ignore object))
  (error "A point cannot have a bounding rectangle."))

;; Offset a point by the given values.
(defmethod geom-offset ((object point) (x number) (y number))
  (setf (point-x object) (+ (point-x object) x)
        (point-y object) (+ (point-y object) y)))

;; Inset a point by the given values.
(defmethod geom-inset ((object point) (x number) (y number))
  (setf (point-x object) (- (point-x object) x)
        (point-y object) (- (point-y object) y)))

;; Divide a point into sub-points -- which cannot be done, at all,
;; ever.
(defmethod geom-divide ((object point) amount edge)
  (declare (ignore object amount edge))
  (error "Cannot divide a point into sub-points no matter now hard ~ 
          we try."))

;; Return the intersection of two points.
(defmethod geom-intersection ((o1 point) (o2 point))
  (if (geom-intersects-p o1 o2)
      o1
      nil))

;; Return the distance between two points.
(defmethod geom-distance ((o1 point) (o2 point))
  (vector-magnitude (geom-magnitude o1 o2)))

;;; point.lisp ends here
