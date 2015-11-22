;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; vector.lisp --- Euclidean vectors.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 10:46:57 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 08:19:40
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

(defmethod print-object ((object euclidean-vector) stream)
  (print-unreadable-object (object stream
                            :type t
                            :identity t)
    (format stream "(~D,~D)"
            (point-x object)
            (point-y object))))

(defgeneric vector-magnitude (vec)
  (:documentation "Returns the magnitude of a vector.")
  (:method ((vec euclidean-vector))
    (let ((x (point-x vec))
          (y (point-y vec)))
      (sqrt (+ (* x x) (* y y))))))

(defgeneric vector-angle (vec &key degrees)
  (:documentation "Returns the angle of a vector.")
  (:method ((vec euclidean-vector) &key (degrees nil))
    (let ((angle (atan (point-y vec) (point-x vec))))
      (if degrees
          (degrees<-radians angle)
          angle))))

(defmethod geom-empty-p ((object euclidean-vector))
  (and (zerop (point-x object))
       (zerop (point-y object))))

(defmethod geom-x ((object euclidean-vector))
  (point-x object))

(defmethod (setf geom-x) (x (object euclidean-vector))
  (setf (point-x object) x))

(defmethod geom-y ((object euclidean-vector))
  (point-y object))

(defmethod (setf geom-y) (y (object euclidean-vector))
  (setf (point-y object) y))

(defmethod geom-incf ((object euclidean-vector) (delta euclidean-vector))
  (incf (point-x object) (point-x delta))
  (incf (point-y object) (point-y delta)))

(defmethod geom-incf ((object euclidean-vector) (delta number))
  (incf (point-x object) delta)
  (incf (point-y object) delta))

(defmethod geom-incf ((object euclidean-vector) (delta point))
  (incf (point-x object) (point-x delta))
  (incf (point-y object) (point-y delta)))

(defmethod geom-decf ((object euclidean-vector) (delta euclidean-vector))
  (decf (point-x object) (point-x delta))
  (decf (point-y object) (point-y delta)))

(defmethod geom-decf ((object euclidean-vector) (delta number))
  (decf (point-x object) delta)
  (decf (point-y object) delta))

(defmethod geom-decf ((object euclidean-vector) (delta point))
  (decf (point-x object) (point-x delta))
  (decf (point-y object) (point-y delta)))

(defmethod geom-add ((o1 euclidean-vector) (o2 euclidean-vector))
  (setf (point-x o1) (+ (point-x o1) (point-x o2))
        (point-y o1) (+ (point-y o1) (point-y o2))))

(defmethod geom-add ((o1 euclidean-vector) (o2 number))
  (setf (point-x o1) (+ (point-x o1) o2)
        (point-y o1) (+ (point-y o1) o2)))

(defmethod geom-add ((o1 euclidean-vector) (o2 point))
  (setf (point-x o1) (+ (point-x o1) (point-x o2))
        (point-y o1) (+ (point-y o1) (point-y o2))))

(defmethod geom-sub ((o1 euclidean-vector) (o2 euclidean-vector))
  (setf (point-x o1) (- (point-x o1) (point-x o2))
        (point-y o1) (- (point-y o1) (point-y o2))))

(defmethod geom-sub ((o1 euclidean-vector) (o2 number))
  (setf (point-x o1) (- (point-x o1) o2)
        (point-y o1) (- (point-y o1) o2)))

(defmethod geom-sub ((o1 euclidean-vector) (o2 point))
  (setf (point-x o1) (- (point-x o1) (point-x o2))
        (point-y o1) (- (point-y o1) (point-y o2))))

(defmethod geom-mul ((o1 euclidean-vector) (o2 euclidean-vector))
  (setf (point-x o1) (* (point-x o1) (point-x o2))
        (point-y o1) (* (point-y o1) (point-y o2))))

(defmethod geom-mul ((o1 euclidean-vector) (o2 number))
  (setf (point-x o1) (* (point-x o1) o2)
        (point-y o1) (* (point-y o1) o2)))

(defmethod geom-mul ((o1 euclidean-vector) (o2 point))
  (setf (point-x o1) (* (point-x o1) (point-x o2))
        (point-y o1) (* (point-y o1) (point-y o2))))

(defmethod geom-div ((o1 euclidean-vector) (o2 euclidean-vector))
  (setf (point-x o1) (/ (point-x o1) (point-x o2))
        (point-y o1) (/ (point-y o1) (point-y o2))))

(defmethod geom-div ((o1 euclidean-vector) (o2 number))
  (setf (point-x o1) (/ (point-x o1) o2)
        (point-y o1) (/ (point-y o1) o2)))

(defmethod geom-div ((o1 euclidean-vector) (o2 point))
  (setf (point-x o1) (/ (point-x o1) (point-x o2))
        (point-y o1) (/ (point-y o1) (point-y o2))))

(defmethod geom-eq ((o1 euclidean-vector) (o2 euclidean-vector))
  (or (eq o1 o2)
      (and (= (point-x o1) (point-x o2))
           (= (point-y o1) (point-y o2)))))

(defmethod geom-neq ((o1 euclidean-vector) (o2 euclidean-vector))
  (not (geom-eq o1 o2)))

(defmethod geom-magnitude ((o1 euclidean-vector) (o2 euclidean-vector))
  (let ((x (- (point-x o2) (point-x o1)))
        (y (- (point-y o2) (point-y o1))))
    (make-euclidean-vector :x x
                           :y y)))

(defmethod geom-contains-p ((o1 euclidean-vector) (o2 euclidean-vector)
                            &key (flipped nil))
  (if flipped
      (geom-neq o1 o2)
      (geom-eq o1 o2)))

(defmethod geom-intersects-p ((o1 euclidean-vector) (o2 euclidean-vector))
  (geom-contains-p o1 o2))

(defmethod geom-max-x ((object euclidean-vector))
  (point-x object))

(defmethod geom-mid-x ((object euclidean-vector))
  (point-x object))

(defmethod geom-min-x ((object euclidean-vector))
  (point-x object))

(defmethod geom-max-y ((object euclidean-vector))
  (point-y object))

(defmethod geom-mid-y ((object euclidean-vector))
  (point-y object))

(defmethod geom-min-y ((object euclidean-vector))
  (point-y object))

(defmethod geom-bounds ((object euclidean-vector))
  (declare (ignore object))
  (error "A vector cannot have a bounding rectangle."))

(defmethod geom-offset ((object euclidean-vector) (x number) (y number))
  (setf (point-x object) (+ (point-x object) x)
        (point-y object) (+ (point-y object) y)))

(defmethod geom-inset ((object euclidean-vector) (x number) (y number))
  (setf (point-x object) (- (point-x object) x)
        (point-y object) (- (point-y object) y)))

(defmethod geom-divide ((object euclidean-vector) amount edge)
  (declare (ignore object amount edge))
  (error "A vector cannot be devided into sub-vectors."))

(defmethod geom-intersection ((o1 euclidean-vector) (o2 euclidean-vector))
  (if (geom-intersects-p o1 o2)
      o1
      nil))

(defmethod geom-distance ((o1 euclidean-vector) (o2 euclidean-vector))
  (vector-magnitude (geom-magnitude o1 o2)))

(defmethod geom-direction ((object euclidean-vector) &key (degrees nil))
  (vector-angle object :degrees degrees))

;;; vector.lisp ends here
