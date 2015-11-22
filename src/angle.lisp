;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; angle.lisp --- Angle class.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 08:13:48 asmodai>
;;; Revision:   10
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 09:04:28
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

(defmethod print-object ((object angle) stream)
  (print-unreadable-object (object stream
                            :type t
                            :identity t)
    (format stream "~F rads"
            (slot-value object 'radians))))

(defgeneric angle-radians (angle)
  (:documentation "Returns the value of the given angle in radians.")
  (:method ((angle angle))
    (slot-value angle 'radians)))

(defgeneric (setf angle-radians) (radians angle)
  (:documentation "Sets the angle value in radians.")
  (:method (radians (angle angle))
    (declare (type number radians))
    (setf (slot-value angle 'radians) (float radians 1.0s0))))

(defgeneric angle-degrees (angle)
  (:documentation "Returns the value of the angle in degrees.")
  (:method ((angle angle))
    (degrees<-radians (slot-value angle 'radians))))

(defgeneric (setf angle-degrees) (degrees angle)
  (:documentation "Sets the angle value in degrees.")
  (:method (degrees (angle angle))
    (setf (slot-value angle 'radians) (radians<-degrees degrees))))

(defgeneric right-angle-p (angle)
  (:documentation "Returns T if the given angle is 90 degrees.")
  (:method ((angle float))
    (= angle +half-pi+))
  (:method ((angle angle))
    (right-angle-p (slot-value angle 'radians))))

(defgeneric complementary-angle (angle &key degrees)
  (:documentation "Returns the angle that would be required to produce
a complementary angle from the specified angle.

Returns NIL if the specified angle is greater than or equal to 90
degrees.

If `degrees' is non-NIL, the computation uses degrees.  The default is
to use radians.")
  (:method ((angle number) &key (degrees nil))
    (when (>= (abs angle) (if degrees
                              90.0s0
                              +half-pi+))
      (return-from complementary-angle nil))
    (- (if degrees 90.0s0 +half-pi+) angle))
  (:method ((angle angle) &key (degrees nil))
    (complementary-angle (if degrees
                             (angle-degrees angle)
                             (angle-radians angle))
                         :degrees degrees)))

(defgeneric supplementary-angle (angle &key degrees)
  (:documentation "Returns the angle that would be required to produce
a supplementary angle from the specified angle.

Returns NIL if the specified angle is greater than or equal to 180
degrees.

If `degrees' is non-NIL, the computation uses degrees.  The default is
to use radians.")
  (:method ((angle number) &key (degrees nil))
    (when (>= (abs angle) (if degrees
                              180.0s0
                              +full-pi+))
      (return-from supplementary-angle nil))
    (- (if degrees 180.0s0 +full-pi+) angle))
  (:method ((angle angle) &key (degrees nil))
    (supplementary-angle (if degrees
                             (angle-degrees angle)
                             (angle-radians angle))
                         :degrees degrees)))

(defgeneric reflex-angle (angle &key degrees)
    (:documentation "Returns the angle that is the reflex angle of the
specified angle.

If the specified angle is greater than 360 degrees, an error condition
will be signalled.

If `degrees' is non-NIL, the computation uses degrees.  The default is
to use radians.")
    (:method ((angle number) &key (degrees nil))
      (when (> (abs angle) (if degrees
                               360.0s0
                               +tau+))
        (error "Angle value is beyond 360 degrees."))
      (- (if degrees 360.0s0 +tau+) angle))
    (:method ((angle angle) &key (degrees nil))
      (reflex-angle (if degrees
                        (angle-degrees angle)
                        (angle-radians angle))
                    :degrees degrees)))

(defgeneric bisect-angle (angle)
  (:documentation "Returns the bisection of the given angle.")
  (:method ((angle angle))
    (make-angle :angle (/ (angle-radians angle) 2.0s0))))

(defgeneric trisect-angle (angle)
  (:documentation "Returns the trisection of the given angle.")
  (:method ((angle angle))
    (make-angle :angle (/ (angle-radians angle) 3.0s0))))

(defun angles-complementary-p (&rest angles)
  "Returns T if the sum of the angles specified in `angles' comes to
90 degrees exactly.

This function expects any numeric arguments to be degrees.  It will
not work as expected if you pass it any numeric radian values."
  (let ((total 0))
    (mapcar #'(lambda (a)
                (incf total (typecase a
                              (angle
                               (angle-radians a))
                              (number
                               (radians<-degrees a))
                              (t
                               (error 'invalid-angular-argument
                                      :proc 'angles-complementary-p
                                      :args (list 'angles
                                                  (class-name
                                                   (class-of a)))))))
                (if (> total +half-pi+)
                    (return-from angles-complementary-p nil)))
            angles)
    (= total +half-pi+)))

(defun angles-supplementary-p (&rest angles)
    "Returns T if the sum of the angles specified in `angles' comes to
180 degrees exactly.

This function expects any numeric arguments to be degrees.  It will
not work as expected if you pass it any numeric radian values."
    (let ((total 0))
      (mapcar #'(lambda (a)
                  (incf total (typecase a
                                (angle
                                 (angle-radians a))
                                (number
                                 (radians<-degrees a))
                                (t
                                 (error 'invalid-angular-argument
                                        :proc 'angles-supplementary-p
                                        :args (list 'angles
                                                    (class-name
                                                     (class-of
                                                      a)))))))
                  (if (> total +full-pi+)
                      (return-from angles-supplementary-p nil)))
              angles)
      (= total +full-pi+)))

(defmethod geom-empty-p ((object angle))
  (zerop (angle-radians object)))

(defmethod geom-incf ((object angle) (delta angle))
  (incf (angle-radians object) (angle-radians delta)))

(defmethod geom-incf ((object angle) (delta number))
  (incf (angle-radians object) delta))

(defmethod geom-decf ((object angle) (delta angle))
  (decf (angle-radians object) (angle-radians delta)))

(defmethod geom-decf ((object angle) (delta number))
  (decf (angle-radians object) delta))

(defmethod geom-add ((o1 angle) (o2 angle))
  (setf (angle-radians o1)
          (mod (+ (angle-radians o1) (angle-radians o2)) +tau+)))

(defmethod geom-add ((o1 angle) (o2 number))
  (setf (angle-radians o1)
          (mod (+ (angle-radians o1) o2) +tau+)))

(defmethod geom-sub ((o1 angle) (o2 angle))
  (setf (angle-radians o1)
          (mod (- (angle-radians o1) (angle-radians o2)) +tau+)))

(defmethod geom-sub ((o1 angle) (o2 number))
  (setf (angle-radians o1)
          (mod (- (angle-radians o1) o2) +tau+)))

(defmethod geom-mul ((o1 angle) (o2 angle))
  (setf (angle-radians o1)
          (mod (* (angle-radians o1) (angle-radians o2)) +tau+)))

(defmethod geom-mul ((o1 angle) (o2 number))
  (setf (angle-radians o1)
          (mod (* (angle-radians o1) o2) +tau+)))

(defmethod geom-div ((o1 angle) (o2 angle))
  (setf (angle-radians o1)
          (mod (float (/ (angle-radians o1) (angle-radians o2)) 1.0s0) +tau+)))

(defmethod geom-div ((o1 angle) (o2 number))
  (setf (angle-radians o1)
          (mod (float (/ (angle-radians o1) o2) 1.0s0) +tau+)))

(defmethod geom-eq ((o1 angle) (o2 angle))
  (or (eq o1 o2)
      (= (angle-radians o1) (angle-radians o2))))

(defmethod geom-eq ((o1 angle) (o2 number))
  (= (angle-radians o1) o2))

(defmethod geom-neq ((o1 angle) (o2 angle))
  (not (geom-eq o1 o2)))

(defmethod geom-neq ((o1 angle) (o2 number))
  (not (geom-eq o1 o2)))

;;; angle.lisp ends here
