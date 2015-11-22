;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; line.lisp --- Line methods.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 09:20:00 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 09:07:09
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

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream
                            :type t
                            :identity t)
    (format stream "(~D,~D)-(~D,~D)"
            (point-x (slot-value object 'start))
            (point-y (slot-value object 'start))
            (point-x (slot-value object 'end))
            (point-y (slot-value object 'end)))))

(defmethod geom-empty-p ((object line))
  (and (= (point-x (slot-value object 'start))
          (point-x (slot-value object 'end)))
       (= (point-y (slot-value object 'start))
          (point-y (slot-value object 'end)))))

(defmethod geom-start ((object line))
  (slot-value object 'start))

(defmethod (setf geom-start) ((start point) (object line))
  (setf (slot-value object 'start) start))

(defmethod geom-end ((object line))
  (slot-value object 'end))

(defmethod (setf geom-end) ((end point) (object line))
  (setf (slot-value object 'end) end))

(defmethod geom-slope ((object line))
  (let* ((x0 (point-x (geom-start object)))
         (y0 (point-y (geom-start object)))
         (x1 (point-x (geom-end object)))
         (y1 (point-y (geom-end object)))
         (rise (float (- y0 y1)))
         (run (float (- x0 x1)))
         slope)
    (cond ((= 0 run)                    ; Y points on same X.
           (if (plusp rise)
               (setq slope :down)
               (setq slope
                     :up)))
          ((= 0 rise)                   ; Left to right.
           (if (< x0 x1)
               (setq slope
                     :right)
               (setq
                slope
                :left)))
          (t                            ; Now set slope value.
           (setq slope (/ rise run))))))

(defmethod geom-direction ((object line) &key (degrees nil))
  (let ((d (geom-slope object)))
    (when degrees
      (setq d (/ (* (atan d) 180.0s0) pi)))
    d))

(defmethod geom-eq ((o1 line) (o2 line))
  (or (eq o1 o2)
      (= (geom-eq (geom-start o1) (geom-start o2))
         (geom-eq (geom-end o1) (geom-end o2)))))

(defmethod geom-intersection ((o1 line) (o2 line))
  (let* ((x1 (point-x (geom-start o1)))
         (x2 (point-x (geom-end o1)))
         (x3 (point-x (geom-start o2)))
         (x4 (point-x (geom-end o2)))
         (y1 (point-y (geom-start o1)))
         (y2 (point-y (geom-end o1)))
         (y3 (point-y (geom-start o2)))
         (y4 (point-y (geom-end o2)))
         (denom (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1))))
         (ua-num (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3))))
         (ub-num (- (* (- x2 x1) (- y1 y3)) (* (- y2 y1) (- x1 x3)))))
    (cond ((zerop denom)
           (return-from geom-intersection nil))
          (t
           (let ((ua (/ ua-num denom))
                 (ub (/ ub-num denom)))
             (if (and (<= 0 ua 1)
                      (<= 0 ub 1))
                 (make-point (float (+ x1 (* ua (- x2 x1))))
                             (float (+ y1 (* ua (- y2 y1)))))
                 nil))))))

(defmethod geom-intersects-p ((o1 line) (o2 line))
  (not (null (geom-intersection o1 o2))))

(defmethod geom-distance ((o1 point) (o2 line))
  (let* ((mag (vector-magnitude
               (geom-magnitude (geom-start o2) (geom-end o2))))
         (px (point-x o1))
         (py (point-y o1))
         (x1 (point-x (geom-start o2)))
         (y1 (point-y (geom-start o2)))
         (x2 (point-x (geom-end o2)))
         (y2 (point-y (geom-end o2)))
         (u (/ (+ (* (- px x1) (- x2 x1))
                  (* (- py y1) (- y2 y1)))
               (* mag mag)))
         (ix 0)
         (iy 0))
    (when (< mag 0.00000001)
      (return-from geom-distance :infinity))
    (if (or (< u 0.0) (> u 1))
        (progn
          (setf ix (vector-magnitude (geom-magnitude o1 (geom-start o2)))
                iy (vector-magnitude (geom-magnitude o1 (geom-end o2))))
          (if (> ix iy)
              iy
              ix))
        (progn
          (setf ix (+ x1 (* u (- x2 x1)))
                iy (+ y1 (* u (- y2 y1))))
          (vector-magnitude (geom-magnitude o1 (make-point ix iy)))))))

;;; line.lisp ends here
