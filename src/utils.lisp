;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; utils.lisp --- Various utilities.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 07:55:37 asmodai>
;;; Revision:   8
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 01:25:04
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

(defun round-to (number precision &optional (fun #'round))
  "Rounds the given number to the given precision using the given
rounding function.

The rounding function could be one of the following: `round',
`ceiling', `floor', `truncate', etc."
  (declare (type float number)
           (type number precision))
  (let ((div (expt 10.0 precision)))
    (/ (funcall fun (* number div)) div)))

(defun degrees<-radians (rads)
  "Convert a radian value to degrees."
  (declare (type number rads))
  (round-to (* 180.0s0 (/ rads +full-pi+)) 4))

(defun radians<-degrees (degs)
  "Converts a degree  value to radians."
  (declare (type number degs))
  (* +full-pi+ (/ degs 180.0s0)))

(defmacro make-vertex-array (n-vertices
                                &key (adjustable nil) (fill-pointer nil))
  "Create an array of 2D vertices."
  (let ((vertices (loop for i from 1 upto n-vertices
                        collect (list 'make-instance
                                      (list 'quote 'point)
                                      :x 0.0 :y 0.0))))
    `(make-array ,n-vertices
                 :element-type 'point
                 :initial-contents `(,,@vertices)
                 :adjustable ,adjustable
                 :fill-pointer ,fill-pointer)))

;;; utils.lisp ends here
