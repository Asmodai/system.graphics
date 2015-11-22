;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; parameters.lisp --- Parameters.
;;;
;;; Time-stamp: <Saturday Nov 21, 2015 08:53:27 asmodai>
;;; Revision:   1
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 08:50:29
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

(defparameter *suppress-graphics-warnings* nil
  "Set T to suppress friendly warnings.")

(defconstant +tau+ (float (* 2 pi) 1.0s0)
  "2pi in short-float format.")

(defconstant +full-pi+ (float pi 1.0s0)
  "Pi in short-float format.")

(defconstant +half-pi+ (float (/ pi 2) 1.0s0)
  "Pi/2 in short-float format.")

(defconstant +one-degree-in-rads+ (float (* +full-pi+ (/ 1.0s0 180.0s0)))
  "1 degree converted to radians.")

(defparameter *circle-types*
  '(:circle
    :arc
    :segment
    :curve)
  "Circle types.")

(defparameter *ellipse-types*
  '(:ellipse
    :arc
    :segment
    :curve)
  "Ellipse types.")

;;; parameters.lisp ends here
