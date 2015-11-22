;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; constants.lisp --- Constants.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 11:39:39 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 11:31:15
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

(eval-when (:compile-toplevel) 
  (defconstant +vector-0+ (make-zero-euclidean-vector)
    "Empty 2D vector.")
  
  (defconstant +vector-1-y+ (make-euclidean-vector :x 0 :y 1)
    "2D vector with 1 unit in the Y direction.")
  
  (defconstant +vector-1-x+ (make-euclidean-vector :x 1 :y 0)
    "2D vector with 1 unit in the X direction.")
  
  (defconstant +vector-1+ (make-euclidean-vector :x 1 :y 1)
    "2D vector with 1 unit in both X and Y directions."))

;;; constants.lisp ends here
