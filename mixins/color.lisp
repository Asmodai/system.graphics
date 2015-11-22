;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS.MIXINS; Base: 10; Lowercase: Yes -*-
;;;
;;; color.lisp --- Colour mixin.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 09:01:12 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 03:16:46
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
(in-package #:system.graphics.mixins)

(defclass graphics-color-mixin ()
  ((background-ink
    :initarg :background-ink
    :initform +transparent-color+
    :type color)
   (foreground-ink
    :initarg :foreground-ink
    :initform +black+
    :type color)))

(defgeneric geom-foreground-ink (object)
  (:method ((object graphics-color-mixin))
    (slot-value object 'foreground-ink)))

(defgeneric (setf geom-foreground-ink) (ink object)
  (:method ((ink color) (object graphics-color-mixin))
    (setf (slot-value object 'foreground-ink) ink)))

(defgeneric geom-background-ink (object)
  (:method ((object graphics-color-mixin))
    (slot-value object 'background-ink)))

(defgeneric (setf geom-background-ink) (ink object)
  (:method ((ink color) (object graphics-color-mixin))
    (setf (slot-value object 'background-ink) ink)))

(defgeneric geom-flip-inks (object)
  (:method ((object graphics-color-mixin))
    (let ((tmp (slot-value object 'background-ink)))
      (setf (geom-background-ink object) (geom-foreground-ink object)
            (geom-foreground-ink object) tmp))))

;;; color.lisp ends here
