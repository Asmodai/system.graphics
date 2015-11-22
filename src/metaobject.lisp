;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; metaobject.lisp --- Graphics metaobject.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 10:46:49 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 09:05:18
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

;;;==================================================================
;;;{{{ `graphics-object' meta class:

;; This is abstract, so do not try to instantiate it.
(define-abstract-class graphics-object ()
  ()
  (:documentation "Base meta object inherited by all graphics
objects."))

(define-abstract-class graphics-object/color (graphics-object)
  ()
  (:documentation "Meta object for graphics objects that have a colour
attributes."))

(define-abstract-class graphics-object/drawable (graphics-object)
  ()
  (:documentation "Meta object for graphics objects that can be
drawn."))

(defun graphics-object-p (object)
  "Is the given object a graphics system entity?

If the object is a subclass of `graphics-object', then T is returned;
 otherwise NIL is returned."
  (typep object 'graphics-object))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Class forward declarations:

(defclass angle (graphics-object)
  ())

(defclass point (graphics-object/color
                 graphics-object/drawable)
  ())

(defclass euclidean-vector (graphics-object)
  ())

(defclass line (graphics-object/color
                graphics-object/drawable)
  ())

(defclass circle (graphics-object/color
                  graphics-object/drawable)
  ())

(defclass ellipse (circle)
  ())

(defclass polygon (graphics-object/color
                   graphics-object/drawable)
  ())

(defclass triangle (polygon)
  ())

(defclass rectangle (polygon)
  ())

(defclass path (polygon)
  ())

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Generic methods:

;;;------------------------------------------------------------------
;;;{{{ Accessors:

(defgeneric geom-x (object)
  (:documentation "Return the X value of the object."))

(defgeneric (setf geom-x) (x object)
  (:documentation "Set the X value of the object to the given
value."))

(defgeneric geom-y (object)
  (:documentation "Return the Y value of the object."))

(defgeneric (setf geom-y) (y object)
  (:documentation "Set the Y value of the object to the given
value."))

(defgeneric geom-z (object)
  (:documentation "Return the Z value of the object."))

(defgeneric (setf geom-z) (y object)
  (:documentation "Set the Z value of the object to the given
value."))

(defgeneric geom-start (object)
  (:documentation "Return the starting point of the object."))

(defgeneric geom-end (object)
  (:documentation "Return the ending point of the object."))

(defgeneric (setf geom-start) (start object)
  (:documentation "Set the starting point of the object."))

(defgeneric (setf geom-end) (end object)
  (:documentation "Set the ending point of the object."))

(defgeneric geom-width (object)
  (:documentation "Return the width value of the object."))

(defgeneric (setf geom-width) (width object)
  (:documentation "Set the width value of the object to the given
value."))

(defgeneric geom-height (object)
  (:documentation "Return the height value of the object."))

(defgeneric (setf geom-height) (height object)
  (:documentation "Set the height value of the object to the given
value."))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Predicates:

(defgeneric geom-empty-p (object)
  (:documentation "Returns T if the geometry of the given object is
empty."))

(defgeneric geom-contains-p (o1 o2 &key flipped)
  (:documentation "Returns T if any point of `o2' is inside `o1.

If `flipped' is non-NIL the test is reversed; returning T if `o2' is
outside `o1'."))

(defgeneric geom-intersects-p (o1 o2)
  (:documentation "Returns T if `o1' intersects `o2'."))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Mathematics primitives:

(defgeneric geom-incf (object delta)
  (:documentation "Increment an object by the given delta.")
  (:method :around (object delta)
    (call-next-method)
    object))

(defgeneric geom-decf (object delta)
  (:documentation "Decrement an object by the given delta.")
  (:method :around (object delta)
    (call-next-method)
    object))

(defgeneric geom-add (o1 o2)
  (:documentation "Add two objects and returns the result.")
  (:method :around (o1 o2)
    (call-next-method)
    o1))

(defgeneric geom-sub (o1 o2)
  (:documentation "Subtract two objects and returns the result.")
    (:method :around (o1 o2)
    (call-next-method)
    o1))

(defgeneric geom-mul (o1 o2)
  (:documentation "Multiplies two objects and returns the result.")
    (:method :around (o1 o2)
    (call-next-method)
    o1))

(defgeneric geom-div (o1 o2)
  (:documentation "Divides two objects and returns the results.")
    (:method :around (o1 o2)
    (call-next-method)
    o1))

(defgeneric geom-eq (o1 o2)
  (:documentation "Returns T if `o1' is equal to `o2'; otherwise, NIL
is returned."))

(defgeneric geom-neq (o1 o2)
  (:documentation "Returns T if `o1' is not equal to `o2'; otherwise,
NIL is returned."))

(defgeneric geom-magnitude (o1 o2)
  (:documentation "Returns the magnitude between two objects.  The
result is returned as a vector."))

(defgeneric geom-direction (object &key degrees)
  (:documentation "Returns the direction of an object.

The direction is taken from the (0,0) coordinate.

If `degrees' is non-NIL, the result is returned in degrees; otherwise
the result is returned in radians."))

(defgeneric geom-slope (object)
  (:documentation "Returns the slope (if any) of the given object."))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Coordinates:

(defgeneric geom-max-x (object)
  (:documentation "Return the maximum X coordinate of the object."))

(defgeneric geom-max-y (object)
  (:documentation "Return the maximum Y coordinate of the object."))

(defgeneric geom-mid-x (object)
  (:documentation "Returns the middle X coordinate of the object."))

(defgeneric geom-mid-y (object)
  (:documentation "Returns the middle Y coordinate of the object."))

(defgeneric geom-min-x (object)
  (:documentation "Returns the minimum X coordinate of the object."))

(defgeneric geom-min-y (object)
  (:documentation "Returns the minimum Y coordinate of the object."))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Operations:

(defgeneric geom-bounds (object)
  (:documentation "Returns the bounds of the given object."))

(defgeneric geom-offset (object x y)
  (:documentation "Create a new object that has been offset from the
given object by the given X and Y values."))

(defgeneric geom-inset (object x y)
  (:documentation "Create a new object that has been inset from the
given object by the given X and Y values."))

(defgeneric geom-divide (object amount edge)
  (:documentation "Divides an object by the given amount along the
given edge.

The `edge' argument must be one of the following:

:MINIMUM-X   The object is divided at the minimum X coordinate.
:MAXIMUM-X   The object is divided at the maximum X coordinate.
:MIDDLE-X    The object is divided at the middle X coordinate.
:MINIMUM-Y   The object is divided at the minimum Y coordinate.
:MAXIMUM-Y   The object is divided at the maximum Y coordinate.
:MIDDLE-Y    The object is divided at the middle Y coordinate."))

(defgeneric geom-union (o1 o2)
  (:documentation "Returns a new object which is a union of objects
`o1' and `o2'."))

(defgeneric geom-intersection (o1 o2)
  (:documentation "Returns the point at which objects `o1' and `o2'
intersect, or NIL if there are no intersectins."))

(defgeneric geom-distance (o1 o2)
  (:documentation "Returns the Euclidean distance between two
objects."))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;; metaobject.lisp ends here
