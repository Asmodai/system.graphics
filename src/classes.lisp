;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; classes.lisp --- Class definitions.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 11:36:53 asmodai>
;;; Revision:   26
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    22 Nov 2015 04:14:31
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
;;;{{{ The `angle' class:

(defclass angle (graphics-object)
  ((radians
    :initarg :radians
    :initform 0.0s0
    :type short-float
    :documentation "The value of the angle expressed in radians."))
  (:documentation "This class represents an angular value."))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

;; Create a new angle.
(defun make-angle (&key (angle 0.0s0) (degrees nil))
    (declare (type number angle))
    (let ((limit (if degrees
                        360.0s0
                        +tau+)))
      (if (or (> angle limit)
              (< angle 0.0s0))
          (setq angle (mod angle limit)))
      (make-instance 'angle
         :radians (if degrees
                      (radians<-degrees angle)
                      (float angle 1.0s0)))))

;; Create an empty angle.
(defun make-zero-angle ()
  (make-instance 'angle
     :radians 0.0s0))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-angle 'function
  "Makes an angle with the given value.

If `degrees' is non-NIL, then the value will be converted from degrees
to radians.

It is important to note that the angluar value should be a radian
value.  If you plan on using degrees, set `degrees' to T while
constructing your angle.")

(define-documentation 'make-zero-angle 'function
    "Makes an angle with 0 as the initial value.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `point' class:

(defclass point (graphics-object/color
                 graphics-object/drawable)
  ((x
    :initarg :x
    :initform 0.0s0
    :type short-float
    :accessor point-x
    :documentation "The X coordinate on the Cartesian plane.")
   (y
    :initarg :y
    :initform 0.0s0
    :type short-float
    :accessor point-y
    :documentation "The Y coordinate on the Cartesian plane."))
  (:documentation "This class represents a 2-dimensional Cartesian
coordinate."))

;;;------------------------------------------------------------------
;;;{{{ Construction methods:

;; Create a new point.
(defun make-point (&key (x 0.0s0) (y 0.0s0))
  (declare (type number x y))
  #+debug
  (format *debug-io* "~&;; --> Creating a point: X=~F, Y=~F"
          x
          y)
  (make-instance 'point
     :x (float x 1.0s0)
     :y (float y 1.0s0)))

;; Create an empty point.
(defun make-zero-point ()
  (make-instance 'point))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-point 'function
  "Makes a point with the given X and Y coordinates.")

(define-documentation 'make-zero-point 'function
  "Makes a point with zeros as the initial X and Y coordinates.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `euclidean-vector' class::

;;;
;;; polar -> cartesian
;;;   x     = r * cos(phi)
;;;   y     = r * sin(phi)
;;;
;;; cartesian -> polar
;;;   r     = sqrt(x^2 + y^2)        - magnitude
;;;   phi   = atan(y,x)              - angle
;;;
;;;
;;; spherical -> cartesian
;;;   x     = r * sin(phi) * cos(theta)
;;;   y     = r * sin(phi) * sin(theta)
;;;   z     = r * cos(phi)
;;;
;;; cartesian -> spherical
;;;   r     = sqrt(x^2 + y^2 + z^2)  - magnitude
;;;   theta = arctan(y/x)            - inclination
;;;   phi   = arccos(z/r)            - azimuth
;;;

(defclass euclidean-vector (graphics-object)
  ((x
    :initarg :x
    :initform 0.0
    :type short-float
    :accessor point-x
    :documentation "The X-axis magnitude of the vector.")
   (y
    :initarg :y
    :initform 0.0
    :type short-float
    :accessor point-y
    :documentation "The Y-axis magnitude of the vector."))
  (:documentation "This class represents a 2-dimensional vector."))

;;;------------------------------------------------------------------
;;;{{{ Construction methods:

(defun make-euclidean-vector (&key x y angle magnitude (degrees nil))
  ;; If either x or y are provided, check they are numeric and, if the
  ;; corresponding value is not provided, default it to 0.
  (when x                               ; We have x?
    (or (typep x 'number)               ; Check the type
        (invalid-numeric-argument 'make-euclidean-vector x))
    (or y                               ; We have Y too?
        (setq y 0.0)))                  ; No? Make it 0.0.
  (when y
    (or (typep y 'number)
        (invalid-numeric-argument 'make-euclidean-vector y))
    (or x
        (setq x 0.0)))
  ;; Check magnitude and angle.
  (when magnitude
    (or (typep magnitude 'number)
        (invalid-numeric-argument 'make-euclidean-vector magnitude))
    (or angle
        (setq angle 0.0s0)))
  (when angle
    (or (typep angle 'number)
        (typep angle 'angle)
        (invalid-angular-argument 'make-euclidean-vector angle))
    (or magnitude
        (setq magnitude 0.0s0)))
  ;; If x and y are provided along with angle and magnitude, warn the
  ;; user.
  (when (and x y angle magnitude)
    (unless *suppress-graphics-warnings*
      (warn "MAKE-EUCLIDEAN-VECTOR had both X,Y coordinates and ~
             angle,magnitude both~%specified in arguments.  The X,Y ~
             coordinates will take precedence.")))
  ;; Build the vector.
  (let* ((phi (if (typep angle 'angle)
                              (slot-value angle 'radians)
                              (if degrees
                                  (radians<-degrees angle)
                                  angle)))
         (x (if x
                x
                (round-to (* magnitude (cos phi)) 4)))
         (y (if y
                y
                (round-to (* magnitude (sin phi)) 4))))
    #+debug
    (format *debug-io*
            "~&;; --> Creating a vector: X=~D, Y=~D~%~
             ~&;;                  from: r=~D, phi=~D"
            x
            y
            magnitude
            phi)
    (make-instance 'euclidean-vector
       :x (float x 1.0s0)
       :y (float y 1.0s0))))

(defun make-zero-euclidean-vector ()
  (make-instance 'euclidean-vector
     :x 0.0
     :y 0.0))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Utilities:

;; 2D vector  from  2D point
(defun vector<-point (point)
  (declare (type point point))
  (make-euclidean-vector :x (point-x point)
                         :y (point-y point)))

;; 2D point  from  2D vector
(defun point<-vector (vec2d)
  (declare (type euclidean-vector vec2d))
  (make-point :x (point-x vec2d)
              :Y (point-y vec2d)))
;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-euclidean-vector 'function
  "Creates a new 3D vector.

If `x', `y', and `z' are provided, then they are used as the distances
on the x, y, and z planes.

If `inclination', `azimuth', and `magnitude' are provided, then they are
used.

However, if both `x', `y', `z', `inclination', `azimuth', and
`magnitude' are provided, then `x', `y', and `z' take precedence.

If `degrees' is non-NIL and `inclination' and `azimuth' are given, then
the angles will be converted to radians.  Otherwise it is assumed that
`inclination' and `azimuth' contain radian values.")

(define-documentation 'make-zero-euclidean-vector 'function
  "Creates an empty vector.  All values default to 0.")

(define-documentation 'point<-vector 'function
  "Converts a vector to a point")

(define-documentation 'vector<-point 'function
  "Converts a point to a vector.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `line' class:

(defclass line (graphics-object/color
                graphics-object/drawable)
  ((start
    :initarg :start
    :initform (make-instance 'point
                 :x 0.0
                 :y 0.0)
    :type point
    :accessor line-start
    :documentation "The start point of the line segment on the
Cartesian plane.")
   (end
    :initarg :end
    :initform (make-instance 'point
                 :x 0.0
                 :y 0.0)
    :type point
    :accessor line-end
    :documentation "The end point of the line segment on the Cartesian
plane."))
  (:documentation "This class represents a 2-dimensional line
segment."))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-line (&key (start nil) (end nil)
                       (start-x 0.0s0) (start-y 0.0s0)
                       (end-x 0.0s0) (end-y 0.0s0)
                       (width 1.0s0))
  (declare (type number start-x start-y end-x end-y width))
  (let ((the-start (typecase start
                     (euclidean-vector ; Vector given for `start'.
                      (point<-vector start))
                     (point            ; Point given for `start'.
                      start)
                     (null             ; No object given for `start'.
                      (make-point :x (float start-x 1.0s0)
                                  :y (float start-y 1.0s0)))
                     (t                ; Something else given.
                      (error "Starting point not a point or vector ~
                              type!."))))
        (the-end (typecase end
                   (euclidean-vector ; vector given for `end'.
                    (point<-vector start))
                   (point            ; point given for `end'.
                    end)
                   (null             ; No object given for `end'.
                    (make-point :x (float end-x 1.0s0)
                                :y (float end-y 1.0s0)))
                   (t                ; Something else given.
                    (error "Ending point not a point or vector ~
                            type!")))))
    #+debug
    (format *debug-io* "~&;; --> Creating a line: (~D,~D)->(~D,~D)."
            (point-x the-start)
            (point-y the-start)
            (point-x the-end)
            (point-y the-end))
    (make-instance 'line
       :start the-start
       :end the-end
       :width (float width 1.0s0))))

(defun make-zero-line ()
  (make-instance 'line))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-line 'function
  "Creates a line from the given coordinates.

If `start' and `end' are non-NIL and are of either `point' or
`euclidean-vector' then those will be used to specify the start and end
coordinates of the line.

If `start' and `end' are NIL, then `start-x', `start-y', `end-x', and
`end-y' will be used to specify the start and end coordinates of the
line, otherwise the coordinates will default to 0.

If specified, `width' will give the width of the line, otherwise it
will default to a width of 1.0.")

(define-documentation 'make-zero-line 'function
  "Creates an empty line.  The coordinates default to 0.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `circle' class:

(defclass circle (graphics-object/color
                  graphics-object/drawable)
  ((origin
    :initarg :origin
    :initform (make-instance 'point)
    :type point
    :accessor circle-origin
    :documentation "The origin point on the Cartesian plane.")
   (radius
    :initarg :radius
    :initform 0.0
    :type short-float
    :accessor circle-radius
    :documentation "The radius of the circle.")
   (arc
    :initarg :arg
    :initform (make-angle :angle +tau+)
    :type angle
    :accessor circle-arc
    :documentation "The angle of the circular arc.")
   (arc-offset
    :initarg :arc-offset
    :initform (make-zero-angle)
    :type angle
    :accessor circle-arc-offset
    :documentation "The angular offset of the circular arc.")
   (type
    :initarg :type
    :initform :circle
    :accessor circle-type
    :documentation "The type of the circle."))
  (:documentation "This class represents a 2-dimensional circle."))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-circle (&key (origin nil) (origin-x 0.0s0) (origin-y 0.0s0)
                         (radius 1.0s0) (arc nil) (arc-offset nil)
                         (type :circle) (degrees nil))
  (declare (type number origin-x origin-y radius)
           (type (or null angle number) arc arc-offset))
  (when (not (member type *circle-types*))
    ;; ... invalid argument exception please.
    (error "Invalid argument ~S given, expecting one of ~{~S~^, ~}."
           type *circle-types*))
  (let ((the-origin (typecase origin
                      (euclidean-vector ; Vector given for `origin'.
                       (point<-vector origin))
                      (point            ; Point given for `origin'.
                       origin)
                      (null             ; No object given for `origin'.
                       (make-point :x (float origin-x 1.0s0)
                                   :y (float origin-y 1.0s0)))
                      (otherwise        ; Something else given.
                       (error "Origin point not a point type!"))))
        (the-arc (typecase arc
                   (angle               ; Angle given for `arc'.
                    arc)
                   (otherwise           ; No object given for `arc'.
                    (make-angle :angle (if arc
                                           arc
                                           (if degrees
                                               360.0s0
                                               +tau+)) 
                                :degrees degrees))))
        (the-arc-offset (typecase arc-offset
                          (angle        ; Angle given for `arc-offset'.
                           arc-offset)
                          (otherwise    ; No object given for `arc-offset'.
                           (make-angle :angle (if arc-offset
                                                  arc-offset
                                                  0.0s0)
                                       :degrees degrees)))))
    #+debug
    (format *debug-io*
            "~&;; --> Creating a circle: (~D,~D) r=~D, arc=~D, ~
             ~&;;                        offset=~D, type=~S"
            (point-x the-origin)
            (point-y the-origin)
            radius
            (degrees<-radians (slot-value the-arc 'radians))
            (degrees<-radians (slot-value the-arc-offset 'radians))
            type)
    (make-instance 'circle
       :origin the-origin
       :radius (float radius 1.0s0)
       :arc the-arc
       :arc-offset the-arc-offset
       :type type)))

(defun make-zero-circle ()
  (make-instance 'circle))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-circle 'function
  "Creates a circle from the given coordinates and dimensions.

If `origin' is non-NIL and is of either `point' or `euclidean-vector' then
that will specify the origin coordinates of the circle.

If `origin' is NIL, then `origin-x' and `origin-y' will be used to
specify the origin coordinates, otherwise the origin will default to 0.

If specified, `radius' will specify the radius of the circle, otherwise
it will default to a radius of 1.0.

If `arc' is non-NIL and of either `angle' or `number', then that will
specify the angle of the circle's arc, otherwise the arc angle will
default to 360 degrees (or tau).

If `arc-offset' is non-NIL and of either `angle' or `number', then that
will specify the offset to the starting point of the circle's arc
angle, otherwise the arc offset will default to 0.

If `type' is non-NIL and one of the values given in `*circle-types*',
then that will specify the type of circle that the renderer will draw.
It will default to `:circle'.

If `degrees' is non-NIL, then any numeric angle argument will be
treated as degrees rather than radians.  It should be noted that this
means that any angle values will be converted to radians.")

(define-documentation 'make-zero-circle 'function
  "Create an empty circle.  The coordinates and dimensions will default to
0.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `ellipse' class:

(defclass ellipse (circle)
  ((conjugate
    :initarg :conjugate
    :initform 0.0
    :type short-float
    :accessor ellipse-conjugate
    :documentation "The radius of the ellipse's conjugate."))
  (:documentation "This class represents a 2-dimensional ellipse."))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-elipse (&key (origin nil) (origin-x 0.0s0) (origin-y 0.0s0)
                          (radius 2.0s0) (arc nil) (arc-offset nil)
                          (conjugate 1.0s0) (type :ellipse)
                          (degrees nil))
  (declare (type number origin-x origin-y radius conjugate)
           (type (or null number angle arc arc-offset)))
  (when (not (member type *ellipse-types*))
    ;; ... invalid argument exception please.
    (error "Invalid argument ~S given, expecting one of ~{~S~^, ~}."
           type *ellipse-types*))
  (let ((the-origin (typecase origin
                      (euclidean-vector ; Vector given for `origin'.
                       (point<-vector origin))
                      (point            ; Point given for `origin'.
                       origin)
                      (otherwise        ; No object given for `origin'.
                       (make-point :x (float origin-x 1.0s0)
                                   :y (float origin-y 1.0s0)))))
        (the-arc (typecase arc
                   (angle               ; Angle given for `arc'.
                    arc)
                   (otherwise           ; no object given for `arc'.
                    (make-angle :angle (if arc
                                           (float arc)
                                           (if degrees
                                               360.0s0
                                               +tau+))
                                :degrees degrees))))
        (the-arc-offset (typecase arc-offset
                          (angle        ; Angle given for `arc-offset'.
                           arc-offset)
                          (otherwise    ; No object given for `arc-offset'.
                           (make-angle :angle (if arc-offset
                                                  (float arc-offset 1.0s0)
                                                  0.0s0)
                                       :degrees degrees)))))
    #+debug
    (format *debug-io*
            "~&;; --> Creating an ellipse: (~D,~D) r=~D, conjugate=~D~%~
             ~&;;                          arc=~D, offset=~D, type=~S"
            (point-x the-origin)
            (point-y the-origin)
            radius
            conjugate
            (degrees<-radians (slot-value the-arc 'radians))
            (degrees<-radians (slot-value the-arc 'radians))
            type)
    (make-instance 'ellipse
       :origin the-origin
       :radius (float radius 1.0s0)
       :conjugate (float conjugate 1.0s0)
       :arc the-arc
       :arc-offset the-arc-offset
       :type type)))

(defun make-zero-ellipse ()
  (make-instance 'ellipse))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-ellipse 'function
  "Creates a ellipse from the given coordinates and dimensions.

If `origin' is non-NIL and is of either `point' or `euclidean-vector' then
that will specify the origin coordinates of the ellipse.

If `origin' is NIL, then `origin-x' and `origin-y' will be used to
specify the origin coordinates, otherwise the origin will default to 0.

If specified, `radius' will specify the traverse radius of the ellipse,
otherwise it will default to a traverse radius of 2.0.

If specified, `conjugate' will specify the conjugate radius of the
ellipse, otherwise it will default to a conjugate radius of 1.0.

If `arc' is non-NIL and of either `angle' or `number', then that will
specify the angle of the circle's arc, otherwise the arc angle will
default to 360 degrees (or tau).

If `arc-offset' is non-NIL and of either `angle' or `number', then that
will specify the offset to the starting point of the circle's arc
angle, otherwise the arc offset will default to 0.

If `type' is non-NIL and one of the values given in `*ellipse-types*',
then that will specify the type of ellipse that the renderer will draw.
It will default to `:ellipse'.

If `degrees' is non-NIL, then any numeric angle argument will be
treated as degrees rather than radians.  It should be noted that this
means that any angle values will be converted to radians.")

(define-documentation 'make-zero-ellipse 'function
  "Creates an empty ellipse.  The coordinates and dimensions will
default to 0.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `polygon' class:

(defclass polygon (graphics-object/color
                   graphics-object/drawable)
  ((vertices
    :initarg :vertices
    :initform (make-vertex-array 1
                                    :adjustable nil
                                    :fill-pointer nil)
    :type array
    :accessor polygon-vertices
    :documentation "The array of vertices in the polygon."))
  (:documentation "This class represents a 2-dimensional n-vertex
polygon."))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-polygon (&key (points nil))
  (declare (type list points))
  (unless (= (length points) 0)
    (loop for point in points
          if (not (or (typep point 'point)
                      (typep point 'euclidean-vector)
                      (and (typep point 'cons)
                           (or (= (length point) 2)
                               (= (length point) 3)))))
            do (error "Point ~S is not a coordinate."
                      point)))
  (let ((points-array (if (= (length points) 0)
                          nil
                          (loop for point in points
                                collect (typecase point
                                          (point
                                           point)
                                          (euclidean-vector
                                           (point<-vector point))
                                          (cons
                                           (make-point
                                            :x (car point)
                                            :y (cadr point))))))))
    #+debug
    (format *debug-io* "~&;; --> Creating a polygon with ~D vertices."
            (length points))
    (make-instance 'polygon
       :vertices (make-array (length points)
                             :element-type 'point
                             :initial-contents points-array
                             :adjustable t
                             :fill-pointer t))))

(defun make-zero-polygon ()
  (make-instance 'polygon))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-polygon 'function
  "Creates a polygon from the given list of points.

If `points' is non-NIL and is a list, the polygon vertices will be
initialized to a list of points generated from the list.

Each point in the `points' list may be either a list of x and y
coordinates in the format of (X Y) or an instance of `point' or
`euclidean-vector'.

The resulting polygon contains an array of vertices that is adjustable
and has a fill pointer.")

(define-documentation 'make-zero-polygon 'function
  "Creates an empty polygon with no vertices.

The resulting polygon contains an array of vertices that is adjustable
and has a fill pointer.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `triangle' class:

(defclass triangle (polygon)
  ()
  (:default-initargs
   :vertices (make-vertex-array 3
                                   :adjustable nil
                                   :fill-pointer nil)))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-triangle (&key (point-a nil) (point-b nil) (point-c nil)
                           (point-ax 0.0s0) (point-ay 0.0s0)
                           (point-bx 0.0s0) (point-by 0.0s0)
                           (point-cx 0.0s0) (point-cy 0.0s0))
  (declare (type number point-ax point-bx point-cx point-ay point-by point-cy))
  (let ((the-point-a (typecase point-a
                       (euclidean-vector ; vector given for `point-a'.
                        (point<-vector point-a))
                       (point            ; point given for `point-a'.
                        point-a)
                       (null             ; No object given for `point-a'.
                        (make-point :x (float point-ax 1.0s0)
                                    :y (float point-ay 1.0s0)))
                       (t                ; Something else given.
                        (error "Point A not a point or vector type!"))))
        (the-point-b (typecase point-b
                       (euclidean-vector ; vector given for `point-b'.
                        (point<-vector point-b))
                       (point            ; point given for `point-b'.
                        point-b)
                       (null             ; No object given for `point-b'.
                        (make-point :x (float point-bx 1.0s0)
                                    :y (float point-by 1.0s0)))
                       (t                ; Something else given.
                        (error "Point B not a point or vector type!"))))
        (the-point-c (typecase point-c
                       (euclidean-vector ; vector given for `point-c'.
                        (point<-vector point-c))
                       (point            ; point given for `point-c'.
                        point-c)
                       (null             ; No object given for `point-c'.
                        (make-point :x (float point-cx 1.0s0)
                                    :y (float point-cy 1.0s0)))
                       (t                ; Something else given.
                        (error "Point C not a point or vector type!")))))
    #+debug
    (format *debug-io*
            "~&;; Creating a triangle: (~D,~D)-(~D,~D)-(~D,~D)."
            (point-x the-point-a)
            (point-y the-point-a)
            (point-x the-point-b)
            (point-y the-point-a)
            (point-x the-point-c)
            (point-y the-point-c))
    (make-instance 'triangle
       :vertices (make-array 3
                             :element-type 'point
                             :initial-contents (list the-point-a
                                                     the-point-b
                                                     the-point-c)
                             :adjustable nil
                             :fill-pointer nil))))

(defun make-zero-triangle ()
  (make-instance 'triangle))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Documentation:

(define-documentation 'make-triangle 'function
  "Creates a 2D triangle from the given coordinates.

If `point-a', `point-b', and `point-c' are non-NIL and of either
`point' or `euclidean-vector' then they will specify the coordinates of each
point of the triangle.

If `point-a', `point-b', or `point-c' are NIL, then `point-ax',
`point-ay' et al will specify the coordinates of each point of the
triangle.

The resulting polygon contains an array of vertices that is not
adjustable and has no fill pointer.")

(define-documentation 'make-zero-triangle 'function
  "Creates an empty 2D triangle with each point defaulting to 0.

The resulting polygon contains an array of vertices that is not
adjustable and has no fill pointer.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ The `rectangle' class:

(defclass rectangle (polygon)
  ()
  (:default-initargs
   :vertices (make-vertex-array 4)))

;;;------------------------------------------------------------------
;;;{{{ Construction functions:

(defun make-rectangle (&key (top-left nil) (bottom-right nil)
                            (top-left-x 0.0s0) (top-left-y 0.0s0)
                            (bottom-right-x 0.0s0)
                            (bottom-right-y 0.0s0))
  (declare (type number top-left-x top-left-y
                 bottom-right-x bottom-right-y))
  (let* ((start (typecase top-left
                  (euclidean-vector ; vector given for `top-left'.
                   (point<-vector top-left))
                  (point            ; point given for `top-left'.
                   top-left)
                  (null             ; No object given for `top-left'.
                   (make-point :x (float top-left-x 1.0s0)
                               :y (float top-left-y 1.0s0)))
                  (t                ; Something else given.
                   (error "Top left not a point type!"))))
         (end (typecase bottom-right
                (euclidean-vector ; vector given for `bottom-right'.
                 (point<-vector bottom-right))
                (point            ; point given for `bottom-right'.
                 bottom-right)
                (null             ; No object given for `bottom-right'.
                 (make-point :x (float bottom-right-x 1.0s0)
                             :y (float bottom-right-y 1.0s0)))
                (t                ; Something else given.
                 (error "Point B not a point type!"))))
         (start-opposite (make-point :x (point-x end)
                                     :y (point-y start)))
         (end-opposite (make-point :x (point-x start)
                                   :y (point-y end))))
    #+debug
    (format *debug-io*
            "~&;; --> Creating a rectangle: (~D,~D)-(~D,~D)."
            (point-x start)
            (point-y start)
            (point-x end)
            (point-y end))
    (make-instance 'rectangle
       :vertices (make-array 4
                             :element-type 'point
                             :initial-contents (list start
                                                     start-opposite
                                                     end
                                                     end-opposite)
                             :adjustable nil
                             :fill-pointer nil))))

(defun make-zero-rectangle ()
  (make-instance 'rectangle))

;;;}}}
;;;------------------------------------------------------------------

;;;..................................................................
;;;{{{ Minor:

(define-documentation 'make-rectangle 'function
  "Creates a 2D rectangle from the given coordinates.

If `top-left' or `top-right' are non-NIL and of either `point' or
`euclidean-vector' then they will be used to  specfy the top-left and
bottom-right coordinate of the rectangle bounds respectively.

If `top-left' or `top-right' are NIL, then `top-left-x', `top-left-y'
et al will be used to specify the top-left and bottom-right coordinates
of the rectangle bounds.

The resulting polygon contains an array of vertices that is not
adjustable and has no fill pointer.")

(define-documentation 'make-zero-rectangle 'function
  "Creates an empty 2D rectangle with all vertex coordinates defaulting
to 0.

The resulting polygon contains an array of vertices that is not
adjustable and has no fill pointer.")

;;;}}}
;;;..................................................................

;;;}}}
;;;==================================================================

;;; classes.lisp ends here
