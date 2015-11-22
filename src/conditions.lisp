;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: SYSTEM.GRAPHICS; Base: 10; Lowercase: Yes -*-
;;;
;;; conditions.lisp --- Graphics system conditions.
;;;
;;; Time-stamp: <Sunday Nov 22, 2015 09:53:13 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Nov 2015 08:53:05
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

(define-condition invalid-numeric-argument (code-error)
  ((mesg
    :initform "Invalid numeric argument for `~S' given.

A ~A value was given, but a number was expected."
    :type simple-string
    :reader code-mesg)
   (args
    :initform nil
    :type symbol
    :reader code-args))
  (:documentation "An invalid numeric argument was provided.

For example, a string value was given when a numeric value was
expected.

If you are receiving errors of this condition, ensure you are using a
`number' value for the argument specified in the condition message."))

(defmacro invalid-numeric-argument (fn arg)
  "A convenience macro that signals an `invalid-numeric-argument'
condition to the user."
  `(error 'invalid-numeric-argument
          :proc ,(if fn
                     fn
                     'invalid-numeric-argument)
          :args (list ',arg (class-name (class-of ,arg)))))

(define-condition invalid-angular-argument (code-error)
  ((mesg
    :initform "Invalid angular argument for `~S' given.

A ~S value was given, but a number or angle was expected."
    :type simple-string
    :reader code-mesg)
   (args
    :initform nil
    :type symbol
    :reader code-args))
  (:documentation "An invalid angular argument was provided.

For example, a string value was given when a numeric or angle was
expected.

If you are receiving errors of this condition, ensure you are using
either a `number' or an `angle' value for the argument specified in
the condition message."))

(defmacro invalid-angular-argument (fn arg)
  "A convenience macro that signals an `invalid-angular-argument'
condition to the user."
  `(error 'invalid-angular-argument
          :proc ,(if fn
                     fn
                     'invalid-angular-argument)
          :args (list ',arg (class-name (class-of ,arg)))))

(define-condition invalid-vector-argument (code-error)
  ((mesg
    :initform "Invalid vector argument for `~S' given.

A ~A value was given, but a vector was expected."
    :type simple-string
    :reader code-mesg)
   (args
    :initform nil
    :type symbol
    :reader code-args))
  (:documentation "An invalid vector argument was provided.

For example, a string value was given when a vector value was
expected.

If you are receiving errors of this condition, ensure you are using an
`euclidean-vector' value for the argument specified in the condition
message."))

(defmacro invalid-vector-argument (fn arg)
  "A convenience macro that signals an `invalid-vector-argument'
condition to the user."
  `(error 'invalid-vector-argument
          :proc ,(if fn
                     fn
                     'invalid-vector-argument)
          :args (list ',arg (class-name (class-of ,arg)))))

;;; conditions.lisp ends here
