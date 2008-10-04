i;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2007, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp. 

;;; File:       data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2007, AJ Rossini.  BSD, LLGPL, or GPLv2, depending
;;;             on how it arrives.  
;;; Purpose:    data package for lispstat
;;; Time-stamp: <2006-05-19 12:33:41 rossini> 
;;; Creation:   <2006-05-17 21:34:07 rossini> 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(defclass run-time-data ())

(defclass nonmem-data ()
  ((store) (advan-code) (model-code) (dosing-code))
  (:documentation "")
  )

(defclass nonmem-result (nonmem-data run-time-data)

  )