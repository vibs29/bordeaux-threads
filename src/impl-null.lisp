;;;; -*- indent-tabs-mode: nil -*-

(in-package #:bordeaux-threads)

(defun current-thread ()
  nil)

(defun thread-name (thread)
  (declare (ignore thread))
  "Main thread")

(defun acquire-lock (lock &optional wait-p)
  (declare (ignore lock wait-p))
  t)

(defun release-lock (lock)
  (declare (ignore lock))
  (values))

(defun acquire-recursive-lock (lock)
  (declare (ignore lock))
  t)

(defun release-recursive-lock (lock)
  (declare (ignore lock))
  (values))
