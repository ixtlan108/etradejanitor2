(defpackage janitor/common
  (:use :cl)
  (:export
    #:clet
    #:clet*
    #:read-csv
    #:timestamp
    #:^))

(in-package :janitor/common)

(defun timestamp (year month day)
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defmacro fn (&rest forms)
  `(lambda ,@forms))

(defmacro clet (bindings &body body)
  `(let ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))

(defmacro clet* (bindings &body body)
  `(let* ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))

(defun read-csv (csv-file &key (keep-header nil))
  (clet* (pn (pathname csv-file)
          items (cl-csv:read-csv pn :separator ","))
    (if keep-header
      items
      (rest items))))
