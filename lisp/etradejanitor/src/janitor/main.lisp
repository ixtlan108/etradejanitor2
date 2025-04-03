(defpackage janitor/main
  (:use :cl)
  (:import-from :janitor/common #:clet #:clet*)
  (:export
    #:run))

(in-package :janitor/main)

(defun run ()
  (clet*
    (tdx (janitor/db:ticker-dx)
      cur-dx (gethash "YAR" tdx)
      items (janitor/parser:parse-cut-off "YAR" cur-dx))
    items))
