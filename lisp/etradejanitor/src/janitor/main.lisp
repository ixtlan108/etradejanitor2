(defpackage janitor/main
  (:use :cl)
  (:import-from :janitor/common
    #:clet
    #:clet*
    #:>0)
  (:local-nicknames
    (#:pa #:janitor/parser))
  (:export
    #:run))

(in-package :janitor/main)

(defun process-ticker (ticker ht)
  (clet
    (cur-dx (gethash ticker ht))
    (when cur-dx
      (clet (items (pa:parse ticker))
        (when items
          (clet (cut-offs (pa:cut-off items cur-dx))
            (when (>0 cut-offs)
              (print cut-offs)
              (print "success!"))))))))

(defun run-2 (tdx)
  (process-ticker "YAR" tdx))

(defun run ()
  (run-2 (janitor/db:ticker-dx)))
