(defpackage janitor/yahoo
  (:use :cl)
  (:import-from :janitor/common #:clet #:clet*)
  (:import-from :local-time #:today)
  (:import-from :janitor/common
    #:clet
    #:clet
    #:between
    #:diff-days)
  (:export
    #:yahoo-period))

(in-package :janitor/yahoo)

;(uiop:run-program (list cmd file-name) :output :string))

(defun yahoo-period (start-date &key (end-date nil))
  (clet*
    (dt (if (null end-date) (today) end-date)
     dd (diff-days start-date dt))
    (print dd)
    (cond
      ((between 0 1.0 dd :closed-end t) "1d")
      ((between 1.0 5.0 dd) "5d")
      ((between 5.0 29.0 dd) "1m")
      ((between 29.0 89.0 dd) "3m")
      ((between 89.0 180.0 dd) "6m")
      ((between 180.0 364.0 dd) "1y")
      ((between 364.0 729.0 dd) "2y")
      ((between 729.0 1824.0 dd) "5y")
      (t "10y"))))
