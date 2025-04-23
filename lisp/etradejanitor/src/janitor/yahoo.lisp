(defpackage janitor/yahoo
  (:use :cl)
  (:import-from :janitor/common #:clet #:clet*)
  (:import-from :local-time #:today)
  (:import-from :janitor/common
    #:between
    #:diff-days)
  (:export
    #:download-tickers
    #:download-spot
    #:yahoo-period))

(in-package :janitor/yahoo)


(defun yahoo-period (start-date &key (end-date nil))
  (let
    ((dt (if (null end-date) (today) end-date))
     (dd (diff-days start-date dt)))
    (cond
      ((between 0 1.0 dd) "1d")
      ((between 1.0 5.0 dd) "5d")
      ((between 5.0 29.0 dd) "1mo")
      ((between 29.0 89.0 dd) "3mo")
      ((between 89.0 180.0 dd) "6mo")
      ((between 180.0 365.0 dd) "1y")
      ((between 365.0 729.0 dd) "2y")
      ((between 729.0 1824.0 dd) "5y")
      (t "na"))))

(defvar yahoo-python (format nil "~a/python/yahoo.py" janitor/common:*home*))

(defvar python3 "/usr/bin/python3")
;(defvar python3 "/opt/homebrew/bin/python3")

(defun download-ticker (ht-tdx ticker)
  (let
    ((cur-dx (gethash ticker ht-tdx)))
    (if cur-dx
      (let ((yp (yahoo-period cur-dx)))
        (format t "Ticker: ~a, period: ~a~%" ticker yp)
        (finish-output)
        (unless (equal "na" yp)
          (uiop:run-program (list python3 yahoo-python "-t" ticker "-p" yp) :output :string))))))

(defun download-tickers (ht-tdx tickers)
  (dolist (ticker tickers)
    (download-ticker ht-tdx ticker)))

(defun download-spot (ticker)
  (uiop:run-program (list python3 yahoo-python "-t" ticker "-x") :output :string))
