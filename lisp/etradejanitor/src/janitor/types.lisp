(defpackage janitor/types
  (:use :cl)
  (:import-from :local-time #:timestamp= #:parse-timestring)
  (:import-from :janitor/common #:clet)
  (:import-from :org.mapcar.parse-number #:parse-number)
  (:export
    #:stockprice
    #:mk-stockprice
    #:stockprice-equals
    #:s-ticker
    #:s-dx
    #:s-opn
    #:s-hi
    #:s-lo
    #:s-cls
    #:s-vol))

(in-package :janitor/types)

(defstruct (stockprice (:conc-name s-)) (ticker 0 :type integer) dx opn hi lo cls vol)

(defun stockprice-equals (s1 s2)
  (and
    (equalp (s-ticker s1) (s-ticker s2))
    (timestamp= (s-dx s1) (s-dx s2))
    (equalp (s-opn s1) (s-opn s2))
    (equalp (s-hi s1) (s-hi s2))
    (equalp (s-lo s1) (s-lo s2))
    (equalp (s-cls s1) (s-cls s2))
    (equalp (s-vol s1) (s-vol s2))))

(defun csv->time (s)
  (parse-timestring (nth 0 (str:split " " s)) ))

(defun mk-stockprice (oid row)
  (clet
    (dx (csv->time (nth 0 row))
    opn (parse-number (nth 1 row))
    hi (parse-number (nth 2 row))
    lo (parse-number (nth 3 row))
    cls (parse-number (nth 4 row))
    vol (parse-number (nth 5 row)))
    (make-stockprice :ticker oid :dx dx :opn opn :hi hi :lo lo :cls cls :vol vol)))

; Date 0,Open 1,High 2,Low 3,Close 4,Volume 5,Dividends,Stock Splits

; (defparameter rt1
;   (mk-stockprice
;     '("2025-03-28 00:00:00+01:00" "317.1000061035156" "319.29998779296875" "311.8999938964844" "314.20001220703125" "497342" "0.0" "0.0")))

; (defparameter rt2
;   (mk-stockprice
;     '("2025-03-28 00:00:00+01:00" "317.1000061035156" "319.29998779296875" "311.8999938964844" "314.20001220703125" "497342" "0.0" "0.0")))
