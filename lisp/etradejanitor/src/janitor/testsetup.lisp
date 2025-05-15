(defpackage janitor/testsetup
  (:use :cl)
  (:local-nicknames
    (#:sprice #:janitor/stockmarket/stockprice)
    (#:rutil #:janitor/redisutil)))

(in-package :janitor/testsetup)

(defun opening-prices ()
  (let ((yar (sprice:make-stockprice :ticker 3 :opn 403.0)))
    (rutil:opening-price-spots (list yar) :db 6)))

(defun nordnet-expiries ()
  (let ((millis
    (list 
      1672527600000
      1704841200000
      1706742000000
      1718920800000
      1773961200000)))  ;<-- "Valid for test_parse_real_time"
    (rutil:nordnet-expiries millis :db 6)))
