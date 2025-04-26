(defpackage janitor/stockmarket/stockpurchase
  (:use :cl)
  (:import-from :janitor/stockmarket/util
    #:ticker-oid-ht)
  (:export
    #:mk-stockpurchase
    #:p-ticker
    #:p-dx
    #:p-price
    #:p-vol
    #:new-stockpurchase))

(in-package :janitor/stockmarket/stockpurchase)

(defstruct (stockpurchase (:conc-name p-)) (ticker 0 :type integer) dx price vol)

(defun new-stockpurchase (ticker price volume &key dx))
