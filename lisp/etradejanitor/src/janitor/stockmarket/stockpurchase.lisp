(defpackage janitor/stockmarket/stockpurchase
  (:use :cl)
  (:export
    #:mk-stockpurchase
    #:p-ticker
    #:p-dx
    #:p-price
    #:p-vol))

(in-package :janitor/stockmarket/stockpurchase)

(defstruct (stockpurchase (:conc-name p-)) (ticker 0 :type integer) dx price vol)
