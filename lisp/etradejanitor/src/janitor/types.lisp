(defpackage janitor/types
  (:use :cl)
  (:export
    #:stockprice
    #:make-stockprice
    #:s-opn
    #:s-hi
    #:s-lo
    #:s-cls
    #:s-vol))


(in-package :janitor/types)

(defstruct (stockprice (:conc-name s-)) opn hi lo cls vol)

(defun demo () (make-stockprice :opn 10 :hi 12 :lo 8 :cls 9 :vol 1000))
