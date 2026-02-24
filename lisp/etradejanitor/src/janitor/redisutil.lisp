(defpackage janitor/redisutil
  (:use :cl)
  (:import-from :janitor/stockmarket/util
     #:ticker-oid-ht)
  (:import-from :serapeum #:->)
  (:import-from :janitor/stockmarket/stockprice
     #:stockprice)
  (:local-nicknames
    (#:sprice #:janitor/stockmarket/stockprice)
    (#:pa #:janitor/parser))
  (:export
    #:save-stockprices
    #:nordnet-expiries
    #:opening-price-test 
    #:opening-price-spots))

(in-package :janitor/redisutil)

(defvar redis-host "172.20.1.2")

(deftype redis-db-enum () '(member 0 6))

(defun t-oid (ticker)
  (gethash ticker ticker-oid-ht))

(defun nordnet-expiry (expiry)
  (redis:red-sadd "expiry" expiry))

(defun nordnet-expiries (items &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (expiry items)
      (nordnet-expiry expiry))))


; (defun spot-key (price)
;   (let ((oid (sprice:s-ticker price)))
;     (format nil "spot:~a" oid)))

(-> save-stockprice-value (stockprice string t))
(defun save-stockprice-value (price redis-key prop-fn)
  (let ((value (funcall prop-fn price))
        (oid (sprice:s-ticker price)))
    (redis:red-hset redis-key oid value)))

; (declaim (ftype (function (stockprice boolean) t) save-stockprice))

(-> save-stockprice (stockprice boolean))
(defun save-stockprice (price save-open)
  (when save-open
    (save-stockprice-value price "stockprice:open" #'sprice:s-opn))
  (save-stockprice-value price "stockprice:hi" #'sprice:s-hi)
  (save-stockprice-value price "stockprice:lo" #'sprice:s-lo)
  (save-stockprice-value price "stockprice:close" #'sprice:s-cls)
  (save-stockprice-value price "stockprice:vol" #'sprice:s-vol))
  
;(list o hi lo cls vol k)))
;(redis:red-rpush )))

;(declaim (ftype (function (list redis-db-enum boolean) t) save-stockprices))

(-> save-stockprices (list redis-db boolean))
(defun save-stockprices (prices db save-open)
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (price prices)
      (save-stockprice price save-open))))

;(declaim (ftype (function (integer redis-db-enum (function (integer) integer)) t) opening-price-spots))
;(declaim (ftype (function (list redis-db-enum) t) opening-price-spots))

(-> opening-price-spots (list redis-db-enum))
(defun opening-price-spots (spots redis-db)
  (redis:with-connection (:host redis-host)
    (redis:red-select redis-db)
    (dolist (spot spots)
      (save-stockprice-value spot "stockprice:open" #'sprice:s-opn))))

(defun opening-price-test ()
  (redis:with-connection (:host redis-host)
    (redis:red-select 6)
    (redis:red-hset "stockprice:open" 3 462.7)))

; (defun seconds-p (s)
;   (<= 0 s 59))
;
; (deftype seconds ()
;   '(and fixnum (satisfies seconds-p)))
;
; (-> sec (seconds seconds) seconds)
; (defun sec (a b) 
;   (+ a b))


;(defparameter x (opening-price-spots 3 0))


; (defun opening-price-ticker (ticker)
;   (let ((spot (pa:parse-spot ticker)))
;     (when spot
;       (opening-price-spot spot))))
;
; (defun opening-prices (tickers &key (db 0))
;   (redis:with-connection (:host redis-host)
;     (redis:red-select db)
;     (dolist (ticker tickers)
;       (opening-price-ticker ticker))))
;


