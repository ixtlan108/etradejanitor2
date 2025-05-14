(defpackage janitor/redisutil
  (:use :cl)
  (:import-from :janitor/stockmarket/util
    #:ticker-oid-ht)
  (:local-nicknames
    (#:sprice #:janitor/stockmarket/stockprice)
    (#:pa #:janitor/parser))
  (:export
    #:save-stockprices
    #:nordnet-expiries
    #:opening-price-spots
    #:opening-prices))

(in-package :janitor/redisutil)

(defvar redis-host "172.20.1.2")

(defun t-oid (ticker)
  (gethash ticker ticker-oid-ht))

(defun nordnet-expiry (expiry)
  (redis:red-sadd "expiry" expiry))

(defun nordnet-expiries (items &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (expiry items)
      (nordnet-expiry expiry))))



(defun opening-price-ticker (ticker)
  (let ((spot (pa:parse-spot ticker)))
    (when spot
      (opening-price-spot spot))))

(defun opening-prices (tickers &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (ticker tickers)
      (opening-price-ticker ticker))))

; (defun spot-key (price)
;   (let ((oid (sprice:s-ticker price)))
;     (format nil "spot:~a" oid)))

(defun save-stockprice-value (price redis-key prop-fn)
  (let ((value (funcall prop-fn price))
        (oid (sprice:s-ticker price)))
    (redis:red-hset redis-key oid value)))

(defun save-stockprice (price save-open)
  (when save-open
    (save-stockprice-value price "stockprice:open" #'sprice:s-opn))
  (save-stockprice-value price "stockprice:hi" #'sprice:s-hi)
  (save-stockprice-value price "stockprice:lo" #'sprice:s-lo)
  (save-stockprice-value price "stockprice:close" #'sprice:s-cls)
  (save-stockprice-value price "stockprice:vol" #'sprice:s-vol))
  
;(list o hi lo cls vol k)))
;(redis:red-rpush )))

(defun save-stockprices (prices db save-open)
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (price prices)
      (save-stockprice price save-open))))

(defun opening-price-spots (spots &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (spot spots)
      (save-stockprice-value spot "stockprice:open" #'sprice:s-opn))))





