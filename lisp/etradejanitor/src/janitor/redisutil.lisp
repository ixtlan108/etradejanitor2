(defpackage janitor/redisutil
  (:use :cl)
  (:import-from :janitor/stockmarket/util
    #:ticker-oid-ht)
  (:local-nicknames
    (#:sprice #:janitor/stockmarket/stockprice)
    (#:pa #:janitor/parser))
  (:export
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

(defun opening-price-spot (spot)
  (let ((opn (sprice:s-opn spot))
        (oid (sprice:s-ticker spot)))
    (redis:red-hset "openingprices" oid opn)))

(defun opening-price-spots (spots &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (spot spots)
      (opening-price-spot spot))))

(defun opening-price-ticker (ticker)
  (let ((spot (pa:parse-spot ticker)))
    (when spot
      (opening-price-spot spot))))

(defun opening-prices (tickers &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (ticker tickers)
      (opening-price-ticker ticker))))

(defun spot-key (price)
  (let ((oid (sprice:s-ticker price)))
    (format nil "spot:~a" oid)))

(defun save-stockprice (price)
  (let ((o (sprice:s-opn price))
        (hi (sprice:s-hi price))
        (lo (sprice:s-lo price))
        (cls (sprice:s-cls price))
        (vol (sprice:s-vol price))
        ;(dx (sprice:s-dx price))
        (k (spot-key price)))
  (list o hi lo cls vol k)))
   ;(redis:red-rpush )))

(defun save-stockprices (prices &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (price prices)
      (save-stockprice price))))
