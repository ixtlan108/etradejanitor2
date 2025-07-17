(defpackage janitor/db
  (:use :cl)
  (:import-from :janitor/common
    #:iso-8601-string)
  (:import-from :local-time
    #:parse-timestring
    #:timestamp+)
  (:import-from :janitor/stockmarket/stockprice
    #:s-ticker
    #:s-dx
    #:s-opn
    #:s-hi
    #:s-lo
    #:s-cls
    #:s-vol)
  (:import-from :janitor/stockmarket/stockpurchase
    #:p-ticker
    #:p-dx
    #:p-price
    #:p-vol)
  (:import-from :postmodern
    #:*database*)
  (:local-nicknames
    (#:pm #:postmodern))
  (:export
    #:current-migration
    #:current-migration-info 
    #:insert-migration-version 
    #:ticker-dx
    #:latest-dx
    #:insert-stockprice
    #:insert-stockpurchase))

(in-package :janitor/db)

;(defparameter host "172.20.1.6")
;(defparameter host "172.20.1.7")
;(defparameter host "localhost")
(defparameter host-prod "172.20.1.6")
(defparameter host-atest "172.20.1.7")

(defun conn-param (profile)
  (if (eq profile :prod)
    (list "trader" "trader" "ok" host-prod :port 5432 :pooled-p t)
    (list "trader" "trader" "ok" host-atest :port 5432 :pooled-p t)))

; (defun my-connect ()
;   "Start the database connection."
;   (unless *database*
;     (pm:connect-toplevel
;       "trader" "trader" "ok" host :port 5432)))

; (defun my-disconnect()
;   (print "Disconnecting from database...")
;   (pm:disconnect-toplevel))

(defun db-ticker-dx ()
  (pm:query
    "select t.ticker,to_char(max(s.dx), 'yyyy-MM-dd') from stockmarket.stockprice s
    join stockmarket.stocktickers t on t.oid = s.ticker_id
    where t.status = 1
    group by t.ticker"))


(defun populate-ht (items inc-date)
  (let ((ht (make-hash-table :test 'equal)))
    (loop for item in items
      do
        (let*
          ((ticker (nth 0 item))
           (dx (nth 1 item))
           (parsed-dx (parse-timestring dx))
           (dxt (if inc-date 
                (timestamp+ parsed-dx 1 :day)
                parsed-dx)))
            (setf (gethash ticker ht) dxt)))
    ht))

(defun ticker-dx-internal (profile inc-date)
  (pm:with-connection (conn-param profile)
    (let*
      ((items (db-ticker-dx))
       (result (populate-ht items inc-date)))
      result)))

(defun ticker-dx (profile)
  (ticker-dx-internal profile t))

(defun latest-dx (profile)
  (ticker-dx-internal profile nil))

(pm:defprepared insert-stockprice-sql
  "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values ($1,$2,$3,$4,$5,$6,$7)")

(defun insert-stockprice (profile rows)
  (pm:with-connection (conn-param profile)
    (pm:with-transaction ()
      (loop :for r :across rows :do
        (let
          ((oid (s-ticker r))
           (dx (iso-8601-string (s-dx r)))
           (opn (s-opn r))
           (hi (s-hi r))
           (lo (s-lo r))
           (cls (s-cls r))
           (vol (s-vol r)))
          (funcall 'insert-stockprice-sql oid dx opn hi lo cls vol))))))

(pm:defprepared insert-stockpurchase-sql
  "insert into stockmarket.stock_purchase (ticker_id,unix_time,price,volume) values ($1,$2,$3,$4)")

(defun insert-stockpurchase (profile p)
  (pm:with-connection (conn-param profile)
    (pm:with-transaction ()
      (let ((oid (p-ticker p))
            (dx (iso-8601-string (p-dx p)))
            (price (p-price p))
            (vol (p-vol p)))
      (funcall 'insert-stockpurchase-sql oid dx price vol)))))

(pm:defprepared current-migration-sql
  "select max(version) from stockmarket.migrations")

(defun current-migration (profile)
  (pm:with-connection (conn-param profile)
    (pm:with-transaction ()
      (funcall 'current-migration-sql))))

(pm:defprepared current-migration-info-sql
  "select * from stockmarket.migrations where version = (select max(version) from stockmarket.migrations)")

(defun current-migration-info (profile)
  (pm:with-connection (conn-param profile)
    (pm:with-transaction ()
      (funcall 'current-migration-info-sql))))

(pm:defprepared insert-migration-sql
  "insert into stockmarket.migrations (version,comment) values ($1,$2)")


(defun insert-migration-version (profile unix-time comment sql-file)
  (pm:with-connection (conn-param profile)
    (pm:with-transaction ()
      (pm:execute-file sql-file t))))

      ;(funcall 'insert-migration-sql unix-time comment))))
