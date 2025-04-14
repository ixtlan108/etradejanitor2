(defpackage janitor/db
  (:use :cl)
  (:import-from :janitor/common
    #:iso-8601-string
    #:clet
    #:clet*)
  (:import-from :local-time
    #:parse-timestring
    #:timestamp+)
  (:import-from :janitor/types
    #:s-ticker
    #:s-dx
    #:s-opn
    #:s-hi
    #:s-lo
    #:s-cls
    #:s-vol)
  (:import-from :postmodern
    #:connect-toplevel
    #:disconnect-toplevel
    #:query
    #:defprepared
    #:with-transaction
    #:*database*)
  (:export
    #:ticker-dx
    #:insert-stockprice))

(in-package :janitor/db)

(defun my-connect ()
  "Start the database connection."
  (unless *database*
    (connect-toplevel
      "trader" "trader" "ok" "172.20.1.6" :port 5432)))

(defun my-disconnect()
  (print "Disconnecting from database...")
  (disconnect-toplevel))

(defun db-ticker-dx ()
  (query
    "select t.ticker,to_char(max(s.dx), 'yyyy-MM-dd') from stockmarket.stockprice s
    join stockmarket.stocktickers t on t.oid = s.ticker_id
    where t.status = 1
    group by t.ticker"))


(defun populate-ht (items)
  (clet (ht (make-hash-table :test 'equal))
    (loop for item in items
      do
        (clet*
          (ticker (nth 0 item)
          dx (nth 1 item)
          dxt (timestamp+ (parse-timestring dx) 1 :day))
            (setf (gethash ticker ht) dxt)))
    ht))

;(defun populate (items)

(defun ticker-dx ()
  (my-connect)
  (clet*
    (items (db-ticker-dx)
    result (populate-ht items))
    result))

(defprepared insert-stockprice-sql
  "insert into stockmarket.stockprice (ticker_id,dx,opn,hi,lo,cls,vol) values ($1,$2,$3,$4,$5,$6,$7)")

(defun insert-stockprice (rows)
  (my-connect)
  (with-transaction ()
    (loop :for r :across rows :do
      (clet
        (oid (s-ticker r)
        dx (iso-8601-string (s-dx r))
        opn (s-opn r)
        hi (s-hi r)
        lo (s-lo r)
        cls (s-cls r)
        vol (s-vol r))
        (funcall 'insert-stockprice-sql oid dx opn hi lo cls vol)))))
