(defpackage janitor/db
  (:use :cl)
  (:import-from :janitor/common #:clet #:clet*)
  (:import-from :local-time
    #:parse-timestring
    #:timestamp+)
  (:import-from :postmodern
    #:connect-toplevel
    #:disconnect-toplevel
    #:query
    #:defprepared
    #:*database*)
  (:export
    #:ticker-dx))

(in-package :janitor/db)

(defun my-connect ()
  "Start the database connection."
  (unless *database*
    (connect-toplevel
      "trader" "trader" "ok" "172.20.1.7" :port 5432)))

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

(defun ticker-dx ()
  (my-connect)
  (clet*
    (items (db-ticker-dx)
    result (populate-ht items))
    result))

(defprepared test21 "select name from countries where id=$1")
