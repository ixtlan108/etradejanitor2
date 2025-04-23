(defpackage janitor/main
  (:use :cl)
  (:import-from :local-time #:timestamp<)
  (:import-from :janitor/stockmarket/stockprice
    #:s-dx
    #:s-opn)
  (:import-from :janitor/common
    #:cache
    #:clet
    #:clet*
    #:>0
    #:partial
    #:fn
    #:vec-last
    #:iso-8601-string)
  (:import-from :trivia #:match)
  (:local-nicknames
    (#:db #:janitor/db)
    (#:yahoo #:janitor/yahoo)
    (#:pa #:janitor/parser))
  (:export
    #:run))

(in-package :janitor/main)

(defvar ticker-oid-ht
  (clet (ht (make-hash-table :test 'equal))
    (setf (gethash "NHY" ht) 1)
    (setf (gethash "EQNR" ht) 2)
    (setf (gethash "YAR" ht) 3)
    (setf (gethash "SDRL" ht) 4)
    (setf (gethash "ACY" ht) 5)
    (setf (gethash "TEL" ht) 6)
    (setf (gethash "OBX" ht) 7)
    (setf (gethash "MHG" ht) 8)
    (setf (gethash "ORK" ht) 9)
    (setf (gethash "DNBNOR" ht) 10)
    (setf (gethash "REC" ht) 11)
    (setf (gethash "PGS" ht) 12)
    (setf (gethash "RCL" ht) 13)
    (setf (gethash "STB" ht) 14)
    (setf (gethash "TAA" ht) 15)
    (setf (gethash "TGS" ht) 16)
    (setf (gethash "TOM" ht) 17)
    (setf (gethash "AKSO" ht) 18)
    (setf (gethash "DNB" ht) 19)
    (setf (gethash "DNO" ht) 20)
    (setf (gethash "GJF" ht) 21)
    (setf (gethash "NSG" ht) 22)
    (setf (gethash "SUBC" ht) 23)
    (setf (gethash "OSEBX" ht) 24)
    (setf (gethash "AKERBP" ht) 25)
    (setf (gethash "BWLPG" ht) 26)
    (setf (gethash "BAKKA" ht) 27)
    (setf (gethash "GOGL" ht) 28)
    (setf (gethash "NAS" ht) 29)
    ht))

(defparameter tier-1
  '("NHY" "STB" "YAR"))

; 1, 3, 14

(defparameter tier-2
  '("AKSO" "DNB" "GJF" "ORK" "TOM"))

; 18,19,21,9,17

(defparameter tier-3
  '("BAKKA" "BWLPG" "DNO" "GOGL" "NAS" "SUBC" "TGS"))

(defparameter tier-all
  (concatenate 'list tier-1 tier-2 tier-3))

; 27,26,20,28,29,23,16

;(defvar tickers-all ())

(defparameter *cfg*
  (list :skip-db nil :invalidate-tdx nil :skip-validate nil))

(defun prn-cfg ()
  (format t "~{~a ~}~%" *cfg*))
  ;(format t "~{~a~^, ~}" *cfg*))

(defun cfg-get (key)
  (getf *cfg* key))

(defun validate-cut-offs (co dx)
  (if (cfg-get :skip-validate)
    (list :status :ok :result co)
    (progn
      (let
        ((start-date (s-dx (vec-last co))))
          (if (timestamp< dx start-date)
            (list :status :start-date-error
                  :err (format nil "Start date: ~a is greater than cut-off date: ~a" start-date dx))
            (list :status :ok :result co))))))

(defun print-status (cut-offs)
  (dolist (c cut-offs)
    (let ((s (getf c :status)))
      (cond
        ((equal s :ok)
          (let* ((rows (getf c :rows))
                (ed (elt rows 0))
                (sd (vec-last rows)))
            (princ (format nil "ticker: ~a => status: ~a, num items: ~a, start: ~a, end:~a~%"
                    (getf c :ticker) (getf c :status) (getf c :len) (iso-8601-string (s-dx sd)) (iso-8601-string (s-dx ed))))))
        (t
          (if (getf c :err)
            (princ (format nil "ticker: ~a => status: ~a, err: ~a~%" (getf c :ticker) (getf c :status) (getf c :err)))
            (princ (format nil "ticker: ~a => status: ~a~%" (getf c :ticker) (getf c :status)))))))))

(defun parse-ticker (ticker ht)
  (let ((cur-dx (gethash ticker ht)))
    (if cur-dx
      (let
        ((oid (gethash ticker ticker-oid-ht))
         (items (pa:parse ticker)))
        (if items
          (let ((cut-offs (pa:cut-off items oid cur-dx)))
            (if (>0 cut-offs)
              (validate-cut-offs cut-offs cur-dx)
              (list :status :empty-cutoffs)))
          (list :status :missing-csv)))
      (list :status :empty-dx))))

(defun parse-tickers (ht-tdx tickers)
  (let ((remaining '()))
    (dolist (ticker tickers)
      (clet (m (parse-ticker ticker ht-tdx))
        (match m
          ((list :status :empty-cutoffs)
            (push (list :status :empty-cutoffs :ticker ticker) remaining))
          ((list :status :empty-items)
            (push (list :status :empty-items :ticker ticker) remaining))
          ((list :status :empty-dx)
            (push (list :status :empty-dx :ticker ticker) remaining))
          ((list :status :ok :result x)
            (push (list :status :ok :ticker ticker :len (length x) :rows x) remaining))
          ((list :status :missing-csv)
            (push (list :status :missing-csv :ticker ticker) remaining))
          ((list :status :start-date-error :err error)
            (push (list :status :start-date-error :ticker ticker :err error) remaining))
          (_
            (print m)
            (push (list :status :unknown :ticker ticker) remaining)))))
    remaining))

(defparameter tdx (cache #'db:ticker-dx))

(defun process-db-tickers (tix)
  (dolist (ticker tix)
    (when (equal (getf ticker :status) :ok)
      (db:insert-stockprice (getf ticker :rows)))))

(defun run (tickers)
  (let ((tix (parse-tickers (funcall tdx) tickers)))
    (print-status tix)
    (unless (cfg-get :skip-db)
      (print "Inserting prices into database...")
      (process-db-tickers tix))
    tix))

(defun run-tier-n (tier)
  (prn-cfg)
  (if (cfg-get :invalidate-tdx)
    (funcall tdx :invalidate t))
  (run tier))

(defun config-cfg (skip-db invalidate-tdx skip-validate)
  (list
    :skip-db skip-db
    :invalidate-tdx invalidate-tdx
    :skip-validate skip-validate))

(defun run-tier-1 (&key (s-db nil) (i-tdx nil) (s-val nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val)))
    (run-tier-n tier-1)))

(defun run-tier-2 (&key (s-db nil) (i-tdx nil) (s-val nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val)))
    (run-tier-n tier-2)))

(defun run-tier-3 (&key (s-db nil) (i-tdx nil) (s-val nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val)))
    (run-tier-n tier-3)))

(defun download-tickers (tix)
  (yahoo:download-tickers (funcall tdx) tix))

(defun download-all (&key (i-tdx nil))
  (if i-tdx
    (funcall tdx :invalidate t))
  (yahoo:download-tickers (funcall tdx) tier-all))

; (redis:with-connection (:host "172.20.1.2") (redis:red-select 4) (redis:red-hset "openingprice" "YAX" "34.34"))

(defvar redis-host "172.20.1.2")

(defun opening-price (ticker)
  (let ((spot (pa:parse-spot ticker)))
    (when spot
      (let ((opn (s-opn spot)))
        (redis:red-hset "openingprices" ticker opn)))))

(defun opening-prices (tickers &key (db 0))
  (redis:with-connection (:host redis-host)
    (redis:red-select db)
    (dolist (ticker tickers)
      (opening-price ticker))))

(defun spot (ticker)
  (yahoo:download-spot ticker))

(defun spots (tickers)
  (dolist (ticker tickers)
    (spot ticker)))

(defun spots-tier-1 ()
  (spots tier-1))

(defun spots-tier-2 ()
  (spots tier-2))

(defun spots-tier-3 ()
  (spots tier-3))

(defun spots-all ()
  (spots tier-all))

; (defun spot (ticker &key (redis nil) (db 0))
;   (yahoo:download-spot ticker)
;   (if redis
;     (redis:with-connection (:host redis-host)
;       (redis:red-select db)
;       (opening-price ticker db))))

;select t.oid,t.ticker,max(p.dx) from stockmarket.stockprice p join stockmarket.stocktickers t on t.oid = p.ticker_id where t.oid in (1,3) group by t.oid,t.ticker;

; (defun match-demo (m)
;   (match m
;     ((list :status :one ) :one)
;     ((list :status :two :result x) x)
;     (_ :three)))
