(defpackage janitor/main
  (:use :cl)
  (:import-from :local-time #:timestamp<)
  (:import-from :janitor/stockmarket/stockprice
    #:s-dx
    #:s-opn)
  (:import-from :janitor/common #:>0)
  (:import-from :trivia #:match)
  (:import-from :janitor/stockmarket/util
    #:ticker-oid-ht)
  (:local-nicknames
    (#:db #:janitor/db)
    (#:co #:janitor/common)
    (#:rutil #:janitor/redisutil)
    (#:yahoo #:janitor/yahoo)
    (#:pa #:janitor/parser))
  (:export
    #:run))

(in-package :janitor/main)

(defparameter tier-1
  (list "NHY" "STB" "YAR"))

; 1, 3, 14

(defparameter tier-2
  (list "AKSO" "DNB" "ORK" "TOM"))
;(list "AKSO" "DNB" "GJF" "ORK" "TOM"))

; 18,19,21,9,17

(defparameter tier-3
  (list "BAKKA" "BWLPG" "GOGL" "NAS" "SUBC" "TGS"))
  
;(list "BAKKA" "BWLPG" "DNO" "GOGL" "NAS" "SUBC" "TGS"))

(defparameter tier-all
  (concatenate 'list tier-1 tier-2 tier-3))

; 27,26,20,28,29,23,16

;(defvar tickers-all ())

(defparameter *profile* :atest)

(defparameter *cfg*
  (list 
    :skip-db nil 
    :invalidate-tdx nil 
    :skip-validate nil 
    :skip-today nil 
    :profile :atest))

(defun prn-cfg ()
  (format t "~{~a ~}~%" *cfg*))
  ;(format t "~{~a~^, ~}" *cfg*))

(defun cfg-get (key)
  (getf *cfg* key))

(defun validate-cut-offs (co dx)
  (if (cfg-get :skip-validate)
    (list :status :ok :result co)
    (let
      ((start-date (s-dx (co:vec-last co))))
        (if (timestamp< dx start-date)
          (list :status :start-date-error
                :err (format nil "Start date: ~a is greater than cut-off date: ~a" start-date dx))
          (list :status :ok :result co)))))

(defun print-status (cut-offs) 
  "cut-offs <- parse-tickers"
  (dolist (c cut-offs)
    (let ((s (getf c :status)))
      (cond
        ((equal s :ok)
          (let* ((rows (getf c :rows))
                (ed (elt rows 0))
                (sd (co:vec-last rows)))
            (princ (format nil "ticker: ~a => status: ~a, num items: ~a, start: ~a, end:~a~%"
                    (getf c :ticker) (getf c :status) (getf c :len) (co:iso-8601-string (s-dx sd)) (co:iso-8601-string (s-dx ed))))))
        (t
          (if (getf c :err)
            (princ (format nil "ticker: ~a => status: ~a, err: ~a~%" (getf c :ticker) (getf c :status) (getf c :err)))
            (princ (format nil "ticker: ~a => status: ~a~%" (getf c :ticker) (getf c :status)))))))))

(defun parse-ticker (ticker ht &key (skip-today))
  (let ((cur-dx (gethash ticker ht)))
    (if cur-dx
      (let
        ((oid (gethash ticker ticker-oid-ht))
         (items (pa:parse ticker)))
        (if items
          (let ((itemsx (if skip-today (rest items) items)))
            (let ((cut-offs (pa:cut-off itemsx oid cur-dx)))
              (if (>0 cut-offs)
                (validate-cut-offs cut-offs cur-dx)
                (list :status :empty-cutoffs))))
          (list :status :missing-csv)))
      (list :status :empty-dx))))

(defun parse-tickers (ht-tdx tickers skip-today)
  (let ((remaining '()))
    (dolist (ticker tickers)
      (let ((m (parse-ticker ticker ht-tdx :skip-today skip-today)))
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

(defparameter tdx-prod (co:cache-2 #'db:ticker-dx :prod))
(defparameter tdx-atest (co:cache-2 #'db:ticker-dx :atest))
;(defparameter *tdx* tdx-prod)

(defun get-tdx ()
  (format t "Current profile: ~a~%" *profile*)
  (if (eq *profile* :prod)
    tdx-prod
    tdx-atest))


(defun process-db-tickers (tix)
  (dolist (ticker tix)
    (when (equal (getf ticker :status) :ok)
      (db:insert-stockprice *profile* (getf ticker :rows))))
  (funcall (get-tdx) :invalidate t))

(defun run (tickers)
  (let ((tix (parse-tickers (funcall (get-tdx)) tickers (cfg-get :skip-today))))
    (print-status tix)
    (unless (cfg-get :skip-db)
      (print "Inserting prices into database...")
      (process-db-tickers tix))
    tix))

(defun run-tier-n (tier)
  (prn-cfg)
  (if (cfg-get :invalidate-tdx)
    (funcall (get-tdx) :invalidate t))
  (run tier))

(defun config-cfg (skip-db 
                    invalidate-tdx 
                    skip-validate 
                    skip-today)
  (list
    :skip-db skip-db
    :invalidate-tdx invalidate-tdx
    :skip-validate skip-validate
    :skip-today skip-today))

(defun run-tier-1 
  (&key 
    (s-db nil) 
    (i-tdx nil) 
    (s-val nil) 
    (s-today nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val s-today)))
    (run-tier-n tier-1)))

(defun run-tier-2 
  (&key 
    (s-db nil) 
    (i-tdx nil) 
    (s-val nil) 
    (s-today nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val s-today)))
    (run-tier-n tier-2)))

(defun run-tier-3 
  (&key 
    (s-db nil) 
    (i-tdx nil) 
    (s-val nil) 
    (s-today nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val s-today)))
    (run-tier-n tier-3)))

(defun run-tier-all 
  (&key 
    (s-db nil) 
    (i-tdx nil) 
    (s-val nil) 
    (s-today nil))
  (let ((*cfg* (config-cfg s-db i-tdx s-val s-today)))
    (run-tier-n tier-all)))

(defun download-tickers (tix)
  (yahoo:download-tickers (funcall (get-tdx)) tix))

(defun download-all (&key (i-tdx nil))
  (if i-tdx
    (funcall (get-tdx) :invalidate t))
  (yahoo:download-tickers (funcall (get-tdx)) tier-all))

(defun show-yahoo-periods ()
  (let ((periods (yahoo::show-yahoo-periods (funcall (get-tdx)) tier-all)))
    (dolist (p periods)
      (format t "[~a] ~a -> ~a~%" (getf p :oid) (getf p :ticker) (getf p :period)))))

(defun download-spots (tickers)
  (dolist (ticker tickers)
    (yahoo:download-spot ticker)))

(defun dl-spots-tier-1 ()
  (download-spots tier-1))

(defun dl-spots-tier-2 ()
  (download-spots tier-2))

(defun dl-spots-tier-3 ()
  (download-spots tier-3))

(defun dl-spots-all ()
  (download-spots tier-all))

(defun save-spots-redis (tickers &key (db 0) (save-open nil) (download nil))
  (when download
    (download-spots tickers))
  (let ((prices (remove-if #'null (mapcar #'pa:parse-spot tickers))))
    (rutil:save-stockprices prices db save-open)))


(defun prn-tdx (&key (i-tdx nil))
  (if i-tdx
    (funcall (get-tdx) :invalidate t))
  (co:print-hash (funcall (get-tdx))))

(defun prod ()
  (setf *profile* :prod))

(defun atest ()
  (setf *profile* :atest))

(defun curp ()
  (format t "Current profile: ~a" *profile*))
;(asdf:component-pathname (asdf:find-system :etradejanitor))

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
