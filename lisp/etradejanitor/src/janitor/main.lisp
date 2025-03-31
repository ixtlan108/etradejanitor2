(defpackage janitor/main
  (:use :cl)
  (:import-from :local-time #:timestamp<)
  (:import-from :janitor/types
    #:s-dx)
  (:import-from :janitor/common
    #:clet
    #:clet*
    #:>0
    #:partial
    #:fn
    #:vec-last)
  (:import-from :trivia #:match)
  (:local-nicknames
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

(defvar tier-1
  '("NHY" "STB" "YAR"))

(defvar tier-2
  '("AKSO" "DNB" "GJF" "ORK" "PGS" "TOM"))

(defvar tier-3
  '("BAKKA" "BWLPG" "DNO" "GOGL" "NAS" "SUBC" "TGS"))

;(defvar tickers-all ())

(defun validate-cut-offs (co dx)
  (clet
    (start-date (s-dx (vec-last co)))
      (if (timestamp< dx start-date)
        (list :status :start-date-error)
        (list :status :ok :result co))))

(defun parse-ticker (ticker ht)
  (clet
    (cur-dx (gethash ticker ht))
    (if cur-dx
      (clet
        (oid (gethash ticker ticker-oid-ht)
          items (pa:parse ticker))
        (if items
          (clet (cut-offs (pa:cut-off items oid cur-dx))
            (if (>0 cut-offs)
              (validate-cut-offs cut-offs cur-dx)
              (list :status :empty-cutoffs)))
          (list :status :missing-csv)))
      (list :status :empty-dx))))

(defun parse-tickers (ht-tdx tickers)
  (clet (remaining '())
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
          (_ (push (list :status :unknown :ticker ticker) remaining)))))
    remaining))

(defparameter *tdx* nil)

(defun tdx ()
  (unless *tdx*
    (print "Setting tdx...")
    (setq *tdx* (janitor/db:ticker-dx)))
  *tdx*)

(defun run (tickers)
  (clet (tix (parse-tickers (tdx) tickers))
    tix))

(defun run-tier-1 ()
  (run tier-1))

(defun download-all ()
  (clet (tix (concatenate 'list tier-1 tier-2 tier-3))
    (janitor/yahoo::download-tickers (tdx) tix)))

; (defun copy-ht-item (new-ht ticker key value)
;   (format t "~S ~S ~S => ~S~%"
;           new-ht ticker key value))

; (defun copy-ht-items (ht)
;   (clet*
;     (new-ht (make-hash-table :test 'equal)
;     func (partial #'copy-ht-item new-ht))
;       (maphash func ht "YAR")))


; (defun match-demo (m)
;   (match m
;     ((list :status :one ) :one)
;     ((list :status :two :result x) x)
;     (_ :three)))
