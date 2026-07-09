(defpackage janitor/migrations
  (:use :cl)
  (:import-from :janitor/common
    #:*home*)
  (:local-nicknames
    (#:lt #:local-time)
    (#:co #:janitor/common)
    (#:db #:janitor/db))
  (:export
    #:exec-migrations
    #:get-migrations
    #:new-migration))

(in-package :janitor/migrations)


(defparameter *feed*
  (format nil "~a/database/migrations" *home*))

(deftype dbvar-enum () '(member :postgres :tcp :inss :insp :insm))
  
(declaim (ftype (function (dbvar-enum) t) db-variant-paths))
(defun db-variant-paths (db-var)
  (cond 
    ((eq db-var :postgres) (list "postgres" "inserts"))
    ((eq db-var :tcp) (list "postgres" "testcontainer"))))

(declaim (ftype (function (dbvar-enum) string) output-path))
(defun output-path (db-var)
  (case db-var
    (:postgres "postgres")
    (:tcp "testcontainer")
    (:insp "inserts")))

(defun info()
  (let ((dbvars (list :postgres :tcp :insp)))
    (loop :for v :in dbvars :do (format t "New Migration Output path: ~a => ~a~%" v (output-path v))))
  (let ((dbvars2 (list :postgres :tcp)))
    (loop :for v :in dbvars2 :do (format t "Db variant paths: ~a => ~a~%" v (db-variant-paths v)))))

(defun capitalize-comment (comment)
  (let* ((comment-parts (str:split #\_ comment))
         (cap (string-capitalize (first comment-parts)))
         (comment-partsx (cons cap (rest comment-parts))))
    (format nil "~{~A~^ ~}" comment-partsx)))

(defun unix-time-comment (fname_dot_sql)
  (let* ((splits (str:split "__" fname_dot_sql))
         (ut (first splits))
         (cap (capitalize-comment (first (str:split #\. (first (last splits)))))))
    (values ut cap)))

(defun unix-time-comment-sql (sql-file)
  (let ((fname_dot_sql (first (last (str:split #\/ (namestring sql-file)))))) ; 12345__this_is_a_comment.sql
    (multiple-value-bind (ut comment)
        (unix-time-comment fname_dot_sql)
      (list :unix (parse-integer ut) :comment comment :sql sql-file))))

(defun get-migrations-list ()
  (let ((sql-files (directory (format nil "~a/*.sql" *feed*))))
    (mapcar #'unix-time-comment-sql sql-files)))

(defun get-migrations (cut-off-unix-time)
  (let ((mig (get-migrations-list))
        (result (make-hash-table :test #'equal)))
    (loop for item in mig
      do
        (let ((cur-unix (getf item :unix)))
          (when (> cur-unix cut-off-unix-time)
            ;(setf (gethash (getf item :unix) result) (getf item :sql)))))
            (setf (gethash (getf item :unix) result) item))))
    result))

(defun get-migrations-keys (mig-ht)
  (let ((keys (loop for key being the hash-key of mig-ht collect key))) 
    (sort keys #'<)))

(defun exec-migrations (profile &key (test-run nil))
  (let* ((cur-version (first (first (db:current-migration profile))))
         (migs (get-migrations cur-version)))
    (if (= 0 (hash-table-count migs))
      (format t "Already at latest version: ~a" cur-version)
      (progn
        (co:print-hash migs)
        (format t "Current version: ~a~%" cur-version)
        (let ((mig-keys (get-migrations-keys migs)))
          (format t "mig-keys ~a~%" mig-keys)
          (dolist (k mig-keys)
            (format t "key ~a~%" k)
            (let* ((cur-sql (gethash k migs))
                   (cur-unix (getf cur-sql :unix))
                   (cur-comment (getf cur-sql :comment)))
              (format t "~a ~a~%" cur-unix cur-comment)
              (when (not test-run)
                (db:insert-migration-version profile cur-unix cur-comment (getf cur-sql :sql))))))))))


(defun write-migration (fname unix comment)
  (with-open-file (output fname
                    :direction         :output
                    :if-does-not-exist :create
                    :if-exists         :supersede) 
    (format output "--- new migration ~a ---~%" (co:iso-8601-string (lt:now)))
    (let ((pretty-comment (capitalize-comment comment)))
      (format output "insert into stockmarket.migrations (version,comment) values (~a,'~a');" unix pretty-comment))))

(defun new-migration (comment)
  (let* ((unix (co:unix-time-now))
         (dbv-path (output-path db-var))
         (fname (format nil "~a/~a/~a__~a.sql" *feed* dbv-path unix comment)))
    (write-migration fname unix comment)))



