(defpackage janitor/migrations
  (:use :cl)
  (:import-from :janitor/common
    #:*home*)
  (:export
    #:get-migrations))

(in-package :janitor/migrations)


(defvar *feed*
  (format nil "~a/database/migrations" *home*))

(defun get-unix-time-sql (sql-file)
  (let* ((splits (str:split #\/ (namestring sql-file)))
         (fname (first (last splits))))
    (list :unix (parse-integer (first (str:split #\. fname))) :sql sql-file)))

(defun get-migrations-list ()
  (let ((sql-files (directory (format nil "~a/*.sql" *feed*))))
    (mapcar #'get-unix-time-sql sql-files)))

(defun get-migrations ()
  (let ((mig (get-migrations-list))
        (result (make-hash-table :test #'equal)))
    (loop for item in mig
      do
        (setf (gethash (getf item :unix) result) (getf item :sql)))
    result))

