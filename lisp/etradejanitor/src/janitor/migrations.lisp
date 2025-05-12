(defpackage janitor/migrations
  (:use :cl)
  (:import-from :janitor/common
    #:print-hash
    #:*home*)
  (:local-nicknames
    (#:db #:janitor/db))
  (:export
    #:exec-migrations
    #:get-migrations))

(in-package :janitor/migrations)


(defvar *feed*
  (format nil "~a/database/migrations" *home*))

(defun unix-time-comment (fname_dot_sql)
  (let* ((splits (str:split "__" fname_dot_sql))
         (ut (first splits))
         (comment-parts (str:split #\_ (first (str:split #\. (first (last splits))))))
         (cap (string-capitalize (first comment-parts)))
         (comment-partsx (cons cap (rest comment-parts))))
    (values ut (format nil "~{~A~^ ~}" comment-partsx))))

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

(defun exec-migrations (&key (test-run nil))
  ;(let* ((cur-version 0) ;(first (first (db:current-migration))))
  (let* ((cur-version (first (first (db:current-migration))))
         (migs (get-migrations cur-version))
         (mig-keys (get-migrations-keys migs)))
    (if (null mig-keys)
      (format t "Already at latest version: ~a" cur-version)
      (progn
        (format t "Current version: ~a, keys: ~a~%" cur-version mig-keys)
        (print-hash migs)))
    (dolist (k mig-keys)
      (let* ((cur-sql (gethash k migs))
             (cur-unix (getf cur-sql :unix))
             (cur-comment (getf cur-sql :comment)))
        (format t "~a ~a~%" cur-unix cur-comment)
        (when (not test-run)
          (db::insert-migration-version cur-unix cur-comment (getf cur-sql :sql)))))))




