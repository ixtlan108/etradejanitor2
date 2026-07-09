(defpackage janitor/migrations
  (:use :cl)
  (:import-from :cl-ppcre
      #:register-groups-bind)
  (:import-from :janitor/common
    #:unix-time-now
    #:*home*)
  (:local-nicknames
    (#:lt #:local-time)
    (#:co #:janitor/common)
    (#:db #:janitor/db))
  (:export
    #:new-migration))

(in-package :janitor/migrations)

(defparameter *feed*
  (format nil "~a/database/migrations" *home*))

(defparameter *sql-home*
  (format nil "~a/database/sql" *home*))

(defparameter *docker-home*
  (format nil "~a/database/docker/docker_entry_point" *home*))

(deftype dbvar-enum () '(member :postgres :tcp :insp))

  
(declaim (ftype (function (dbvar-enum) t) db-variant-paths))
(defun db-variant-paths (db-var)
  (cond 
    ((eq db-var :postgres) (list "postgres" "inserts/postgres"))
    ((eq db-var :tcp) (list "postgres" "testcontainer/postgres"))))

(declaim (ftype (function (dbvar-enum) string) output-path))
(defun output-path (db-var)
  (case db-var
    (:postgres "postgres")
    (:tcp "testcontainer/postgres")
    (:insp "inserts/postgres")))

(defun collect-sql (p)
  (directory (format nil "~a/~a/*.sql" *feed* p)))

(defun sql-files-for (db-var)
  (let ((result (mapcar #'collect-sql (db-variant-paths db-var))))
    (co:flatten result)))

(defun sort-key-val (item)
  (getf item :unix))

(defun view-table-for (s)
  (register-groups-bind (fst snd)
    ("(view|table)_(\\w*)" s :sharedp t)
    (list (string-upcase fst) snd)))

(defun pretty-comment-std (comment)
  (let* ((comment-parts (str:split #\_ comment))
         (cap (string-capitalize (first comment-parts)))
         (comment-partsx (cons cap (rest comment-parts))))
    (format nil "~{~A~^ ~}" comment-partsx)))

(defun pretty-comment-view (comment)
  (destructuring-bind (title body) comment
    (format nil "~a ~a" title body)))

(defun pretty-comment (comment)
  (let ((vt (view-table-for comment)))
    (if (null vt)
        (pretty-comment-std comment)
        (pretty-comment-view vt))))

(defun unix-time-comment (fname_dot_sql)
  (let* ((splits (str:split "__" fname_dot_sql))
         (ut (first splits))
         (cap (pretty-comment (first (str:split #\. (first (last splits)))))))
    (values ut cap)))

; (defun sql-file-placement (s) 
;   (let ((sx (str:split #\/ s))) 
;     (if (= (length sx) 11) 
;       (format nil "~a/~a" (nth 8 sx) (nth 9 sx)) 
;       (nth 8 sx))))

(defun sql-file-placement (s) 
  (let ((sx (str:split #\/ s))) 
    (case (length sx) 
      (9 (nth 7 sx))
      (10 (if (string= "home" (nth 1 sx)) (nth 8 sx) (format nil "~a/~a" (nth 7 sx) (nth 8 sx)))) 
      (11 (format nil "~a/~a" (nth 8 sx) (nth 9 sx)))))) 

(defun unix-time-comment-sql (sql-file)
  (let* ((name-str (namestring sql-file))
         (fname_dot_sql (first (last (str:split #\/ name-str))))) ; 12345__this_is_a_comment.sql
    (multiple-value-bind (ut comment) (unix-time-comment fname_dot_sql)
      (progn 
        (let* ((unix (parse-integer ut))
               (tm (co:format-local-time (lt:unix-to-timestamp unix)))
               (place (sql-file-placement name-str)))
          (list :unix (parse-integer ut) :comment comment :sql sql-file :place place :tm tm))))))

(defun get-migrations-list (db-var)
  (let* ((sql-files (sql-files-for db-var))
         (result (mapcar #'unix-time-comment-sql sql-files)))
    (sort result #'< :key #'sort-key-val)))

(defun get-migrations (cut-off-unix-time db-var &key (closed nil))
  (let ((mig (get-migrations-list db-var))
        (result (make-hash-table :test #'equal))
        (fn (if closed #'>= #'>)))
    (loop for item in mig
      do
        (let ((cur-unix (getf item :unix)))
          (when (funcall fn cur-unix cut-off-unix-time)
            (setf (gethash (getf item :unix) result) item))))
    result))

(defun write-migration (fname unix comment)
  (with-open-file (output fname
                    :direction         :output
                    :if-does-not-exist :create
                    :if-exists         :supersede)
    (format output "--- new migration ~a ---~%" (co:iso-8601-string (lt:now)))
    (let ((pretty-comment (pretty-comment comment)))
      (format output "insert into art.migrations (version,comment) values (~a,'~a');" unix pretty-comment))))



(defun demox () 3)

(defun info()
  (let ((dbvars (list :postgres :tcp :insp)))
    (loop :for v :in dbvars :do (format t "New Migration Output path: ~a => ~a~%" v (output-path v))))
  (let ((dbvars2 (list :postgres :tcp)))
    (loop :for v :in dbvars2 :do (format t "Db variant paths: ~a => ~a~%" v (db-variant-paths v)))))

(declaim (ftype (function (string dbvar-enum) t) new-migration))
(defun new-migration (comment db-var)
  (let* ((unix (unix-time-now))
         (dbv-path (output-path db-var))
         (fname (format nil "~a/~a/~a__~a.sql" *feed* dbv-path unix comment)))
    (format nil "NEW MIGRATION: ~a" fname)
    (write-migration fname unix comment)))


(defparameter cco 1753719799)

(defun get-migrations-keys (mig-ht)
  (let ((keys (loop for key being the hash-key of mig-ht collect key)))
    (sort keys #'<)))

(defun num-spaces (db-var)
  (check-type db-var dbvar-enum)
  (cond 
    ((eq db-var :postgres) 20)
    ((eq db-var :tcp) 26)))

(defun prn-migs (db-var &key (cut-off cco) (closed nil))
  (check-type db-var dbvar-enum)
  (let ((migs (get-migrations cut-off db-var :closed closed)))
    (if (= 0 (hash-table-count migs))
      (format t "Already at latest version")
      (let ((mig-keys (get-migrations-keys migs)))
        (dolist (k mig-keys)
          (let ((c (gethash k migs)))
            (let ((key (getf c :unix))
                  (tm (getf c :tm))
                  (comment (getf c :comment))
                  (place (getf c :place)))
              (progn
                (cond 
                  ((eq db-var :postgres) (format t "[~a] ~20TKey: ~:D, tm: ~a, comment: ~a~%" place key tm comment))
                  ((eq db-var :tcp) (format t "[~a] ~26TKey: ~:D, tm: ~a, comment: ~a~%" place key tm comment)))
                (format t "~v@{~A~:*~}~%" 12 "----------")))))))))

(defun prn-migs-2 (db-var &key (closed nil))
  (let ((co (janitor/db:current-migration :prod)))
    (prn-migs db-var :cut-off co)))

(defun write-migrations-single (source output)
  (with-open-file (in source :direction :input)
    (uiop:copy-stream-to-stream in output)
    (format output "~%~%")))

(defun write-migrations-all (cut-off-unix-time db-var result-sql closed)
  (let ((migs (get-migrations cut-off-unix-time db-var :closed closed)))
    (if (= 0 (hash-table-count migs))
      (format t "Already at latest version")
      (progn
        (with-open-file (output result-sql
                          :direction         :output
                          :if-does-not-exist :create
                          :if-exists         :supersede)
          (let ((mig-keys (get-migrations-keys migs)))
            (dolist (k mig-keys)
              (format t "key ~a~%" k)
              (let ((cur-sql (gethash k migs)))
                (write-migrations-single (getf cur-sql :sql) output)))))))))

(defun wr-tcp (&key (cut-off cco) (closed nil))
  (write-migrations-all 
    cut-off :tcp
    (format nil "~a/~a" *docker-home* "trader_itest.sql")
    closed))

(defun wr-postgres (&key (cut-off cco) (closed nil)) 
  (write-migrations-all 
    cut-off :postgres
    (format nil "~a/~a" *sql-home* "postgres.sql")
    closed))

(defun wr-itest (&key (cut-off cco) (closed nil)) 
  (write-migrations-all 
    cut-off :tcp
    (format nil "~a/~a" *sql-home* "postgres.sql")
    closed))

(defun wr-postgres-2 (&key (closed nil)) 
  (let ((co (janitor/db:current-migration :prod)))
    (wr-postgres :cut-off co)))

(defun q() 
  (sb-ext:exit))
