(use test
     files
     call-with-sqlite3-connection
     debug
     srfi-19
     posix)
(require-library auth)
(import (prefix auth auth:))

(test-group
 "auth"
 (let ((database (create-temporary-file ".db")))
   (test
    "load initial database"
    0
    (system (format "sqlite3 ~a < ~a"
                    database
                    (make-pathname (chicken-home) "auth.sql"))))
   (call-with-sqlite3-connection
    database
    (lambda (connection)
      (let ((local-timestamp
             (string->number (date->string (current-date) "~s")))
            (sleep-seconds 2))
        (test-assert
         "create test user"
         (condition-case
          (auth:create connection "test" "test" "test")
          ((exn) #f)))

        ;; this may fail because of locale-differences or
        ;; race-conditions, mightn't it?
        (test
         "user timestamp matches local timestamp"
         local-timestamp
         (auth:timestamp connection "test"))

        (sleep sleep-seconds)

        (test-assert
         "update test user"
         (condition-case
          (auth:update connection "test" "test" "test")
          ((exn) #f)))

        (test-assert
         (format
          "user timestamp is at least ~a seconds greater than local timestamp"
          sleep-seconds)
         (>= (- (auth:timestamp connection "test")
                local-timestamp)
             sleep-seconds)))))))

(test-exit)