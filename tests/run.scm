(use test
     files
     call-with-sqlite3-connection
     debug
     srfi-19
     posix)
(require-library auth)
(import (prefix auth auth:))

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
     (test-group
      "create-or-update"
      (test-assert
       "create"
       (condition-case
        (auth:create-or-update
         connection
         "create-or-update-user"
         "create-or-update-password"
         "create-or-update-role")
        ((exn) #f)))

      (test-assert
       "creation valid?"
       (auth:valid?
        connection
        "create-or-update-user"
        "create-or-update-password"
        "create-or-update-role"))

      (test-assert
       "update"
       (condition-case
        (auth:create-or-update
         connection
         "create-or-update-user"
         "create-or-update-password2"
         "create-or-update-role")))

      (test-assert
       "update valid?"
       (auth:valid?
        connection
        "create-or-update-user"
        "create-or-update-password2"
        "create-or-update-role")))

     (test-group
      "timestamp"
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
         (auth:timestamp connection "test" "test"))

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
         (>= (- (auth:timestamp connection "test" "test")
                local-timestamp)
             sleep-seconds)))))))

(test-exit)