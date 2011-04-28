(module
 auth

 (create
  valid?
  update
  delete
  timestamp
  create-or-update)

 (import scheme
         chicken
         extras)

 (use srfi-1
      sha2
      debug)

 (require-library sqlite3)
 (import (prefix sqlite3 sqlite3:))

 (define salt
   (case-lambda
    (()
     (salt 32))
    ((length)
     (let ((random-letter
            (lambda () (integer->char (+ 32 (random (+ 94 1)))))))
       (list->string (list-tabulate length (lambda (i) (random-letter))))))))

 (define (hash password salt)
   (sha512-digest (string-append password salt)))

 (define (valid? connection user password role)
   (sqlite3:call-with-temporary-statements
    (lambda (salt)
      (let ((salt-hash (condition-case
                        (sqlite3:first-row salt user role)
                        ((exn sqlite3) #f))))
        (and salt-hash
             (string=? (cadr salt-hash)
                       (hash password (car salt-hash))))))
    connection
    "SELECT salt, hash FROM auth WHERE user = ? AND role = ?;"))

 (define (create-or-update connection user password role)
   (let* ((salt (salt))
          (hash (hash password salt)))
     (sqlite3:call-with-temporary-statements
      (lambda (create-or-update)
        (sqlite3:execute create-or-update user salt role hash))
      connection
      "INSERT OR REPLACE INTO auth (user, salt, role, hash) VALUES(?, ?, ?, ?)")))

 (define create create-or-update)

 (define update create-or-update)

 (define (delete connection user role)
   (sqlite3:call-with-temporary-statements
    (lambda (delete)
      (sqlite3:execute delete user role))
    connection
    "DELETE FROM auth WHERE user = ? AND role = ?;"))

 (define (timestamp connection user role)
   (sqlite3:call-with-temporary-statements
    (lambda (timestamp)
      (condition-case
       (sqlite3:first-result timestamp user role)
       ((exn sqlite3) #f)))
    connection
    "SELECT timestamp FROM auth WHERE user = ? AND role = ?;")))
