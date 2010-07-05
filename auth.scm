(module
 auth

 (create
  read
  update
  delete)

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

 (define (create connection user password realm)
   (let* ((salt (salt))
          (hash (hash password salt)))
     (sqlite3:call-with-temporary-statements
      (lambda (create)
        (sqlite3:execute create user salt realm hash))
      connection
      "INSERT INTO auth (user, salt, realm, hash) VALUES(?, ?, ?, ?);")))

 (define (read connection user password realm)
   (sqlite3:call-with-temporary-statements
    (lambda (salt)
      (let ((salt-hash (condition-case
                        (sqlite3:first-row salt user realm)
                        ((exn sqlite3) #f))))
        (and salt-hash
             (string=? (cadr salt-hash)
                       (hash password (car salt-hash))))))
    connection
    "SELECT salt, hash FROM auth WHERE user = ? AND realm = ?;"))

 ;; can't change realm (makes user unique, etc.)
 (define (update connection user password realm)
   (let* ((salt (salt))
          (hash (hash password salt)))
     (sqlite3:call-with-temporary-statements
      (lambda (update)
        (sqlite3:execute update hash salt user realm))
      connection
      "UPDATE auth SET hash = ?, salt = ? WHERE user = ? AND realm = ?")))

 (define (delete connection user realm)
   (sqlite3:call-with-temporary-statements
    (lambda (delete)
      (sqlite3:execute delete user realm))
    connection
    "DELETE FROM auth WHERE user = ? AND realm = ?;")))
