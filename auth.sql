DROP TABLE IF EXISTS auth;
CREATE TABLE auth (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user TEXT NOT NULL,
       salt TEXT,
       role TEXT NOT NULL,
       hash TEXT,
       timestamp INTEGER DEFAULT (strftime('%s', 'now')),
       -- This constraint is not violated when either user or role are
       -- null; therefore, NOT NULL supra.
       UNIQUE (user, role)
);

CREATE TRIGGER update_timestamp UPDATE ON auth
    BEGIN
       UPDATE auth SET timestamp = strftime('%s', 'now')
       WHERE rowid = new.rowid;
    END;
