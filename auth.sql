DROP TABLE IF EXISTS auth;
CREATE TABLE auth (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user TEXT KEY,
       salt TEXT,
       role TEXT,
       hash TEXT,
       timestamp INTEGER DEFAULT (strftime('%s', 'now'))
);

CREATE TRIGGER update_timestamp UPDATE ON auth
    BEGIN
       UPDATE auth SET timestamp = strftime('%s', 'now')
       WHERE rowid = new.rowid;
    END;
