DROP TABLE IF EXISTS auth;
CREATE TABLE auth (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user TEXT KEY,
       salt TEXT,
       role TEXT,
       hash TEXT,
       timestamp INTEGER DEFAULT (strftime('%s', 'now'))
);
