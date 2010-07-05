DROP TABLE IF EXISTS auth;
CREATE TABLE auth (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user TEXT KEY,
       salt TEXT,
       realm TEXT,
       hash TEXT
);
