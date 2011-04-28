DROP TABLE IF EXISTS auth;
CREATE TABLE auth (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user TEXT NOT NULL,
       salt TEXT NOT NULL,
       role TEXT NOT NULL,
       hash TEXT NOT NULL,
       timestamp INTEGER DEFAULT (strftime('%s', 'now')),
       -- This constraint is not violated when either user or role are
       -- null; therefore, NOT NULL supra.
       UNIQUE (user, role)
);

-- create-or-update triggers INSERT not UPDATE.
CREATE TRIGGER insert_timestamp INSERT ON auth
    BEGIN
       UPDATE auth SET timestamp = strftime('%s', 'now')
       WHERE rowid = new.rowid;
    END;

-- putting this here, even the insert_timestamp is the relevant
-- trigger for create-or-update.
CREATE TRIGGER update_timestamp UPDATE ON auth
    BEGIN
       UPDATE auth SET timestamp = strftime('%s', 'now')
       WHERE rowid = new.rowid;
    END;
