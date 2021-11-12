CREATE TABLE IF NOT EXISTS timetable
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , title TEXT NOT NULL
  , start_time DATETIME NOT NULL
  , duration INTEGER NOT NULL
  );

CREATE TABLE IF NOT EXISTS seats
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , row INTEGER NOT NULL
  , seat INTEGER NOT NULL
  , available BOOLEAN NOT NULL
  , time_slot_id INTEGER REFERENCES timetable(id)
  );

CREATE TABLE IF NOT EXISTS bookings
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , seat_id INTEGER REFERENCES seats(id)
  , time_slot_id INTEGER REFERENCES timetable(id)
  , is_preliminary BOOLEAN NOT NULL
  , created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , UNIQUE(seat_id, time_slot_id)
  );

INSERT INTO timetable (title, start_time, duration)
VALUES ('John Wick 3', '2020-05-11 19:11:23', 120);

INSERT INTO seats (row, seat, available, time_slot_id)
VALUES (1, 3, true, 1);
INSERT INTO seats (row, seat, available, time_slot_id)
VALUES (2, 3, true, 1);
INSERT INTO seats (row, seat, available, time_slot_id)
VALUES (3, 3, false, 1);
