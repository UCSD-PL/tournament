DROP TABLE IF EXISTS testCases;
DROP TABLE IF EXISTS functions;
DROP TABLE IF EXISTS assignments;
DROP TABLE IF EXISTS courses;
DROP TABLE IF EXISTS users;

CREATE TABLE IF NOT EXISTS users(
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  password TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS courses(
  id SERIAL PRIMARY KEY,
  userId INT NOT NULL REFERENCES users(id),
  code TEXT,
  name TEXT,
  term TEXT
);

CREATE TABLE IF NOT EXISTS assignments(
  id SERIAL PRIMARY KEY,
  courseId INT NOT NULL REFERENCES courses(id),
  name TEXT,
  maxPoints INT
);

CREATE TABLE IF NOT EXISTS functions(
  id SERIAL PRIMARY KEY,
  assignemntId INT NOT NULL REFERENCES assignments(id),
  description TEXT,
  instructions TEXT
);

CREATE TABLE IF NOT EXISTS testCases(
  id SERIAL PRIMARY KEY,
  functionId INT NOT NULL REFERENCES functions(id),
  userId INT NOT NULL REFERENCES users(id),
  args TEXT,
  attempts INT NOT NULL DEFAULT 0,
  failures INT NOT NULL DEFAULT 0
);
