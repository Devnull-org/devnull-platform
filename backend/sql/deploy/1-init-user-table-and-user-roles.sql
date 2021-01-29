-- Deploy devnull:1-init-user-table-and-user-roles to pg

BEGIN;

CREATE TYPE userRole AS ENUM ('Admin', 'Developer', 'User');

CREATE TABLE "user" (
  id SERIAL PRIMARY KEY,
  username VARCHAR ( 50 ) NOT NULL,
  email VARCHAR ( 255 ) UNIQUE NOT NULL,
  firstname VARCHAR ( 50 ),
  lastname VARCHAR ( 50 ),
  password VARCHAR ( 50 ) NOT NULL,
  "role" VARCHAR (50) NOT NULL
);

grant all privileges on table "user" to devnull;

insert into "user" (username, email, firstname, lastname, password, "role")
   values ('v0d1ch', 'sasa.bogicevic@pm.me', 'sasa', 'bogicevic', 'sasa', 'Admin' );
insert into "user" (username, email, firstname, lastname, password, "role")
   values ('jovana', 'jovana.bogicevic@pm.me', 'jovana', 'bogicevic', 'jovana', 'User' );
insert into "user" (username, email, firstname, lastname, password, "role")
   values ('david', 'david.bogicevic@pm.me', 'david', 'bogicevic', 'david', 'User' );
insert into "user" (username, email, firstname, lastname, password, "role")
   values ('dimi', 'dimi.bogicevic@pm.me', 'dimi', 'bogicevic', 'dimi', 'Developer' );

COMMIT;
