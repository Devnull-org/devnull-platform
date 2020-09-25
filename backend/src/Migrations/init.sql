CREATE TABLE "user" (
  id SERIAL PRIMARY KEY,
  username VARCHAR ( 50 ) NOT NULL,
  email VARCHAR ( 255 ) UNIQUE NOT NULL,
  firstname VARCHAR ( 50 ),
  lastname VARCHAR ( 50 ),
  password VARCHAR ( 50 ) NOT NULL,
  "role" VARCHAR ( 50 )
);
