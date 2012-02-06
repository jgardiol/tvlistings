# --- !Ups

CREATE SEQUENCE users_id_seq;
CREATE SEQUENCE episodes_id_seq;

CREATE TABLE users (
	id integer NOT NULL PRIMARY KEY DEFAULT NEXTVAL('users_id_seq'),
	name varchar(64) NOT NULL,
	password varchar(128) NOT NULL
);

CREATE TABLE shows(
	id integer UNIQUE PRIMARY KEY NOT NULL,
	name varchar(255) NOT NULL
);

CREATE TABLE episodes(
	id integer NOT NULL PRIMARY KEY DEFAULT NEXTVAL('episodes_id_seq'),
	title varchar(255) NOT NULL,
	season integer NOT NULL,
	epnum integer NOT NULL,
	airdate date NOT NULL,
	showid integer NOT NULL REFERENCES shows(id)
);

CREATE TABLE user_show(
	userid integer NOT NULL REFERENCES users(id),
	showid integer NOT NULL REFERENCES shows(id),
	PRIMARY KEY(userid, showid)
);

CREATE TABLE user_episode(
	userid integer NOT NULL REFERENCES users(id),
	episodeid integer NOT NULL REFERENCES episodes(id),
	PRIMARY KEY(userid, episodeid)
);

# --- !Downs

DROP TABLE user_show;
DROP TABLE user_episode;
DROP TABLE episodes;
DROP TABLE users;
DROP TABLE shows;

DROP SEQUENCE episodes_id_sequence;
DROP SEQUENCE users_id_sequence;