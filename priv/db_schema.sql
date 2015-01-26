-- Table: pastebin

-- DROP TABLE pastebin;

CREATE TABLE pastebin
(
  id serial NOT NULL,
  release character varying(16) NOT NULL,
  emit character varying(16) NOT NULL,
  expires timestamp with time zone,
  created timestamp with time zone NOT NULL,
  code text NOT NULL,
  CONSTRAINT pastebin_pk PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE pastebin
  OWNER TO eplaypen;
