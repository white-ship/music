--
-- openGauss database dump
--

SET statement_timeout = 0;
SET xmloption = content;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET session_replication_role = replica;
SET client_min_messages = warning;

SET search_path = public;

--
-- Name: add_user_with_default_role(character varying, text, character varying); Type: FUNCTION; Schema: public; Owner: myroot
--

CREATE FUNCTION add_user_with_default_role(username character varying, password text, default_role_name character varying) RETURNS void
    LANGUAGE plpgsql NOT SHIPPABLE
 AS $$
DECLARE
    new_user_id INT;
    role_id INT;
BEGIN
    IF default_role_name IS NULL THEN default_role_name := 'user';
    END IF;

    INSERT INTO users (username, password, created_at)
    VALUES (username, password, NOW())
    RETURNING id INTO new_user_id;

    SELECT id INTO role_id FROM roles WHERE name = default_role_name;

    IF role_id IS NOT NULL THEN
        INSERT INTO user_roles (user_id, role_id)
        VALUES (new_user_id, role_id);
    END IF;
END;
$$;


ALTER FUNCTION public.add_user_with_default_role(username character varying, password text, default_role_name character varying) OWNER TO myroot;

--
-- Name: delete_user_roles_on_artist_delete(); Type: FUNCTION; Schema: public; Owner: myroot
--

CREATE FUNCTION delete_user_roles_on_artist_delete() RETURNS trigger
    LANGUAGE plpgsql NOT SHIPPABLE
 AS $$
BEGIN
    DELETE FROM user_roles
    WHERE user_id = OLD.user_id
      AND role_id = (SELECT id FROM roles WHERE name = 'artist');
    RETURN OLD;
END;
$$;


ALTER FUNCTION public.delete_user_roles_on_artist_delete() OWNER TO myroot;

--
-- Name: update_last_played_time(); Type: FUNCTION; Schema: public; Owner: myroot
--

CREATE FUNCTION update_last_played_time() RETURNS trigger
    LANGUAGE plpgsql NOT SHIPPABLE
 AS $$
BEGIN
    UPDATE songs
    SET released_at = NOW()
    WHERE id = NEW.song_id;
    RETURN NEW;
END;
$$;


ALTER FUNCTION public.update_last_played_time() OWNER TO myroot;

--
-- Name: update_user_roles_on_artist_insert(); Type: FUNCTION; Schema: public; Owner: myroot
--

CREATE FUNCTION update_user_roles_on_artist_insert() RETURNS trigger
    LANGUAGE plpgsql NOT SHIPPABLE
 AS $$
BEGIN
    INSERT INTO user_roles (user_id, role_id)
    VALUES (NEW.user_id, (SELECT id FROM roles WHERE name = 'artist'));
    RETURN NEW;
END;
$$;


ALTER FUNCTION public.update_user_roles_on_artist_insert() OWNER TO myroot;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: artists; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE artists (
    id integer NOT NULL,
    user_id integer
)
WITH (orientation=row, compression=no);


ALTER TABLE public.artists OWNER TO myroot;

--
-- Name: artists_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE artists_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.artists_id_seq OWNER TO myroot;

--
-- Name: artists_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE artists_id_seq OWNED BY artists.id;


--
-- Name: favorites; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE favorites (
    id integer NOT NULL,
    user_id integer,
    song_id integer,
    favorited_at timestamp without time zone
)
WITH (orientation=row, compression=no);


ALTER TABLE public.favorites OWNER TO myroot;

--
-- Name: favorites_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE favorites_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.favorites_id_seq OWNER TO myroot;

--
-- Name: favorites_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE favorites_id_seq OWNED BY favorites.id;


--
-- Name: history; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE history (
    id integer NOT NULL,
    user_id integer,
    song_id integer,
    played_at timestamp without time zone NOT NULL
)
WITH (orientation=row, compression=no);


ALTER TABLE public.history OWNER TO myroot;

--
-- Name: history_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.history_id_seq OWNER TO myroot;

--
-- Name: history_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE history_id_seq OWNED BY history.id;


--
-- Name: playlist_songs; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE playlist_songs (
    playlist_id integer NOT NULL,
    song_id integer NOT NULL
)
WITH (orientation=row, compression=no);


ALTER TABLE public.playlist_songs OWNER TO myroot;

--
-- Name: playlists; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE playlists (
    id integer NOT NULL,
    user_id integer,
    name character varying,
    decription text,
    created_at timestamp without time zone
)
WITH (orientation=row, compression=no);


ALTER TABLE public.playlists OWNER TO myroot;

--
-- Name: playlists_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE playlists_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.playlists_id_seq OWNER TO myroot;

--
-- Name: playlists_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE playlists_id_seq OWNED BY playlists.id;


--
-- Name: roles; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE roles (
    id integer NOT NULL,
    name character varying NOT NULL
)
WITH (orientation=row, compression=no);


ALTER TABLE public.roles OWNER TO myroot;

--
-- Name: roles_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE roles_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.roles_id_seq OWNER TO myroot;

--
-- Name: roles_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE roles_id_seq OWNED BY roles.id;


--
-- Name: songs; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE songs (
    id integer NOT NULL,
    title character varying NOT NULL,
    album character varying,
    duration integer,
    downloads integer DEFAULT 0,
    uploaded_by bigint,
    released_at timestamp without time zone,
    file_key character varying NOT NULL,
    CONSTRAINT chk_duration_positive CHECK ((duration > 0))
)
WITH (orientation=row, compression=no);


ALTER TABLE public.songs OWNER TO myroot;

--
-- Name: songs_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE songs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.songs_id_seq OWNER TO myroot;

--
-- Name: songs_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE songs_id_seq OWNED BY songs.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    username character varying NOT NULL,
    password text NOT NULL,
    created_at timestamp without time zone
)
WITH (orientation=row, compression=no);


ALTER TABLE public.users OWNER TO myroot;

--
-- Name: user_play_history; Type: VIEW; Schema: public; Owner: myroot
--

CREATE VIEW user_play_history(user_id,username,song_id,song_title,played_at) AS
    SELECT u.id AS user_id, u.username, s.id AS song_id, s.title AS song_title, h.played_at FROM ((history h JOIN users u ON ((h.user_id = u.id))) JOIN songs s ON ((h.song_id = s.id)));


ALTER VIEW public.user_play_history OWNER TO myroot;

--
-- Name: user_roles; Type: TABLE; Schema: public; Owner: myroot; Tablespace: 
--

CREATE TABLE user_roles (
    user_id integer NOT NULL,
    role_id integer NOT NULL
)
WITH (orientation=row, compression=no);


ALTER TABLE public.user_roles OWNER TO myroot;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: myroot
--

CREATE  SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE public.users_id_seq OWNER TO myroot;

--
-- Name: users_id_seq; Type: LARGE SEQUENCE OWNED BY; Schema: public; Owner: myroot
--

ALTER  SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE artists ALTER COLUMN id SET DEFAULT nextval('artists_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE favorites ALTER COLUMN id SET DEFAULT nextval('favorites_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE history ALTER COLUMN id SET DEFAULT nextval('history_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE playlists ALTER COLUMN id SET DEFAULT nextval('playlists_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE roles ALTER COLUMN id SET DEFAULT nextval('roles_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE songs ALTER COLUMN id SET DEFAULT nextval('songs_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: myroot
--

ALTER TABLE users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Data for Name: artists; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY artists (id, user_id) FROM stdin;
1	9
\.
;

--
-- Name: artists_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('artists_id_seq', 7, true);


--
-- Data for Name: favorites; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY favorites (id, user_id, song_id, favorited_at) FROM stdin;
1	9	1	2025-04-30 03:29:35.805858
\.
;

--
-- Name: favorites_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('favorites_id_seq', 5, true);


--
-- Data for Name: history; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY history (id, user_id, song_id, played_at) FROM stdin;
1	9	1	2025-04-30 00:21:21.862477
2	9	1	2025-04-30 04:08:05.069841
3	9	1	2025-04-30 04:09:10.628164
4	9	1	2025-04-30 04:10:07.404142
\.
;

--
-- Name: history_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('history_id_seq', 4, true);


--
-- Data for Name: playlist_songs; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY playlist_songs (playlist_id, song_id) FROM stdin;
\.
;

--
-- Data for Name: playlists; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY playlists (id, user_id, name, decription, created_at) FROM stdin;
\.
;

--
-- Name: playlists_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('playlists_id_seq', 1, false);


--
-- Data for Name: roles; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY roles (id, name) FROM stdin;
1	admin
2	artist
3	user
\.
;

--
-- Name: roles_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('roles_id_seq', 3, true);


--
-- Data for Name: songs; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY songs (id, title, album, duration, downloads, uploaded_by, released_at, file_key) FROM stdin;
2	great	great	160	0	1	2025-04-30 04:09:59.340919	uploads/周杰伦 - 最伟大的作品.wav
1	mojito	mojito	180	2	1	2025-04-30 04:09:05.060733	uploads/mojito.wav
\.
;

--
-- Name: songs_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('songs_id_seq', 2, true);


--
-- Data for Name: user_roles; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY user_roles (user_id, role_id) FROM stdin;
9	1
9	2
\.
;

--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: myroot
--

COPY users (id, username, password, created_at) FROM stdin;
1	user2	a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3	2025-04-29 15:48:55.708917
2	user1	a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3	2025-04-29 15:49:22.894904
9	test	9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08	2025-04-29 15:54:21.664335
\.
;

--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: myroot
--

SELECT pg_catalog.setval('users_id_seq', 9, true);


--
-- Name: artists_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE artists
    ADD CONSTRAINT artists_pkey PRIMARY KEY  (id);


--
-- Name: artists_user_id_key; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE artists
    ADD CONSTRAINT artists_user_id_key UNIQUE (user_id);


--
-- Name: favorites_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE favorites
    ADD CONSTRAINT favorites_pkey PRIMARY KEY  (id);


--
-- Name: history_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE history
    ADD CONSTRAINT history_pkey PRIMARY KEY  (id);


--
-- Name: playlist_songs_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE playlist_songs
    ADD CONSTRAINT playlist_songs_pkey PRIMARY KEY  (playlist_id, song_id);


--
-- Name: playlists_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE playlists
    ADD CONSTRAINT playlists_pkey PRIMARY KEY  (id);


--
-- Name: roles_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE roles
    ADD CONSTRAINT roles_pkey PRIMARY KEY  (id);


--
-- Name: songs_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE songs
    ADD CONSTRAINT songs_pkey PRIMARY KEY  (id);


--
-- Name: unique_user_playlist_name; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE playlists
    ADD CONSTRAINT unique_user_playlist_name UNIQUE (user_id, name);


--
-- Name: unique_user_song_favorite; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE favorites
    ADD CONSTRAINT unique_user_song_favorite UNIQUE (user_id, song_id);


--
-- Name: user_roles_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE user_roles
    ADD CONSTRAINT user_roles_pkey PRIMARY KEY  (user_id, role_id);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE users
    ADD CONSTRAINT users_pkey PRIMARY KEY  (id);


--
-- Name: users_username_key; Type: CONSTRAINT; Schema: public; Owner: myroot; Tablespace: 
--

ALTER TABLE users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: tri_delete_user_roles_on_artist_delete; Type: TRIGGER; Schema: public; Owner: myroot
--

CREATE TRIGGER tri_delete_user_roles_on_artist_delete AFTER DELETE ON public.artists FOR EACH ROW EXECUTE PROCEDURE public.delete_user_roles_on_artist_delete();


--
-- Name: tri_update_last_played_time; Type: TRIGGER; Schema: public; Owner: myroot
--

CREATE TRIGGER tri_update_last_played_time AFTER INSERT ON public.history FOR EACH ROW EXECUTE PROCEDURE public.update_last_played_time();


--
-- Name: tri_update_user_roles_on_artist_insert; Type: TRIGGER; Schema: public; Owner: myroot
--

CREATE TRIGGER tri_update_user_roles_on_artist_insert AFTER INSERT ON public.artists FOR EACH ROW EXECUTE PROCEDURE public.update_user_roles_on_artist_insert();


--
-- Name: artists_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: myroot
--

ALTER TABLE artists
    ADD CONSTRAINT artists_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: playlist_songs_playlist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: myroot
--

ALTER TABLE playlist_songs
    ADD CONSTRAINT playlist_songs_playlist_id_fkey FOREIGN KEY (playlist_id) REFERENCES playlists(id);


--
-- Name: songs_uploaded_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: myroot
--

ALTER TABLE songs
    ADD CONSTRAINT songs_uploaded_by_fkey FOREIGN KEY (uploaded_by) REFERENCES artists(id);


--
-- Name: user_roles_role_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: myroot
--

ALTER TABLE user_roles
    ADD CONSTRAINT user_roles_role_id_fkey FOREIGN KEY (role_id) REFERENCES roles(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: omm
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM omm;
GRANT CREATE,USAGE ON SCHEMA public TO omm;
GRANT USAGE ON SCHEMA public TO PUBLIC;


--
-- openGauss database dump complete
--

