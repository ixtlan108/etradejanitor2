--
-- PostgreSQL database dump
--

-- Dumped from database version 17.2 (Debian 17.2-1.pgdg120+1)
-- Dumped by pg_dump version 17.2 (Debian 17.2-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

create database trader_itest owner trader;

\c trader_itest;

CREATE SCHEMA stockmarket;

ALTER SCHEMA stockmarket OWNER TO trader;

CREATE DOMAIN stockmarket.bool_type AS character(1) CONSTRAINT bool_type CHECK (((VALUE = 'y'::bpchar) OR (VALUE = 'n'::bpchar)));

ALTER DOMAIN stockmarket.bool_type OWNER TO trader;

CREATE DOMAIN stockmarket.company_name AS character varying(20);

ALTER DOMAIN stockmarket.company_name OWNER TO trader;

CREATE DOMAIN stockmarket.critter_name AS character varying(10);

ALTER DOMAIN stockmarket.critter_name OWNER TO trader;

CREATE DOMAIN stockmarket.critter_type AS integer DEFAULT 1;

ALTER DOMAIN stockmarket.critter_type OWNER TO trader;

CREATE DOMAIN stockmarket.imp_vol AS numeric(8,4);

ALTER DOMAIN stockmarket.imp_vol OWNER TO trader;

CREATE DOMAIN stockmarket.option_type AS character(1) CONSTRAINT option_type_check CHECK (((VALUE = 'c'::bpchar) OR (VALUE = 'p'::bpchar)));

ALTER DOMAIN stockmarket.option_type OWNER TO trader;

CREATE DOMAIN stockmarket.opx_series AS character(2);

ALTER DOMAIN stockmarket.opx_series OWNER TO trader;

CREATE DOMAIN stockmarket.price AS numeric(15,2) DEFAULT 0;

ALTER DOMAIN stockmarket.price OWNER TO trader;

CREATE DOMAIN stockmarket.rule_desc AS character varying(100);

ALTER DOMAIN stockmarket.rule_desc OWNER TO trader;

CREATE DOMAIN stockmarket.sell_volume AS integer DEFAULT 10;

ALTER DOMAIN stockmarket.sell_volume OWNER TO trader;

CREATE DOMAIN stockmarket.status AS integer;

ALTER DOMAIN stockmarket.status OWNER TO trader;

CREATE DOMAIN stockmarket.status_desc AS character varying(20);

ALTER DOMAIN stockmarket.status_desc OWNER TO trader;

CREATE DOMAIN stockmarket.ticker AS character varying(20);

ALTER DOMAIN stockmarket.ticker OWNER TO trader;

CREATE DOMAIN stockmarket.ticker_category AS smallint;

ALTER DOMAIN stockmarket.ticker_category OWNER TO trader;

CREATE DOMAIN stockmarket.volume AS bigint DEFAULT 0;

ALTER DOMAIN stockmarket.volume OWNER TO trader;

SET default_tablespace = '';

SET default_table_access_method = heap;

CREATE TABLE stockmarket.stockprice (
    oid integer NOT NULL,
    ticker_id integer,
    dx date DEFAULT now(),
    opn stockmarket.price,
    hi stockmarket.price,
    lo stockmarket.price,
    cls stockmarket.price,
    vol stockmarket.volume
);

ALTER TABLE stockmarket.stockprice OWNER TO trader;

CREATE TABLE stockmarket.stocktickers (
    oid integer NOT NULL,
    ticker stockmarket.ticker NOT NULL,
    company_name stockmarket.company_name NOT NULL,
    status stockmarket.status DEFAULT 1,
    ticker_category stockmarket.ticker_category DEFAULT 1 NOT NULL
);

ALTER TABLE stockmarket.stocktickers OWNER TO trader;

CREATE TABLE stockmarket.accept_rules (
    oid integer NOT NULL,
    value stockmarket.price NOT NULL,
    rtyp integer NOT NULL,
    active stockmarket.bool_type DEFAULT 'y'::bpchar,
    description stockmarket.rule_desc,
    cid integer
);

ALTER TABLE stockmarket.accept_rules OWNER TO trader;

CREATE SEQUENCE stockmarket.accept_rules_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.accept_rules_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.accept_rules_oid_seq OWNED BY stockmarket.accept_rules.oid;

CREATE TABLE stockmarket.blackscholes (
    oid integer NOT NULL,
    iv_buy stockmarket.imp_vol NOT NULL,
    iv_sell stockmarket.imp_vol NOT NULL
);

ALTER TABLE stockmarket.blackscholes OWNER TO trader;

CREATE TABLE stockmarket.critter (
    oid integer NOT NULL,
    status stockmarket.status DEFAULT 0,
    critter_type stockmarket.critter_type,
    sell_vol stockmarket.sell_volume DEFAULT 10,
    purchase_id integer,
    sale_id integer
);

ALTER TABLE stockmarket.critter OWNER TO trader;

CREATE SEQUENCE stockmarket.critter_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.critter_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.critter_oid_seq OWNED BY stockmarket.critter.oid;

CREATE TABLE stockmarket.deny_rules (
    oid integer NOT NULL,
    value stockmarket.price NOT NULL,
    rtyp integer NOT NULL,
    group_id integer NOT NULL,
    active stockmarket.bool_type DEFAULT 'y'::bpchar,
    has_memory stockmarket.bool_type DEFAULT 'y'::bpchar
);

ALTER TABLE stockmarket.deny_rules OWNER TO trader;

CREATE SEQUENCE stockmarket.deny_rules_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.deny_rules_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.deny_rules_oid_seq OWNED BY stockmarket.deny_rules.oid;

CREATE TABLE stockmarket.gradient_rules (
    oid integer NOT NULL,
    rtyp integer NOT NULL,
    value_1 stockmarket.price NOT NULL,
    value_2 stockmarket.price NOT NULL,
    level_1 stockmarket.price NOT NULL,
    level_2 stockmarket.price NOT NULL,
    active stockmarket.bool_type DEFAULT 'y'::bpchar,
    cid integer
);

ALTER TABLE stockmarket.gradient_rules OWNER TO trader;

CREATE SEQUENCE stockmarket.gradient_rules_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.gradient_rules_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.gradient_rules_oid_seq OWNED BY stockmarket.gradient_rules.oid;

CREATE TABLE stockmarket.migrations (
    version integer NOT NULL,
    comment character varying(50) NOT NULL
);

ALTER TABLE stockmarket.migrations OWNER TO trader;

CREATE TABLE stockmarket.optionprice (
    oid integer NOT NULL,
    spot_id integer NOT NULL,
    opx_id integer NOT NULL,
    buy stockmarket.price NOT NULL,
    sell stockmarket.price NOT NULL
);

ALTER TABLE stockmarket.optionprice OWNER TO trader;

CREATE SEQUENCE stockmarket.optionprice_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.optionprice_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.optionprice_oid_seq OWNED BY stockmarket.optionprice.oid;

CREATE TABLE stockmarket.optionpurchase (
    oid integer NOT NULL,
    opid integer NOT NULL,
    dx date DEFAULT now(),
    price stockmarket.price NOT NULL,
    volume stockmarket.volume NOT NULL,
    status stockmarket.status DEFAULT 0,
    transaction_cost stockmarket.price DEFAULT 0 NOT NULL,
    purchase_type integer DEFAULT 0 NOT NULL,
    spot stockmarket.price DEFAULT 0 NOT NULL,
    buy stockmarket.price NOT NULL
);

ALTER TABLE stockmarket.optionpurchase OWNER TO trader;

CREATE SEQUENCE stockmarket.optionpurchase_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.optionpurchase_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.optionpurchase_oid_seq OWNED BY stockmarket.optionpurchase.oid;

CREATE TABLE stockmarket.optionsale (
    oid integer NOT NULL,
    purchase_id integer NOT NULL,
    dx date DEFAULT now(),
    price stockmarket.price NOT NULL,
    volume stockmarket.volume NOT NULL,
    status stockmarket.status DEFAULT 0,
    transaction_cost stockmarket.price DEFAULT 0 NOT NULL
);

ALTER TABLE stockmarket.optionsale OWNER TO trader;

CREATE SEQUENCE stockmarket.optionsale_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.optionsale_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.optionsale_oid_seq OWNED BY stockmarket.optionsale.oid;

CREATE TABLE stockmarket.optionx (
    oid integer NOT NULL,
    opname stockmarket.ticker NOT NULL,
    strike stockmarket.price NOT NULL,
    exp_date date NOT NULL,
    optype stockmarket.option_type NOT NULL,
    stock_id integer DEFAULT 0 NOT NULL,
    series stockmarket.opx_series
);

ALTER TABLE stockmarket.optionx OWNER TO trader;

CREATE SEQUENCE stockmarket.optionx_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.optionx_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.optionx_oid_seq OWNED BY stockmarket.optionx.oid;

CREATE TABLE stockmarket.red_days (
    dx date NOT NULL
);

ALTER TABLE stockmarket.red_days OWNER TO trader;

CREATE TABLE stockmarket.rule_types (
    oid integer NOT NULL,
    description stockmarket.rule_desc NOT NULL
);

ALTER TABLE stockmarket.rule_types OWNER TO trader;

CREATE SEQUENCE stockmarket.rule_types_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.rule_types_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.rule_types_oid_seq OWNED BY stockmarket.rule_types.oid;

CREATE TABLE stockmarket.spot (
    oid integer NOT NULL,
    stock_id integer NOT NULL,
    dx date NOT NULL,
    tm time without time zone NOT NULL,
    price stockmarket.price NOT NULL
);

ALTER TABLE stockmarket.spot OWNER TO trader;

CREATE SEQUENCE stockmarket.spot_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.spot_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.spot_oid_seq OWNED BY stockmarket.spot.oid;

CREATE TABLE stockmarket.status_codes (
    oid integer NOT NULL,
    description stockmarket.status_desc NOT NULL
);

ALTER TABLE stockmarket.status_codes OWNER TO trader;

CREATE TABLE stockmarket.stock_purchase (
    oid integer NOT NULL,
    ticker_id smallint NOT NULL,
    unix_time integer NOT NULL,
    price stockmarket.price NOT NULL,
    volume stockmarket.volume NOT NULL,
    status stockmarket.status DEFAULT 0 NOT NULL,
    transaction_cost stockmarket.price DEFAULT 0 NOT NULL
);

ALTER TABLE stockmarket.stock_purchase OWNER TO trader;

CREATE SEQUENCE stockmarket.stock_purchase_oid_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.stock_purchase_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.stock_purchase_oid_seq OWNED BY stockmarket.stock_purchase.oid;

CREATE SEQUENCE stockmarket.stockprice_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.stockprice_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.stockprice_oid_seq OWNED BY stockmarket.stockprice.oid;

CREATE SEQUENCE stockmarket.stocktickers_oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE stockmarket.stocktickers_oid_seq OWNER TO trader;

ALTER SEQUENCE stockmarket.stocktickers_oid_seq OWNED BY stockmarket.stocktickers.oid;

CREATE VIEW stockmarket.v_spot_opx AS
 SELECT (o.exp_date - p.dx) AS days,
    o.stock_id,
    s.ticker,
    o.oid AS opx_id,
    o.opname,
    o.strike,
    o.exp_date,
    o.optype,
    i.oid AS price_id,
    i.buy,
    i.sell,
    p.oid AS spot_id,
    p.dx,
    p.tm,
    p.price AS spot
   FROM (((stockmarket.optionx o
     JOIN stockmarket.stocktickers s ON ((o.stock_id = s.oid)))
     JOIN stockmarket.optionprice i ON ((i.opx_id = o.oid)))
     JOIN stockmarket.spot p ON ((p.oid = i.spot_id)));

ALTER VIEW stockmarket.v_spot_opx OWNER TO trader;

ALTER TABLE ONLY stockmarket.accept_rules ALTER COLUMN oid SET DEFAULT nextval('stockmarket.accept_rules_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.critter ALTER COLUMN oid SET DEFAULT nextval('stockmarket.critter_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.deny_rules ALTER COLUMN oid SET DEFAULT nextval('stockmarket.deny_rules_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.gradient_rules ALTER COLUMN oid SET DEFAULT nextval('stockmarket.gradient_rules_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.optionprice ALTER COLUMN oid SET DEFAULT nextval('stockmarket.optionprice_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.optionpurchase ALTER COLUMN oid SET DEFAULT nextval('stockmarket.optionpurchase_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.optionsale ALTER COLUMN oid SET DEFAULT nextval('stockmarket.optionsale_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.optionx ALTER COLUMN oid SET DEFAULT nextval('stockmarket.optionx_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.rule_types ALTER COLUMN oid SET DEFAULT nextval('stockmarket.rule_types_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.spot ALTER COLUMN oid SET DEFAULT nextval('stockmarket.spot_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.stock_purchase ALTER COLUMN oid SET DEFAULT nextval('stockmarket.stock_purchase_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.stockprice ALTER COLUMN oid SET DEFAULT nextval('stockmarket.stockprice_oid_seq'::regclass);

ALTER TABLE ONLY stockmarket.stocktickers ALTER COLUMN oid SET DEFAULT nextval('stockmarket.stocktickers_oid_seq'::regclass);


COPY stockmarket.migrations (version, comment) FROM stdin;
1746968400	Bootstrapping
\.

COPY stockmarket.rule_types (oid, description) FROM stdin;
1	Diff from watermark
7	Diff from bought
6	Option price roof (valid if below option price)
5	Option price floor (valid if above option price)
4	Stock price roof (valid if below stock price)
3	Stock price floor (valid if above stock price)
2	Diff from watermark percent
8	Composite
9	Gradient diff from watermark
\.

COPY stockmarket.status_codes (oid, description) FROM stdin;
1	True/active/valid
0	False/inact./invalid
-1	Temporary inactive
7	Critter active
8	Critter inactive
4	Option test purchase
3	Option purchase
11	Option paper purch.
9	Critter sold
2	Option fully sold
\.


COPY stockmarket.stocktickers (oid, ticker, company_name, status, ticker_category) FROM stdin;
5	ACY	Acergy	0	1
10	DNBNOR	DnB NOR	0	1
15	TAA	Tandberg	0	1
22	NSG	Norske Skogindustr	0	1
13	RCL	Royal Caribbean Crui	0	1
24	OSEBX	Oslo Børs Benchmark	0	3
17	TOM	Tomra	1	1
1	NHY	Norsk hydro	1	1
3	YAR	Yara	1	1
9	ORK	Orkla	1	1
12	PGS	Petroleum Geo-Serv	1	1
14	STB	Storebrand	1	1
18	AKSO	Aker Solutions	1	1
19	DNB	DNB	1	1
20	DNO	DNO International	1	1
21	GJF	Gjensidige Forsikr	1	1
23	SUBC	Subsea 7	1	1
16	TGS	TGS-NOPEC Geophysica	1	1
26	BWLPG	BW LPG	1	1
27	BAKKA	Bakkafrost	1	1
28	GOGL	Golden Ocean Group	1	1
29	NAS	Norw. Air Shuttle	1	1
7	OBX	Total Return Index	-1	3
4	SDRL	Seadrill	0	1
8	MHG	Marine Harvest	0	1
11	REC	Renewable Energy Cor	0	1
25	AKERBP	Aker BP	0	1
2	EQNR	Equinor	-1	1
6	TEL	Telenor	0	1
\.

SELECT pg_catalog.setval('stockmarket.accept_rules_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.critter_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.deny_rules_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.gradient_rules_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.optionprice_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.optionpurchase_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.optionsale_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.optionx_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.rule_types_oid_seq', 1, false);

SELECT pg_catalog.setval('stockmarket.spot_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.stock_purchase_oid_seq', 1, false);

SELECT pg_catalog.setval('stockmarket.stockprice_oid_seq', 1, true);

SELECT pg_catalog.setval('stockmarket.stocktickers_oid_seq', 29, true);

ALTER TABLE ONLY stockmarket.accept_rules
    ADD CONSTRAINT accept_rules_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.critter
    ADD CONSTRAINT critter_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.deny_rules
    ADD CONSTRAINT deny_rules_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.gradient_rules
    ADD CONSTRAINT gradient_rules_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.blackscholes
    ADD CONSTRAINT iv_blackscholes_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.migrations
    ADD CONSTRAINT migrations_pkey PRIMARY KEY (version);

ALTER TABLE ONLY stockmarket.optionx
    ADD CONSTRAINT option_uc1 UNIQUE (opname);

ALTER TABLE ONLY stockmarket.optionprice
    ADD CONSTRAINT optionprice_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.optionprice
    ADD CONSTRAINT optionprice_spot_id_opx_id_key UNIQUE (spot_id, opx_id);

ALTER TABLE ONLY stockmarket.optionpurchase
    ADD CONSTRAINT optionpurchase_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.optionpurchase
    ADD CONSTRAINT optionpurchase_uc1 UNIQUE (opid, dx);

ALTER TABLE ONLY stockmarket.optionsale
    ADD CONSTRAINT optionsale_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.optionx
    ADD CONSTRAINT optionx_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.red_days
    ADD CONSTRAINT red_days_pkey PRIMARY KEY (dx);

ALTER TABLE ONLY stockmarket.rule_types
    ADD CONSTRAINT rule_types_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.spot
    ADD CONSTRAINT spot_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.spot
    ADD CONSTRAINT spot_uc1 UNIQUE (stock_id, dx, tm);

ALTER TABLE ONLY stockmarket.status_codes
    ADD CONSTRAINT status_codes_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.stock_purchase
    ADD CONSTRAINT stock_purchase_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.stockprice
    ADD CONSTRAINT stockprice_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.stockprice
    ADD CONSTRAINT stockprice_uc1 UNIQUE (ticker_id, dx);

ALTER TABLE ONLY stockmarket.stocktickers
    ADD CONSTRAINT stocktickers_pkey PRIMARY KEY (oid);

ALTER TABLE ONLY stockmarket.accept_rules
    ADD CONSTRAINT acc_critter_fkey FOREIGN KEY (cid) REFERENCES stockmarket.critter(oid);

ALTER TABLE ONLY stockmarket.accept_rules
    ADD CONSTRAINT accept_rules_type_fkey FOREIGN KEY (rtyp) REFERENCES stockmarket.rule_types(oid) ON DELETE RESTRICT;

ALTER TABLE ONLY stockmarket.critter
    ADD CONSTRAINT critter_purchase_id_fkey FOREIGN KEY (purchase_id) REFERENCES stockmarket.optionpurchase(oid);

ALTER TABLE ONLY stockmarket.critter
    ADD CONSTRAINT critter_sale_id_fkey FOREIGN KEY (sale_id) REFERENCES stockmarket.optionsale(oid);

ALTER TABLE ONLY stockmarket.critter
    ADD CONSTRAINT critter_status_id_fkey FOREIGN KEY (status) REFERENCES stockmarket.status_codes(oid);

ALTER TABLE ONLY stockmarket.deny_rules
    ADD CONSTRAINT deny_acc_fkey FOREIGN KEY (group_id) REFERENCES stockmarket.accept_rules(oid);

ALTER TABLE ONLY stockmarket.deny_rules
    ADD CONSTRAINT deny_rules_type_fkey FOREIGN KEY (rtyp) REFERENCES stockmarket.rule_types(oid) ON DELETE RESTRICT;

ALTER TABLE ONLY stockmarket.gradient_rules
    ADD CONSTRAINT grad_critter_fkey FOREIGN KEY (cid) REFERENCES stockmarket.critter(oid);

ALTER TABLE ONLY stockmarket.blackscholes
    ADD CONSTRAINT iv_blackscholes_oid_fkey FOREIGN KEY (oid) REFERENCES stockmarket.optionprice(oid);

ALTER TABLE ONLY stockmarket.optionprice
    ADD CONSTRAINT optionprice_opx_id_fkey FOREIGN KEY (opx_id) REFERENCES stockmarket.optionx(oid);

ALTER TABLE ONLY stockmarket.optionprice
    ADD CONSTRAINT optionprice_spot_id_fkey FOREIGN KEY (spot_id) REFERENCES stockmarket.spot(oid);

ALTER TABLE ONLY stockmarket.optionpurchase
    ADD CONSTRAINT optionpurchase_opid_fkey FOREIGN KEY (opid) REFERENCES stockmarket.optionx(oid);

ALTER TABLE ONLY stockmarket.optionpurchase
    ADD CONSTRAINT optionpurchase_status_fkey FOREIGN KEY (purchase_type) REFERENCES stockmarket.status_codes(oid);

ALTER TABLE ONLY stockmarket.optionsale
    ADD CONSTRAINT optionsale_purchase_id_fkey FOREIGN KEY (purchase_id) REFERENCES stockmarket.optionpurchase(oid);

ALTER TABLE ONLY stockmarket.optionx
    ADD CONSTRAINT optionx_stock_fkey FOREIGN KEY (stock_id) REFERENCES stockmarket.stocktickers(oid);

ALTER TABLE ONLY stockmarket.spot
    ADD CONSTRAINT spot_stock_id_fkey FOREIGN KEY (stock_id) REFERENCES stockmarket.stocktickers(oid);

ALTER TABLE ONLY stockmarket.stock_purchase
    ADD CONSTRAINT stock_purchase_ticker_id_fkey FOREIGN KEY (ticker_id) REFERENCES stockmarket.stocktickers(oid);

ALTER TABLE ONLY stockmarket.stockprice
    ADD CONSTRAINT stockprice_ticker_id_fkey FOREIGN KEY (ticker_id) REFERENCES stockmarket.stocktickers(oid);

ALTER TABLE ONLY stockmarket.stocktickers
    ADD CONSTRAINT stocktickers_status_fkey FOREIGN KEY (status) REFERENCES stockmarket.status_codes(oid);


-- Test data

insert into stockmarket.optionx (opname,strike,exp_date,optype,stock_id,series) values ('YAR7C800',800,'2027-03-19','c',3,'7C');

insert into stockmarket.optionpurchase (opid,dx,price,volume,status,purchase_type,spot,buy) values (2,'2026-06-03',312.0,10,1,11,516.0,300.0);

insert into stockmarket.critter (status,critter_type,sell_vol,purchase_id) values (7,1,10,2);

insert into stockmarket.accept_rules (value,rtyp,cid) values (16.0,7,2);


