--- new migration 2026-06-09 ---

create schema stockmarket_dev;

--------------------- Optionx --------------------- 

create table stockmarket_dev.optionx (
	oid serial4 not null,
	opname stockmarket.ticker not null,
	strike stockmarket.price not null,
	exp_date date not null,
	optype stockmarket.option_type not null,
	stock_id int4 default 0 not null,
	series stockmarket.opx_series null,
	constraint option_uc1 unique (opname),
	constraint optionx_pkey primary key (oid)
);

alter table stockmarket_dev.optionx add constraint optionx_stock_dev_fkey foreign key (stock_id) references stockmarket.stocktickers("oid");

-------------------- Option Purchase -------------------- 

create table stockmarket_dev.optionpurchase (
    oid serial4 not null,
    opid int4 not null,
    dx date default now() null,
    price stockmarket.price not null,
    volume stockmarket.volume not null,
    status stockmarket.status default 0 null,
    transaction_cost stockmarket.price default 0 not null,
    purchase_type int4 default 0 not null,
    spot stockmarket.price default 0 not null,
    buy stockmarket.price not null,
    constraint optionpurchase_dev_pkey primary key (oid),
    constraint optionpurchase_dev_uc1 unique (opid, dx)
);

alter table stockmarket_dev.optionpurchase add constraint optionpurchase_dev_opid_fkey foreign key (opid) references stockmarket_dev.optionx("oid");
alter table stockmarket_dev.optionpurchase add constraint optionpurchase_dev_status_fkey foreign key (purchase_type) references stockmarket.status_codes("oid");

-------------------- Option Sale -------------------- 

create table stockmarket_dev.optionsale (
	"oid" serial4 not null,
	purchase_id int4 not null,
	dx date default now() null,
	"price" stockmarket."price" not null,
	"volume" stockmarket."volume" not null,
	"status" stockmarket."status" default 0 null,
	transaction_cost stockmarket."price" default 0 not null,
	constraint optionsale_dev_pkey primary key (oid)
);

alter table stockmarket_dev.optionsale add constraint optionsale_dev_purchase_id_fkey foreign key (purchase_id) references stockmarket_dev.optionpurchase("oid");

-------------------- Critter -------------------- 

create table stockmarket_dev.critter (
	"oid" serial4 not null,
	"status" stockmarket."status" default 0 null,
	"critter_type" stockmarket."critter_type" null,
	sell_vol stockmarket."sell_volume" default 10 null,
	purchase_id int4 null,
	sale_id int4 null,
	constraint critter_pkey primary key (oid)
);

alter table stockmarket_dev.critter add constraint critter_dev_purchase_id_fkey foreign key (purchase_id) references stockmarket_dev.optionpurchase("oid");
alter table stockmarket_dev.critter add constraint critter_dev_sale_id_fkey foreign key (sale_id) references stockmarket_dev.optionsale("oid");
alter table stockmarket_dev.critter add constraint critter_dev_status_id_fkey foreign key ("status") references stockmarket.status_codes("oid");

-------------------- Accept Rule -------------------- 

create table stockmarket_dev.accept_rules (
	"oid" serial4 not null,
	value stockmarket."price" not null,
	rtyp int4 not null,
	active stockmarket."bool_type" default 'y'::bpchar null,
	description stockmarket."rule_desc" null,
	cid int4 null,
	constraint accept_rules_dev_pkey primary key (oid)
);

alter table stockmarket_dev.accept_rules add constraint acc_critter_dev_fkey foreign key (cid) references stockmarket_dev.critter("oid");
alter table stockmarket_dev.accept_rules add constraint accept_rules_type_dev_fkey foreign key (rtyp) references stockmarket.rule_types("oid") on delete restrict;

-------------------- Cleanup -------------------- 

delete from stockmarket.deny_rules;

delete from stockmarket.accept_rules;

delete from stockmarket.critter;

delete from stockmarket.optionpurchase;

--------------------- Test data --------------------- 

insert into stockmarket_dev.optionx (opname,strike,exp_date,optype,stock_id,series) values ('YAR7C800',800,'2027-03-19','c',3,'7C');

insert into stockmarket_dev.optionpurchase (opid,dx,price,volume,status,purchase_type,spot,buy) values (1,'2026-06-03',312.0,10,1,11,516.0,300.0);

insert into stockmarket_dev.critter (status,critter_type,sell_vol,purchase_id) values (7,1,10,1);

insert into stockmarket_dev.accept_rules (value,rtyp,cid) values (16.0,7,1);

-------------------- Migrations -------------------- 

insert into stockmarket.migrations (version,comment) values (1780991000,'Test tables optionpurchases');
