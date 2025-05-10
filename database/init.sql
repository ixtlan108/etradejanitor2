create table stockmarket.migrations (
  version int4 not null primary key,
  comment varchar(50) null
);

insert into
  stockmarket.migrations (version, comment)
values
  (1746968400, 'Bootstrapping');
