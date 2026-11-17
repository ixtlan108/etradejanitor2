--- new migration 2026-07-09 ---

into stockmarket.optionx (opname,strike,exp_date,optype,stock_id,series) values ('YAR6H440',440.00,'2026-08-21','c',3,'6H');
into stockmarket.optionx (opname,strike,exp_date,optype,stock_id,series) values ('YAR6T440',440.00,'2026-08-21','p',3,'6T');

-- call settl: 29.24, bid: 20.00, ask: 22.75
-- put settl: 15.11, bid: 20.75, ask: 23.75

into stockmarket_dev.optionpurchase(opid,dx,price,volume,status,purchase_type,spot,buy)
(26980,'2026-07-10',23.00,10,1,11,439.1,20.8);

into stockmarket_dev.optionpurchase(opid,dx,price,volume,status,purchase_type,spot,buy)
(26981,'2026-07-10',21.60,10,1,11,439.1,19.40);

into stockmarket_dev.critter (status,critter_type,sell_vol,purchase_id) values (7,1,10,1);
into stockmarket_dev.critter (status,critter_type,sell_vol,purchase_id) values (7,1,10,2);

insert into stockmarket_dev.accept_rules (value,rtyp,active,cid) values (5.0,7,'y',1);
insert into stockmarket_dev.accept_rules (value,rtyp,active,cid) values (5.0,7,'y',2);

insert into stockmarket.migrations (version,comment) values (1783598637,'Stockmarket dev test data');
