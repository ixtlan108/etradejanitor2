--- new migration 2026-07-09 ---

insert into stockmarket.optionx (opname,strike,exp_date,optype,stock_id,series) values ('YAR6H440',440.00,'2026-08-21','c',3,'6H');

-- 26980

-- optionpurchase
-- oid | opid  |     dx     | price | volume | status | transaction_cost | purchase_type | spot  | buy
--  47 | 26979 | 2019-02-14 |  5.80 |     10 |      1 |             0.00 |            11 | 34.70 | 5.10
-- spot 437.70
-- call settl: 29.24, bid: 20.00, ask: 22.75
-- put settl: 15.11, bid: 20.75, ask: 23.75

-- critter
-- oid | status | critter_type | sell_vol | purchase_id | sale_id
--  45 |      7 |            1 |       10 |          47 |

-- accept_rules
-- oid | value | rtyp | active | description | cid
-- 72 |  3.00 |    1 | y      |             |  45

-- insert into art.migrations (version,comment) values (1783598637,'Stockmarket dev test data');
