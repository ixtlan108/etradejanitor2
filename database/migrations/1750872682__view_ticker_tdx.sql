create view stockmarket.v_ticker_dx as
select t.ticker, max(s.dx) from stockmarket.stockprice s 
join stockmarket.stocktickers t on t.oid = s.ticker_id
where t.status = 1

insert into stockmarket.migrations (version,comment) values (1750872682,'View ticker tdx');
