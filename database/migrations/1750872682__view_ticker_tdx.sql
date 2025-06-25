create view stockmarket.v_ticker_dx as
select t.ticker, max(s.dx) from stockmarket.stockprice s 
join stockmarket.stocktickers t on t.oid = s.ticker_id
where t.status = 1
group by t.ticker;
