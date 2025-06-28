drop view stockmarket.v_ticker_dx;

create view stockmarket.v_ticker_dx as
select t.oid,t.ticker,t.status, max(s.dx) from stockmarket.stockprice s 
join stockmarket.stocktickers t on t.oid = s.ticker_id
group by t.oid,t.ticker,t.status;
