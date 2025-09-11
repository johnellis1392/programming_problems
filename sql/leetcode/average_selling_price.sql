Create table If Not Exists Prices (product_id int, start_date date, end_date date, price int);
Create table If Not Exists UnitsSold (product_id int, purchase_date date, units int);
Truncate table Prices;
insert into Prices (product_id, start_date, end_date, price) values ('1', '2019-02-17', '2019-02-28', '5');
insert into Prices (product_id, start_date, end_date, price) values ('1', '2019-03-01', '2019-03-22', '20');
insert into Prices (product_id, start_date, end_date, price) values ('2', '2019-02-01', '2019-02-20', '15');
insert into Prices (product_id, start_date, end_date, price) values ('2', '2019-02-21', '2019-03-31', '30');
Truncate table UnitsSold;
insert into UnitsSold (product_id, purchase_date, units) values ('1', '2019-02-25', '100');
insert into UnitsSold (product_id, purchase_date, units) values ('1', '2019-03-01', '15');
insert into UnitsSold (product_id, purchase_date, units) values ('2', '2019-02-10', '200');
insert into UnitsSold (product_id, purchase_date, units) values ('2', '2019-03-22', '30');

-- Write an SQL query to find the average selling price for each product. 
-- average_price should be rounded to 2 decimal places.
-- 
-- Return the result table in any order.
-- 
-- The query result format is in the following example.

select
  t.product_id as product_id,
  round(sum(t.value) / sum(t.units), 2) as average_price
from (
  select
    u.product_id,
    u.units,
    (u.units * p.price) as value
  from UnitsSold u join Prices p
    on u.product_id = p.product_id
    and u.purchase_date between p.start_date and p.end_date
) as t
group by t.product_id
;

select
  u.product_id as product_id,
  round(sum(p.price * u.units) / sum(u.units), 2) as average_price
from UnitsSold u join Prices p
  on u.product_id = p.product_id
  and u.purchase_date between p.start_date and p.end_date
group by u.product_id;