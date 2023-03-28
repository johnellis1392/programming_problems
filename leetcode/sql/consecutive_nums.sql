Create table If Not Exists Logs (id int, num int);
Truncate table Logs;
insert into Logs (id, num) values ('1', '1');
insert into Logs (id, num) values ('2', '1');
insert into Logs (id, num) values ('3', '1');
insert into Logs (id, num) values ('4', '2');
insert into Logs (id, num) values ('5', '1');
insert into Logs (id, num) values ('6', '2');
insert into Logs (id, num) values ('7', '2');

-- Write an SQL query to find all numbers that appear at least three times consecutively.
-- 
-- Return the result table in any order.
-- 
-- The query result format is in the following example.

select
  num as ConsecutiveNums,
  -- count(*) over (partition by num) as "rank"
  rank() over (order by id) as "rank"
from
  Logs
;

-- MySQL solution
select
  num,
  @seq_num :=
    case 
      when @prev = num then @seq_num
      else @seq_num + 1
    end as seq_num,
  @prev := num as prev
from
  logs,
  (select @seq_num := 0) _seq_num,
  (select @prev := -1) _prev
;

-- select
--   num,
--   seq_num,
--   prev
-- from 
--   Logs
-- ;

-- do $$
-- declare 
--   seq_num integer := 0;
--   prev integer := -1;
-- begin
--   raise notice '%, %', seq_num, prev;
-- end $$;

select distinct
  num as ConsecutiveNums
from Logs
where (id + 1, num) in (select * from Logs) and
  (id + 2, num) in (select * from Logs);

select distinct
  l1.num as ConsecutiveNums
from
  Logs l1
  join Logs l2 on l2.id = l1.id + 1
  join Logs l3 on l3.id = l1.id + 2
where l1.num = l2.num and l2.num = l3.num;
