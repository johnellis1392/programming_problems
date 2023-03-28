Create table If Not Exists Logs (id int, num int);
Truncate table Logs;
insert into Logs (id, num) values ('1', '1');
insert into Logs (id, num) values ('2', '1');
insert into Logs (id, num) values ('3', '1');
insert into Logs (id, num) values ('4', '2');
insert into Logs (id, num) values ('5', '1');
insert into Logs (id, num) values ('6', '2');
insert into Logs (id, num) values ('7', '2');

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