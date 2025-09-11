create table Employee (
  employee_id serial,
  name text,
  months integer,
  salary integer
);

insert into Employee
(employee_id, name, months, salary)
values
(1, 'Rose', 15, 1968),
(2, 'Angela', 1, 3443),
(3, 'Frank', 17, 1608),
(4, 'Patrick', 7, 1345),
(5, 'Lisa', 11, 2330),
(6, 'Kimberly', 16, 4372),
(7, 'Bonnie', 8, 1771),
(8, 'Michael', 6, 2017),
(9, 'Todd', 5, 3396),
(10, 'Joe', 9, 3573);

select concat(t.n, ' ', count(*))
from (
  select months * salary as n
  from Employee
) as t
group by t.n
order by t.n desc
limit 1;