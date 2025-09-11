Create table If Not Exists Employee (id int, name varchar(255), salary int, managerId int);
Truncate table Employee;
insert into Employee (id, name, salary, managerId) values ('1', 'Joe', '70000', '3');
insert into Employee (id, name, salary, managerId) values ('2', 'Henry', '80000', '4');
insert into Employee (id, name, salary, managerId) values ('3', 'Sam', '60000', null);
insert into Employee (id, name, salary, managerId) values ('4', 'Max', '90000', null);

select
  c.name as Employee
from
  Employee c join Employee m
    on c.managerId = m.id
where
  -- c.managerId is not null -- Don't need to check null because the salary check does it for us
  c.salary > m.salary
  ;