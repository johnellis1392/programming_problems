create table employees (
  id serial,
  name text,
  salary integer
);

insert into employees (name, salary) values
('Ashley', 2340),
('Julia', 1198),
('Britney', 9009),
('Kristeen', 2341),
('Dyana', 9990),
('Diana', 8011),
('Jenny', 2341),
('Christeen', 2342),
('Meera', 2343),
('Priya', 2344),
('Priyanka', 2345),
('Paige', 2346),
('Jane', 2347),
('Belvet', 2348),
('Scarlet', 2349),
('Salma', 9087),
('Amanda', 7777),
('Aamina', 5500),
('Amina', 2570),
('Ketty', 2007);

select 
  ceil( 
    avg(salary) -
    avg(cast(replace(cast(salary as char(10)), '0', '') as signed))
  ) as temp
from employees;