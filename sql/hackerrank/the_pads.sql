create table Occupations (
  name varchar,
  occupation varchar
);

insert into Occupations (name, occupation) values ('Samantha', 'Doctor');
insert into Occupations (name, occupation) values ('Julia', 'Actor');
insert into Occupations (name, occupation) values ('Maria', 'Actor');
insert into Occupations (name, occupation) values ('Meera', 'Singer');
insert into Occupations (name, occupation) values ('Ashely', 'Professor');
insert into Occupations (name, occupation) values ('Ketty', 'Professor');
insert into Occupations (name, occupation) values ('Christeen', 'Professor');
insert into Occupations (name, occupation) values ('Jane', 'Actor');
insert into Occupations (name, occupation) values ('Jenny', 'Doctor');
insert into Occupations (name, occupation) values ('Priya', 'Singer');


select concat(name, '(', substring(occupation, 1, 1), ')')
from Occupations;

select concat('There are a total of ', count(name), ' ', lower(occupation), 's.')
from Occupations
group by occupation;


(
  select concat(name, '(', substring(occupation, 1, 1), ')') v
  from Occupations
  order by name
) union all (
  select concat('There are a total of ', count(name), ' ', lower(occupation), 's.') v
  from Occupations
  group by occupation
  order by count(name) asc, occupation asc
) order by v;

select
  doctors.name as Doctor,
  actors.name as Actor,
  professors.name as Professor,
  singers.name as Singer
from
  (select row_number() over (order by name asc) as row_id, name from occupations where occupation = 'Doctor') as doctors
  left outer join (select row_number() over (order by name asc) as row_id, name from occupations where occupation = 'Actor') as actors
    on doctors.row_id = actors.row_id
  left outer join (select row_number() over (order by name asc) as row_id, name from occupations where occupation = 'Professor') as professors
    on actors.row_id = professors.row_id
  left outer join (select row_number() over (order by name asc) as row_id, name from occupations where occupation = 'Singer') as singers
    on professors.row_id = singers.row_id;

select occupation, group_concat(occupations.name) as names
from occupations
group by occupation;
