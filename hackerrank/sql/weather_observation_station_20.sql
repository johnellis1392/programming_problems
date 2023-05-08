create table station(
  id serial primary key,
  city varchar2(21),
  state varchar2(2),
  lat_n number,
  long_w number
);

-- Query the median of the dataset
set @rowindex := -1;

select round(lat_n, 4)
from (
  select
    @rowindex := @rowindex + 1 as rowindex,
    lat_n
  from station
  order by lat_n asc
) as t
where t.rowindex in
  (floor(@rowindex / 2), ceil(@rowindex / 2));