-- Select the cities with the smallest name and the largest
-- name, ordering by name if there are multiple.
--
-- Schema:
-- Station
-- * id number
-- * city varchar2(21)
-- * state varchar2(2)
-- * lat_n number
-- * long_w number

(
  select city, length(city)
  from station
  order by length(city) asc, city
  limit 1
)
union
(
  select city, length(city)
  from station
  order by length(city) desc, city
  limit 1
);