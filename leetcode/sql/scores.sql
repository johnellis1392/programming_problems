-- Problem:
-- Write an SQL query to rank the scores. The ranking should be calculated 
-- according to the following rules:
--
-- * The scores should be ranked from the highest to the lowest.
-- * If there is a tie between two scores, both should have the same ranking.
-- * After a tie, the next ranking number should be the next consecutive 
--   integer value. In other words, there should be no holes between ranks.
-- * Return the result table ordered by score in descending order.


create table Scores (
  id int primary key,
  score float
);
insert into Scores (id, score) values (1, 3.50);

-- My Solution
select Scores.score as score, s2.rank as "rank"
from
Scores
left join
(
  select s1.score, @rank := @rank + 1 as "rank"
  from (
    select distinct score
    from Scores
    order by score desc
  ) s1, (select @rank := 0) ranks
) s2
on Scores.score = s2.score
order by s2.rank asc
;


-- Another smart solution. Take each score
-- and calculate the number of scores higher than it,
-- then use the count of that result set as the rank.
select S.score, count(S2.score) as Rank
from Scores S,
(select distinct score from scores) S2
where S.score <= S2.score
group by S.id
order by S.score desc
;

-- Best solution, using DENSE_RANK() which is a window
-- function that creates a ranking over a set of data.
select
  score,
  dense_rank() over (order by score desc) "rank"
from Scores;