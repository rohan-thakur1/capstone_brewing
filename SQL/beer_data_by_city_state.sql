drop materialized view if exists wrk.beer_data_by_city_state;
create materialized view wrk.beer_data_by_city_state as
select
	 l.id
	,l.city
	,l.state_code
	,count(distinct br.id) brewery_count
	,count(distinct b.id) beer_count
	,count(distinct r1.id) beer_review_count
	,count(rc.resident_review_count) user_review_count
	,max(p2011.total) total_pop_2011
	,max(p2011.total_over_21) pop_over_21_2011
	,max(p2012.total) total_pop_2012
	,max(p2012.total_over_21) pop_over_21_2012
	,max(p2013.total) total_pop_2013
	,max(p2013.total_over_21) pop_over_21_2013
	,max(p2014.total) total_pop_2014
	,max(p2014.total_over_21) pop_over_21_2014
	,max(p2015.total) total_pop_2015
	,max(p2015.total_over_21) pop_over_21_2015
from wrk.beer b
left outer join wrk.brewery br
  on b.brewery_id = br.id
left outer join wrk.location l
  on br.location_id = l.id
left outer join wrk.review r1
  on r1.beer_id = b.id
left outer join 
(
	select
		 l.id location_id
		,count(r.id) resident_review_count
	from wrk.location l
	left outer join wrk.review r
	  on l.id = r.location_id
	group by l.id
	having count(r.id) > 0
) rc
  on l.id = rc.location_id
left outer join 
(
	select *
	from wrk.pop_by_city_state_year p2011
	where p2011."year" = 2011
) p2011
  on l.id = p2011.id
left outer join 
(
	select *
	from wrk.pop_by_city_state_year p2012
	where p2012."year" = 2012
) p2012
  on l.id = p2012.id
left outer join 
(
	select *
	from wrk.pop_by_city_state_year p2013
	where p2013."year" = 2013
) p2013
  on l.id = p2013.id
left outer join 
(
	select *
	from wrk.pop_by_city_state_year p2014
	where p2014."year" = 2014
) p2014
  on l.id = p2014.id
left outer join 
(
	select *
	from wrk.pop_by_city_state_year p2015
	where p2015."year" = 2015
) p2015
  on l.id = p2015.id
where l.id is not null
group by l.id, l.city, l.state_code
order by l.state_code, l.city;
