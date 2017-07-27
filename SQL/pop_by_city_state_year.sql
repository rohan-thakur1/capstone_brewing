CREATE MATERIALIZED VIEW wrk.pop_by_city_state_year AS
 SELECT a.id,
    a.year,
    a.city,
    a.state_code,
    sum(a.total) AS total,
    sum(a.total_under_21) AS total_under_21,
    sum(a.total) - sum(a.total_under_21) AS total_over_21,
    sum(a.total_18_to_29_male) AS total_18_to_29_male,
    sum(a.total_30_to_49_male) AS total_30_to_49_male,
    sum(a.total_50_to_64_male) AS total_50_to_64_male,
    sum(a.total_65_and_over_male) AS total_65_and_over_male
   FROM wrk.population_bin a
  WHERE a.id IS NOT NULL AND a.state_code::text <> 'PR'::text
  GROUP BY a.id, a.year, a.city, a.state_code
  ORDER BY a.state_code, a.city, a.year;