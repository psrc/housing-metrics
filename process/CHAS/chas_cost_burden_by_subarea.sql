
create function chas.cost_burdens_by_tenure_by_race_by_subarea(@data_vintage_name nvarchar(100))
returns table
as
return
select td.tenure_name as tenure,
	td.tenure_order,
	rd.race_ethnicity_name as race_ethnicity,
	cd.cost_burden_name as cost_burden,
	cd.cost_burden_order,
	gd.housing_subarea_2020,
	gd.county_fips,
	sum(ef.estimate) as estimate
from chas.estimate_facts_table_9 ef
	join chas.vintage_dim vin on ef.vintage_dim_id = vin.vintage_dim_id
	join census.geography_dim gd on ef.geography_dim_id = gd.geography_dim_id
	join chas.tenure_dim td on ef.tenure_dim_id = td.tenure_dim_id
	join chas.race_ethnicity_dim rd on ef.race_ethnicity_dim_id = rd.race_ethnicity_dim_id
	join chas.cost_burden_dim cd on ef.cost_burden_dim_id = cd.cost_burden_dim_id
where vin.vintage_name = @data_vintage_name
group by td.tenure_name, rd.race_ethnicity_name, cd.cost_burden_name, gd.housing_subarea_2020,
td.tenure_order, cd.cost_burden_order, gd.county_fips
go

create view chas.v_cost_burdens_by_tenure_by_race_by_subarea_2012_2016
as 
select d.*
from chas.cost_burdens_by_tenure_by_race_by_subarea('2012-2016 data') d
where d.housing_subarea_2020 <> 'No Subarea Defined'
go

create view chas.v_cost_burdens_by_tenure_by_race_by_subarea_2013_2017
as 
select d.*
from chas.cost_burdens_by_tenure_by_race_by_subarea('2013-2017 data') d
where d.housing_subarea_2020 <> 'No Subarea Defined'
go