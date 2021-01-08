/*
This script creates MERGE stored procedures and queries
to populate fact and dimension tables from staged data.

Script created 12/30/2020 by Chris Peak
updated 1/8/2021 by Chris Peak
*/

-- merge_cost_burden_dim
	create procedure chas.merge_cost_burden_dim
	as
	with cte as (
		select distinct
			dd.[Cost burden] as cost_burden_name
		from stg.chas_data_dict dd 
	) 
	merge chas.cost_burden_dim as target
	using cte as source on (
		source.cost_burden_name = target.cost_burden_name
		)
	when not matched then insert (
		cost_burden_name
	) values (
		source.cost_burden_name
	);

-- merge_race_ethnicity_dim
	create procedure chas.merge_race_ethnicity_dim
	as
	with cte as (
		select distinct
			dd.[Race/ethnicity]  as race_ethnicity_name
		from stg.chas_data_dict dd 
	) 
	merge chas.race_ethnicity_dim as target
	using cte as source on (
		source.race_ethnicity_name = target.race_ethnicity_name 
	)
	when not matched then insert (
		race_ethnicity_name
	) values (
		source.race_ethnicity_name
	);

-- merge_tenure_dim
	create procedure chas.merge_tenure_dim
	as
	with cte as (
		select distinct
			dd.[Tenure]  as tenure_name
		from stg.chas_data_dict dd 
	) 
	merge chas.tenure_dim as target
	using cte as source on (
		source.tenure_name = target.tenure_name 
	)
	when not matched then insert (
		tenure_name
	) values (
		source.tenure_name
	);

-- ad-hoc query to add record to vintage_dim
--   using MERGE here instead of INSERT 
--   to guard against accidental duplicate runs.
	with cte (vintage_name, year_start, year_end) as (
		select '2013-2017 data', 2013, 2017
	)
	merge chas.vintage_dim as target
	using cte as source on 
	(
		source.vintage_name = target.vintage_name
		and source.year_start = target.year_start
		and source.year_end = target.year_end
	)
	when not matched then insert (vintage_name, year_start, year_end)
	values (source.vintage_name, 
		source.year_start,
		source.year_end
		);
	go

	with cte (vintage_name, year_start, year_end) as (
		select '2012-2016 data', 2012, 2016
	)
	merge chas.vintage_dim as target
	using cte as source on 
	(
		source.vintage_name = target.vintage_name
		and source.year_start = target.year_start
		and source.year_end = target.year_end
	)
	when not matched then insert (vintage_name, year_start, year_end)
	values (source.vintage_name, 
		source.year_start,
		source.year_end
		);
	go


create procedure chas.merge_estimate_facts_table_9
as
with cte as (
	select
		vin.vintage_dim_id,
		gd.geography_dim_id,
		td.tenure_dim_id,
		red.race_ethnicity_dim_id,
		cbd.cost_burden_dim_id,
		T9_est as estimate,
		T9_moe as margin_of_error
	from stg.chas_tbl_9_2016 t9
		join stg.chas_data_dict dd ON t9.[Column Name] = dd.[Column Name]
		join chas.tenure_dim td on dd.Tenure = td.tenure_name
		join chas.race_ethnicity_dim red on dd.[Race/ethnicity] = red.race_ethnicity_name
		join chas.cost_burden_dim cbd on dd.[Cost burden] = cbd.cost_burden_name
		join chas.vintage_dim vin on vin.vintage_name = '2012-2016 data'
		join census.geography_dim gd on cast(t9.geoid_short as varchar(20)) = gd.tract_geoid
	where gd.summary_level = 140
)
merge chas.estimate_facts_table_9 as target
using cte as source on (
	source.vintage_dim_id = target.vintage_dim_id
	and source.geography_dim_id = target.geography_dim_id
	and source.tenure_dim_id = target.tenure_dim_id
	and source.race_ethnicity_dim_id = target.race_ethnicity_dim_id
	and source.cost_burden_dim_id = target.cost_burden_dim_id
)
when not matched then insert (
		vintage_dim_id,
		geography_dim_id,
		tenure_dim_id,
		race_ethnicity_dim_id,
		cost_burden_dim_id,
		estimate,
		margin_of_error
) values (
		source.vintage_dim_id,
		source.geography_dim_id,
		source.tenure_dim_id,
		source.race_ethnicity_dim_id,
		source.cost_burden_dim_id,
		source.estimate,
		source.margin_of_error
);
