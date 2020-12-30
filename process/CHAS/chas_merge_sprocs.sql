/*
This script creates MERGE stored procedures and queries
to populate fact and dimension tables from staged data.

Script created 12/30/2020 by Chris Peak
*/

create procedure chas.merge_variable_dim
as
with cte as (
	select distinct
		9 as chas_table_number,
		dd.[Column Name] as variable_name,
		dd.line_type,
		dd.tenure,
		dd.[Race/ethnicity] as race_ethnicity,
		dd.[Cost burden] as cost_burden
	from stg.chas_data_dict dd 
) 
merge chas.variable_dim as target
using cte as source on (
	source.chas_table_number = target.chas_table_number
	and source.variable_name = target.variable_name
	)
when not matched then insert (
		chas_table_number,
		variable_name,
		line_type,
		tenure,
		race_ethnicity,
		cost_burden
) values (
		source.chas_table_number,
		source.variable_name,
		source.line_type,
		source.tenure,
		source.race_ethnicity,
		source.cost_burden
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


create procedure chas.merge_estimate_facts
as
with cte as (
	select
		vin.vintage_dim_id,
		gd.geography_dim_id,
		vd.variable_dim_id,
		T9_est as estimate,
		T9_moe as margin_of_error
	from stg.chas_tbl_9 t9
		join chas.variable_dim vd ON t9.[Column Name] = vd.variable_name
		join chas.vintage_dim vin on vin.vintage_name = '2013-2017 data'
		join census.geography_dim gd on cast(t9.geoid_short as varchar(20)) = gd.tract_geoid
	where gd.summary_level = 140
)
merge chas.estimate_facts as target
using cte as source on (
	source.vintage_dim_id = target.vintage_dim_id
	and source.geography_dim_id = target.geography_dim_id
	and source.variable_dim_id = target.variable_dim_id
)
when not matched then insert (
		vintage_dim_id,
		geography_dim_id,
		variable_dim_id,
		estimate,
		margin_of_error
) values (
		source.vintage_dim_id,
		source.geography_dim_id,
		source.variable_dim_id,
		source.estimate,
		source.margin_of_error
);
