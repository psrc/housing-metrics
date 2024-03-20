-- Add columns to geography_dim table
	alter table census.geography_dim
	add housing_subarea_2020 varchar(50) null

-- Insert records into staging table for the three tracts that aren't represented
-- in the source file.  These were left off presumably because they have no population,
-- but should be in the dimension anyway.
	INSERT INTO stg.tract_subareas(GEOID10, County, Subarea)
	VALUES ('53061990002', 'Snohomish', 'No Subarea Defined' ),
		('53035990100','Kitsap', 'No Subarea Defined' ),
		('53033990100', 'King', 'No Subarea Defined')


-- Update geography_dim.housing_subarea_2020 from the staged data
	;with cte as (
		select gd.geography_dim_id, 
			gd.geography_type,
			ts.subarea as housing_subarea_2020
		from stg.tract_subareas ts
			join census.geography_dim gd on cast(ts.geoid10 as varchar(20)) = gd.tract_geoid
	)
	merge census.geography_dim as target
	using cte as source on source.geography_dim_id = target.geography_dim_id
	when matched 
			and isnull(target.housing_subarea_2020, '') <> source.housing_subarea_2020
		then update set target.housing_subarea_2020 = source.housing_subarea_2020
	;
