
go
create function ofm.housing_production_by_county(@publication_name nvarchar(200))
returns table 
/*
	Inputs:
		@publication_name: The name of a publication (or batch) of data published by 
			OFM.  This value must match a value in OFM.publication_dim.publication_name.
	
	Outputs:
		A table listing the number of housing units added from the preceding year, per year by county.

	Function created 1/5/2021
	Author: Chris Peak
*/
as
return
(
	with cte as (
		select jd.county_name, f.estimate_year, sum(f.housing_units) as housing
		from ofm.april_1_estimate_facts f
			join ofm.jurisdiction_dim jd on f.jurisdiction_dim_id = jd.jurisdiction_dim_id
			join ofm.publication_dim pd on f.publication_dim_id = pd.publication_dim_id
		where pd.publication_name = @publication_name
		group by jd.county_name, f.estimate_year
	)
	select
		cte2.county_name, 
		cte2.estimate_year as end_year,
		cte2.housing - cte1.housing as added_housing
	from cte as cte1
		join cte as cte2 ON cte1.county_name = cte2.county_name
			and cte1.estimate_year = cte2.estimate_year -1
)
/*********
examples
*********/
	select *
	from ofm.housing_production_by_county('2020 April 1 Postcensal')
	order by end_year, county_name

	-- pivot the years
	select *
	from ofm.housing_production_by_county('2020 April 1 Postcensal') as src
	pivot
	( sum(added_housing) for end_year in 
		([2011], [2012],[2013],[2014],[2015],[2016],[2017],[2018],[2019],[2020])
	) as pvt
	order by county_name