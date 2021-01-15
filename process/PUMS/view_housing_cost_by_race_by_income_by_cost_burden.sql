alter VIEW census.v_pums_households_by_rent_cost_burden_by_income_by_race
as
-- This returns the weighted household count from PUMS data
-- by the proportion of gross rent as a percentage of household income (GRPIP)
-- by household income (HINCP)
-- by race/ethnicity of the householder 
--   (reflecting RAC1P for race and HISP for hispanic origin)
--   Note that due to the fact that the "Hispanic" summaries reflect HISP values
--     whereas other racial categories were defined through RAC1P, some households
--     are reflected in both the "Hispani" totals and the other racial categories.

-- Script created by Chris Peak, 1/15/2021
-- based off of Neil Kilgren's spreadsheet '33_pums_cost_burden_R-E_2018.xlsx'
-- with working file PUMS2018-rent.xlsx
--    available at J:\Staff\Neil\Growth_Mgmt_Support\VISION2050\Regional Housing Strategy\Housing Background Paper\2018
with base_pums_housing as (
	select *
	from census.pums_housing_2019 -- UPDATE THIS FOR NEW PUMS DATA
), base_pums_persons as (
	select *
	from census.pums_persons_2019 -- UPDATE THIS FOR NEW PUMS DATA
), hhlds as (
	select
		h.serialno,
		h.hincp,
		case
			when h.hincp < 20000 then '< $20,000'
			when h.hincp < 35000 then '$20K - $34,999'
			when h.hincp < 50000 then '35K - $49,999'
			when h.hincp < 75000 then '50k - $74,999'
			when h.hincp < 100000 then '75k - $99,999'
			when h.hincp >= 100000 then '$100k or more'
		end as hincp_class,
		case
			when h.hincp < 20000 then 1
			when h.hincp < 35000 then 2
			when h.hincp < 50000 then 3
			when h.hincp < 75000 then 4
			when h.hincp < 100000 then 5
			when h.hincp >= 100000 then 6
		end as hincp_order,
		case 
			when h.grpip < 30 then '<=30%'
			when h.grpip < 50 then '30% <= 50%'
			when h.grpip >=50 then '>50%'
		end as cost_burden_class,
		case 
			when h.grpip < 30 then 1
			when h.grpip < 50 then 2
			when h.grpip >=50 then 3
		end as cost_burden_order,
		h.wgtp
	from base_pums_housing h
	where isnull(h.hincp, 0) > 0
		and rntp > 0
		and cast(h.puma as varchar(20)) in ( 
			select distinct gd.puma_code from census.geography_dim gd
			where gd.county_name in ('King', 'Kitsap', 'Snohomish', 'Pierce')
			)
), p as (
	select 
		p.serialno,
		case 
			when p.rac1p = 'Alaska Native alone' then 'American Indian'
			when p.rac1p like ('American Indian %') then 'American Indian'
			else p.rac1p
		end as race,
		case p.hisp
			when 'Not Spanish/Hispanic/Latino' then 'No'
			else 'Yes'
		end as hispanic,
		p.pwgtp
	from base_pums_persons p
	where p.sporder = 1
), households_by_cost_burden_by_income_by_race as (
	select 
		p.race,
		hincp_class, 
		h.hincp_order,
		h.cost_burden_class,
		h.cost_burden_order,
		sum(h.wgtp) as weighted_housing_count
	from hhlds as h 
		join p on h.serialno = p.serialno
	group by p.race, h.hincp_class, h.hincp_order, h.cost_burden_class, h.cost_burden_order
	union
	select 
		'Total Population' as race,
		h.hincp_class,
		h.hincp_order,
		h.cost_burden_class,
		h.cost_burden_order,
		sum(h.wgtp) as weighted_housing_count
	from hhlds as h 
		join p on h.serialno = p.serialno
	group by h.hincp_class, h.hincp_order, h.cost_burden_class, h.cost_burden_order
	union
	select 
		'Hispanic' as race,
		h.hincp_class,
		h.hincp_order,
		h.cost_burden_class,
		h.cost_burden_order,
		sum(h.wgtp) as weighted_housing_count
	from hhlds as h 
		join p on h.serialno = p.serialno
	where p.hispanic = 'Yes'
	group by h.hincp_class, h.hincp_order, h.cost_burden_class, h.cost_burden_order
)
select *
from households_by_cost_burden_by_income_by_race
