

drop table if exists ofm.jurisdiction_dim
go
create table ofm.jurisdiction_dim (
	jurisdiction_dim_id int identity not null,
	[created_at] [datetime2](7) NOT NULL default getdate(),
	[updated_at] [datetime2](7) NOT NULL default getdate(),
	jurisdiction_name nvarchar(100) null,
	county_name nvarchar(20) null,
	constraint pk_ofm_jurisdiction_dim primary key clustered (jurisdiction_dim_id)
)
go

CREATE TRIGGER [ofm].[trigger_ofm__jurisdiction_dim_update] on [ofm].[jurisdiction_dim]
	after update
	as
	begin
		UPDATE ofm.jurisdiction_dim
		set updated_at = GETDATE()
		WHERE jurisdiction_dim_id in (
			select distinct jurisdiction_dim_id from Inserted
		)
	end
GO

ALTER TABLE [ofm].[jurisdiction_dim] ENABLE TRIGGER [trigger_ofm__jurisdiction_dim_update]
GO


drop table if exists ofm.april_1_estimate_facts
go
CREATE TABLE [ofm].[april_1_estimate_facts](
	[estimate_fact_id] [bigint] IDENTITY(1,1) NOT NULL,
	[created_at] [datetime2](7) NOT NULL,
	[updated_at] [datetime2](7) NOT NULL,
	[estimate_year] [smallint] NOT NULL,
	[publication_dim_id] [smallint] NOT NULL,
	[jurisdiction_dim_id] [int] NOT NULL,
	[housing_units] [real] NULL,
	[occupied_housing_units] [real] NULL,
	[group_quarters_population] [real] NULL,
	[household_population] [real] NULL,
	[total_population] [real]
 CONSTRAINT [pk_ofm_april_1_estimate_facts] PRIMARY KEY CLUSTERED 
	( [estimate_fact_id] ASC )
)
GO

ALTER TABLE [ofm].[april_1_estimate_facts] ADD  DEFAULT (getdate()) FOR [created_at]
GO

ALTER TABLE [ofm].[april_1_estimate_facts] ADD  DEFAULT (getdate()) FOR [updated_at]
GO

ALTER TABLE [ofm].[april_1_estimate_facts]  WITH CHECK ADD  CONSTRAINT [fk_ofm_estimate_facts__jurisdiction_dim_id] FOREIGN KEY([jurisdiction_dim_id])
REFERENCES [ofm].[jurisdiction_dim] ([jurisdiction_dim_id])
GO

ALTER TABLE [ofm].[april_1_estimate_facts] CHECK CONSTRAINT [fk_ofm_estimate_facts__jurisdiction_dim_id]
GO

ALTER TABLE [ofm].[april_1_estimate_facts]  WITH CHECK ADD  CONSTRAINT [fk_ofm_estimate_facts__publication_dim_id] FOREIGN KEY([publication_dim_id])
REFERENCES [ofm].[publication_dim] ([publication_dim_id])
GO

ALTER TABLE [ofm].[april_1_estimate_facts] CHECK CONSTRAINT [fk_ofm_estimate_facts__publication_dim_id]
GO


CREATE TRIGGER [ofm].[trigger_ofm__april_1_estimate_facts_update] on [ofm].[april_1_estimate_facts]
	after update
	as
	begin
		UPDATE ofm.april_1_estimate_facts
		set updated_at = GETDATE()
		WHERE estimate_fact_id in (
			select distinct estimate_fact_id from Inserted
		)
	end
GO

ALTER TABLE [ofm].[april_1_estimate_facts] ENABLE TRIGGER [trigger_ofm__april_1_estimate_facts_update]
GO



go
create procedure ofm.merge_jurisdiction_dim_from_apr_intercensal
as
with cte as (
	select distinct 
	jurisdiction as jurisdiction_name,
	county as county_name
	from stg.ofm_apr_intercensal
)
merge ofm.jurisdiction_dim as target
using cte as source
on (source.jurisdiction_name = target.jurisdiction_name
	and source.county_name = target.county_name
	)
when not matched then insert (
	jurisdiction_name,
	county_name
) values (
	source.jurisdiction_name,
	source.county_name
);


alter procedure ofm.merge_april_1_estimate_facts_intercensal
as 
with cte as (
	select stg.estimate_year, 
		pd.publication_dim_id,
		jd.jurisdiction_dim_id,
		stg.housing_units,
		stg.occupied_housing_units,
		stg.group_quarters_population,
		stg.household_population,
		stg.total_population
	from stg.ofm_apr_intercensal stg
		join ofm.publication_dim pd on pd.publication_name =  '2020 April 1 Intercensal'
		join ofm.jurisdiction_dim jd on stg.jurisdiction = jd.jurisdiction_name
			and stg.county = jd.county_name
)
merge ofm.april_1_estimate_facts as target
using cte as source
on (
	source.publication_dim_id = target.publication_dim_id
	and source.jurisdiction_dim_id = target.jurisdiction_dim_id
	and source.estimate_year = target.estimate_year
)
when not matched then insert (
	estimate_year,
	publication_dim_id,
	jurisdiction_dim_id,
	housing_units,
	occupied_housing_units,
	group_quarters_population,
	household_population,
	total_population
) values (
	source.estimate_year,
	source.publication_dim_id,
	source.jurisdiction_dim_id,
	source.housing_units,
	source.occupied_housing_units,
	source.group_quarters_population,
	source.household_population,
	source.total_population
);

/*
exec ofm.merge_jurisdiction_dim_from_apr_intercensal
exec ofm.merge_april_1_estimate_facts_intercensal

select count(*) from ofm.april_1_estimate_facts

select top 10 *
from stg.ofm_apr_postcensal
select * from ofm.publication_dim
insert into ofm.publication_dim (publication_year, publication_name, publication_type)
values (2020, '2020 April 1 Postcensal','postcensal')
*/

create procedure ofm.merge_april_1_estimate_facts_postcensal_population
as 
with cte as (
	select stg.estimate_year, 
		pd.publication_dim_id,
		jd.jurisdiction_dim_id,
		stg.total_population
	from stg.ofm_apr_postcensal stg
		join ofm.publication_dim pd on pd.publication_name =  '2020 April 1 Postcensal'
		join ofm.jurisdiction_dim jd on stg.jurisdiction = jd.jurisdiction_name
			and stg.county = jd.county_name
)
merge ofm.april_1_estimate_facts as target
using cte as source
on (
	source.publication_dim_id = target.publication_dim_id
	and source.jurisdiction_dim_id = target.jurisdiction_dim_id
	and source.estimate_year = target.estimate_year
)
when matched 
		and (
			source.total_population <> target.total_population
			or target.total_population is null
		)
	then update 
		set target.total_population = source.total_population
when not matched then insert (
	estimate_year,
	publication_dim_id,
	jurisdiction_dim_id,
	total_population
) values (
	source.estimate_year,
	source.publication_dim_id,
	source.jurisdiction_dim_id,
	source.total_population
);

create procedure ofm.merge_april_1_estimate_facts_postcensal_housing
as
with cte as (
	select 
		s.estimate_year,
		pd.publication_dim_id,
		jd.jurisdiction_dim_id,
		s.total_housing_units as housing_units
	from stg.ofm_apr_postcensal_housing s
		join ofm.publication_dim pd on pd.publication_name = '2020 April 1 Postcensal'
		join ofm.jurisdiction_dim jd on s.jurisdiction = jd.jurisdiction_name
			and s.county = jd.county_name
)
merge ofm.april_1_estimate_facts as target
using cte as source
on (
	source.publication_dim_id = target.publication_dim_id
	and source.jurisdiction_dim_id = target.jurisdiction_dim_id
	and source.estimate_year = target.estimate_year
)
when matched 
		and (
			source.housing_units <> target.housing_units
			or target.housing_units is null
		)
	then update 
		set target.housing_units = source.housing_units
when not matched then insert (
	estimate_year,
	publication_dim_id,
	jurisdiction_dim_id,
	housing_units
) values (
	source.estimate_year,
	source.publication_dim_id,
	source.jurisdiction_dim_id,
	source.housing_units
);

exec ofm.merge_april_1_estimate_facts_postcensal_housing

