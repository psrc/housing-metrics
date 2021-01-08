/*
Working script for creating CHAS tables in Elmer
Created by: Chris Peak
Created on: 12/22/2020
*/

-- scratch pad
/*
	select top 100 *
	from stg.chas_data_dict

	select top 100 *
	from stg.chas_tbl_9 c
	where c.GEOID_short = 53033000100

	select sum(t9_est)
	from stg.chas_tbl_9 c
	where c.GEOID_short = 53033000100
		and c.measurement_id between 4 and 7

	select count(*)
	from stg.chas_tbl_9 c
*/
-- DDL statements
	-- first, get rid of the fact table with all its foreign key constraints
	drop table if exists chas.estimate_facts
	go

	drop table if exists chas.variable_dim
	go
	/*
	create table chas.variable_dim(
		variable_dim_id int identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		chas_table_number int,
		variable_name varchar(20),
		line_type varchar(50),
		tenure varchar(50),
		race_ethnicity varchar(100),
		cost_burden varchar(100),
		constraint pk_chas_variable_dim primary key clustered (variable_dim_id)
	)

	-- trigger
		CREATE TRIGGER [chas].[trigger_chas__variable_dim_update] on [chas].[variable_dim]
		after update
		as
		begin
			UPDATE chas.variable_dim
			set updated_at = GETDATE()
			WHERE variable_dim_id in (
				select distinct variable_dim_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[variable_dim] ENABLE TRIGGER [trigger_chas__variable_dim_update]
		GO
	*/

-- tenure_dim
	drop table if exists chas.tenure_dim 
	go
	create table chas.tenure_dim(
		tenure_dim_id smallint identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		tenure_name nvarchar(100),
		tenure_order smallint,
		constraint pk_chas_tenure_dim primary key clustered (tenure_dim_id)
	)
	go
		CREATE TRIGGER [chas].[trigger_chas__tenure_dim_update] on [chas].[tenure_dim]
		after update
		as
		begin
			UPDATE chas.tenure_dim
			set updated_at = GETDATE()
			WHERE tenure_dim_id in (
				select distinct tenure_dim_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[tenure_dim] ENABLE TRIGGER [trigger_chas__tenure_dim_update]
		GO

--race_ethnicity_dim
	drop table if exists chas.race_ethnicity_dim
	go
	create table chas.race_ethnicity_dim (
		race_ethnicity_dim_id smallint identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		race_ethnicity_name nvarchar(100),
		constraint pk_chas_race_ethnicity_dim primary key clustered (race_ethnicity_dim_id)
	)
	go

		CREATE TRIGGER [chas].[trigger_chas__race_ethnicity_dim_update] on [chas].[race_ethnicity_dim]
		after update
		as
		begin
			UPDATE chas.race_ethnicity_dim
			set updated_at = GETDATE()
			WHERE race_ethnicity_dim_id in (
				select distinct race_ethnicity_dim_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[race_ethnicity_dim] ENABLE TRIGGER [trigger_chas__race_ethnicity_dim_update]
		GO

--cost_burden_dim
	drop table if exists chas.cost_burden_dim
	go
	create table chas.cost_burden_dim (
		cost_burden_dim_id smallint identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		cost_burden_name nvarchar(100),
		cost_burden_order int,
		constraint pk_chas_cost_burden_dim primary key clustered (cost_burden_dim_id)
	)
	go

		CREATE TRIGGER [chas].[trigger_chas__cost_burden_dim_update] on [chas].[cost_burden_dim]
		after update
		as
		begin
			UPDATE chas.cost_burden_dim
			set updated_at = GETDATE()
			WHERE cost_burden_dim_id in (
				select distinct cost_burden_dim_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[cost_burden_dim] ENABLE TRIGGER [trigger_chas__cost_burden_dim_update]
		GO

--vintage_dim
	drop table if exists chas.vintage_dim
	go
	create table chas.vintage_dim(
		vintage_dim_id smallint identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		vintage_name nvarchar(100),
		year_start smallint,
		year_end smallint, 
		constraint pk_chas_vintage_dim primary key clustered (vintage_dim_id)
	)

	-- trigger
		CREATE TRIGGER [chas].[trigger_chas__vintage_dim_update] on [chas].[vintage_dim]
		after update
		as
		begin
			UPDATE chas.vintage_dim
			set updated_at = GETDATE()
			WHERE vintage_dim_id in (
				select distinct vintage_dim_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[vintage_dim] ENABLE TRIGGER [trigger_chas__vintage_dim_update]
		GO

-- estimate_facts_table_9
	drop table if exists chas.estimate_facts_table_9
	go
	create table chas.estimate_facts_table_9 (
		estimate_fact_id int identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		vintage_dim_id smallint not null, 
		geography_dim_id int not null, -- references census.geography_dim
		tenure_dim_id smallint not null,
		race_ethnicity_dim_id smallint not null,
		cost_burden_dim_id smallint not null,
		estimate int,
		margin_of_error int,
		constraint pk_chas_estimate_facts_table_9 primary key clustered (estimate_fact_id),
		constraint fk_chas_estimate_facts_table_9__vintage_dim FOREIGN KEY (vintage_dim_id)
			references chas.vintage_dim (vintage_dim_id),
		constraint fk_chas_estimate_facts_table_9__geography_dim FOREIGN KEY (geography_dim_id)
			references census.geography_dim (geography_dim_id),
		constraint fk_chas_estimate_facts_table_9__tenure_dim FOREIGN KEY (tenure_dim_id)
			references chas.tenure_dim (tenure_dim_id),
		constraint fk_chas_estimate_facts_table_9__race_ethnicity_dim FOREIGN KEY (race_ethnicity_dim_id)
			references chas.race_ethnicity_dim (race_ethnicity_dim_id),
		constraint fk_chas_estimate_facts_table_9__cost_burden_dim FOREIGN KEY (cost_burden_dim_id)
			references chas.cost_burden_dim (cost_burden_dim_id)
	)
	go

	-- trigger
		CREATE TRIGGER [chas].[trigger_chas__estimate_facts_table_9_update] on [chas].[estimate_facts_table_9]
		after update
		as
		begin
			UPDATE chas.estimate_facts_table_9
			set updated_at = GETDATE()
			WHERE estimate_fact_id in (
				select distinct estimate_fact_id from Inserted
			)
		end
		GO

		ALTER TABLE [chas].[estimate_facts_table_9] ENABLE TRIGGER [trigger_chas__estimate_facts_table_9_update]
		GO