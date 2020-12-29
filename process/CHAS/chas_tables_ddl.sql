/*
Working script for creating CHAS tables in Elmer
Created by: Chris Peak
Created on: 12/22/2020
*/

-- scratch pad
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

-- DDL statements
	drop table if exists chas.variable_dim
	go
	create table chas.variable_dim(
		variable_dim_id int identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		chas_table_number int,
		column_name varchar(20),
		line_type varchar(50),
		tenure varchar(50),
		race_ethnicity varchar(100),
		cost_burden varchar(100),
		constraint pk_chas_variable_dim primary key clustered (variable_dim_id)
	)

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

	drop table if exists chas.estimate_facts
	go
	create table chas.estimate_facts (
		estimate_fact_id int identity not null,
		created_at datetime2 not null default getdate(),
		updated_at datetime2 not null default getdate(),
		vintage_dim_id smallint not null, 
		geography_dim_id int not null, -- references census.geography_dim
		variable_dim_id int not null, -- need variable_dim (from chas_data_dict)
		estimate int,
		margin_of_error int,
		constraint pk_chas_estimate_facts primary key clustered (estimate_fact_id),
		constraint fk_chas_estimate_facts__vintage_dim FOREIGN KEY (vintage_dim_id)
			references chas.vintage_dim (vintage_dim_id),
		constraint fk_chas_estimate_facts__geography_dim FOREIGN KEY (geography_dim_id)
			references census.geography_dim (geography_dim_id),
		constraint fk_chas_estimate_facts__variable_dim FOREIGN KEY (variable_dim_id)
			references chas.variable_dim (variable_dim_id),
	)