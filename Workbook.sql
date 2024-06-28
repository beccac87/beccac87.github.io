
/* ******************* */
/* 1.2                 */
/* ******************* */

Rebecca Candee

GO
/* ******************* */
/* 1.3                 */
/* ******************* */

create table employment
state varchar(2)not null,
employment_month varchar(12)not null,
employed_number int,
unemployed int,
emp_state varchar(50)not null,
jobs_number int,
job_sector varchar(3)not null

constraint pk_job_sector primary key(job_sector),
constraint fk_emp_state foreign key(emp_state) references state(emp_state);

GO
/* ******************* */
/* 1.5                 */
/* ******************* */

insert into f_employment
	
	select distinct employment_month, employed_number, state, uemployed
	from employment

insert into f_jobs

	select distinct emp_state, jobs_number, jobs_sector
	from employment 


GO
/* ******************* */
/* 1.6                 */
/* ******************* */
select distinct COUNT (*)from jobs_number
select COUNT (*)from employment

select distinct COUNT (*)from employed_number
select COUNT (*)from employment
GO
