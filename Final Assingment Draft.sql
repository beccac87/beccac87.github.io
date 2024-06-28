--Rebecca Candee
--1.1 Drop All
if exists (select * from sys.objects where name = 'project')
	drop table project
if exists (select * from sys.objects where name = 'task')
	drop table task
if exists (select * from sys.objects where name = 'employee')
	drop table employee
if exists (select * from sys.objects where name = 'client')
	drop table client

	drop table task

--1.2 TABLE project
CREATE TABLE project
(
	project_id int identity NOT null,
	project_name varchar(30) NOT null,
	project_description varchar(255) NOT null,
	project_start_date date default getdate() NOT null,
	project_estimate_hours int default(1) NOT null
)

--1.3 TABLE task
CREATE TABLE task
(
	task_id int identity NOT null,
	task_name varchar(30) NOT null,
	task_description varchar(255) NOT null,
	task_estimate_hours int default(1) NOT null,
	task_actual_hours int NOT null,
	task_start_date date default getdate() NOT null,
	task_end_date date null,
	task_status varchar(6) NOT null,
	task_priority varchar(10) NOT null,
	task_billable_ammount money null,
	task_required_skills varchar(50) not null,
	fk_project_id int null,
	fk_employee_id int null
)

--1.4 TABLE employee
CREATE TABLE employee
(
	employee_id int identity NOT null,
	employee_fname varchar(30) NOT null,
	employee_lname varchar (30) NOT null,
	employee_title varchar (30) NOT null,
	employee_email varchar(255) NOT null,
	employee_phone varchar(20) NOT null,
	employee_rate money default(0) not null,
	employee_skills varchar (30) not null
)

--1.5 TABLE client
CREATE TABLE client
(
	client_id int identity NOT null,
	client_fname varchar(30) NOT null,
	client_lname varchar (30) NOT null,
	client_company varchar(30) NOT null,
	client_email varchar(255) NOT null,
	client_phone varchar(20) NOT null,
)

--2.1 TABLE project
ALTER TABLE project
	add
	constraint pk_project_id primary key (project_id),
	constraint u_project_name unique (project_name)

--2.2 TABLE task
ALTER TABLE task
	add
	constraint pk_task_id primary key (task_id)

--2.3 TABLE employee
ALTER TABLE employee
	add
	constraint pk_tabantha_employee_id primary key (employee_id),
	constraint u_employee_email unique (employee_email)

--2.3 TABLE client
ALTER TABLE client
	add constraint pk_client_id primary key (client_id),
	constraint u_client_email unique (client_email)

--3.1 TABLE task
ALTER TABLE task
	add
	constraint fk_project_id foreign key (fk_project_id)
	references project(project_id)

ALTER TABLE task
	add
	constraint fk_employee_id foreign key (fk_employee_id)
	references employee(employee_id)

--3.1 TABLE employee

--4.1 TABLE project
INSERT into project (project_name, project_description, project_estimate_hours)
	VALUES
	('Home Depot App', 'Space measurement smartphone app development', 40)

Select * from project
Select * from task
Select * from employee
Select * from client


