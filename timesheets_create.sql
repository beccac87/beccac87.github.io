create table fudgemart_tiemsheet (
	timesheet_id int identity(1,1) not null,
	timesheet_payrolldate datetime not null,
	timesheet_employee_id int not null,
	timesheet_hours decimal(3,1) default 40.0 not null
	)
