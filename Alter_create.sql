alter table fudgemart_tiemsheet
	add
		constraint pk_timesheet_id
			primary key (timesheet_id),
		constraint fk_timesheet_employee_id
			foreign key
		(timesheet_employee_id) references
		fudgemart_employees(employee_id),
		constraint ck_timesheet_hours
			check
		(timesheet_hours between 0 and 60)
			
