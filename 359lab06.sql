--Rebecca Candee
--3a)

create view v_fudgemart_employee_managers
as
select employee_id, employee_ssn, employee_lastname, employee_firstname, employee_jobtitle,
employee_department, employee_birthdate, employee_hiredate, employee_hourlywage,
employee_supervisor_id
from fudgemart_employees
where employee_id IN(select employee_supervisor_id from fudgemart_employees)

select * from v_fudgemart_employee_managers

select * from fudgemart_employees

--dont have termdate in table previously created - i did a select all from fudgemart employees and there are still 2 people missing. 

--3b 
select employee_firstname,employee_lastname, employee_jobtitle, employee_department, employee_hourlywage
from v_fudgemart_employee_managers
where employee_hourlywage > 17 and employee_supervisor_id is not null
order by employee_hourlywage