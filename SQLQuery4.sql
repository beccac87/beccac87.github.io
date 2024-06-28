select top 2 employee_firstname + ' ' + employee_lastname as employee_name,
employee_hourlywage
from fudgemart_employees
order by employee_hourlywage desc
