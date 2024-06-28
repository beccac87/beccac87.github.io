select employee_firstname,
employee_lastname,
employee_department,
employee_hourlywage
from fudgemart_employees
where employee_department = 'Sporting Goods' and employee_hourlywage < 11