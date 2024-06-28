select avg(number_pets) as number_pets,
count(number_languages) as number_langs,
max(number_siblings) as number_siblings
from student_info

select brownies_pref, count(brownies_pref)
from student_info
group by brownies_pref