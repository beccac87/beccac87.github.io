create view unit6_activity_views
as
select first_name, last_name, favorite_movie
from student_info join student_movies on si_id = fk_student_info

select *
from unit6_activity_views
order by favorite_movie

select favorite_movie, count(favorite_movie)
from unit6_activity_views
group by favorite_movie
having count(favorite_movie) > 1

create_procedure u6_movies
as
begin
	select favorite_movie, count(favorite_movie)
	from unit6_activity_views
	group by favorite_movie
	having count(favorite_movie) > 1
end
go

execute u6_movies

create_procedure u6_add_movie
(
	@crazyname1 varchar(30),
	@mysiid int
)
as
begin
	insert into student_movies (favorite_movie, fk_student_info)
	values (@crazyname1, @mysiid)
end
go

execute u6_add_movie 'usual suspects', 4

select *
from student_movies
where fk_student_info = 4

create_procedure u6_add_name
(
	@firstname varchar(30),
	@mysiid int
)
as
begin
update student_info
set first_name = @firstname
where si_id = @mysiid
end
go

select = from student_info

execute u6_add_name 'Becca' , 50




