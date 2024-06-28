--2a creating a stored procedure
create procedure p_fudgemart_add_product
(
	@product_department varchar(20),
	@product_name varchar(50),
	@product_retail_price money,
	@product_wholesale_price money,
	@product_vendor_id int
)
as
begin
	insert into fudgemart_products (
		product_department, product_name, product_retail_price,
		product_wholesale_price, product_is_active, product_add_date,
		product_vendor_id
	) values (
		@product_department, @product_name, @product_retail_price,
		@product_wholesale_price, 1, GETDATE(),
		@product_vendor_id
	)
	return @@identity
end

select routine_name, routine_type, routine_definition
	from information_schema.routines
	where routine_name = 'p_fudgemart_add_product'

--2b
execute p_fudgemart_add_product 'Hardware','Monkey-Wrench',12.95,6.95,3

select top 1 * from fudgemart_products order by product_id desc

--2c
--define the return variable
declare @product_id int
--execute the procedure
execute @product_id = p_fudgemart_add_product
						@product_name= 'Slot Screwdriver',
						@product_department='Hardware',
						@product_retail_price = 6.95,
						@product_wholesale_price = 3.95,
						@product_vendor_id = 3
-- run a select to see the inserted value
select * from fudgemart_products where product_id=@product_id

create view v_fudgemart_display_active_products
as
select	 product_id, product_name, product_department, product_wholesale_price, product_retail_price,
		vendor_id, vendor_name, vendor_website
	from fudgemart_products join fudgemart_vendors on vendor_id=product_vendor_id
	where product_is_active=1

select table_name, view_definition
	from information_schema.views
	where table_name = 'v_fudgemart_display_active_products'

select vendor_name, product_name, product_wholesale_price, product_retail_price
	from v_fudgemart_display_active_products
	where product_department ='Hardware'
	order by vendor_name, product_name
