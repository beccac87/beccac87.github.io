select product_name,
product_department,
product_retail_price,
product_wholesale_price,
(product_retail_price - product_wholesale_price) / product_wholesale_price as product_markup
from fudgemart_products
order by product_markup