CREATE DATABASE Pizza_Data;

USE Pizza_Data



CREATE TABLE #temp_pizza_data (
    order_id INT,
    pizza_id NVARCHAR(50),  
    pizza_type_id NVARCHAR(50), 
    size NVARCHAR(50),
    price DECIMAL(10, 2),
    quantity INT
);

INSERT INTO #temp_pizza_data
SELECT 
    od.order_id,
    od.pizza_id,
    p.pizza_type_id,
    p.size,
    p.price,
    od.quantity
FROM order_details od
LEFT JOIN pizzas p
    ON od.pizza_id = p.pizza_id;


	CREATE TABLE #kpi_quantity_per_pizza (
    pizza_id NVARCHAR(50),  
    total_quantity INT
);

INSERT INTO #kpi_quantity_per_pizza
SELECT 
    pizza_id,
    SUM(quantity) AS total_quantity
FROM #temp_pizza_data
GROUP BY pizza_id;

-- View the results
SELECT * FROM #kpi_quantity_per_pizza;

 SELECT * FROM #temp_pizza_data;

 SELECT * FROM #kpi_quantity_per_pizza;

 SELECT * INTO kpi_quantity_per_pizza FROM #kpi_quantity_per_pizza;

 SELECT * FROM kpi_quantity_per_pizza;


 --KPI 2
 CREATE TABLE #temp_pizza_sales (
    order_id INT,
    pizza_id NVARCHAR(50),  
    pizza_type_id NVARCHAR(50),  
    pizza_name NVARCHAR(100),
    size NVARCHAR(50),
    price DECIMAL(10, 2),
    quantity INT,
    total_revenue DECIMAL(10, 2)
);

INSERT INTO #temp_pizza_sales
SELECT 
    od.order_id,
    od.pizza_id,
    p.pizza_type_id,
    pt.name AS pizza_name, 
    p.size,
    p.price,
    od.quantity,
    (p.price * od.quantity) AS total_revenue
FROM order_details od
LEFT JOIN pizzas p
    ON od.pizza_id = p.pizza_id
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;

CREATE TABLE #kpi_total_revenue_by_type (
    pizza_type_id NVARCHAR(50),  
    pizza_name NVARCHAR(100),
    total_revenue DECIMAL(10, 2)
);

INSERT INTO #kpi_total_revenue_by_type
SELECT 
    pizza_type_id,
    pizza_name,
    SUM(total_revenue) AS total_revenue
FROM #temp_pizza_sales
GROUP BY pizza_type_id, pizza_name;

-- View the KPI results
SELECT * FROM #kpi_total_revenue_by_type;

--making permanent table
CREATE TABLE pizza_sale (
    order_id INT,
    pizza_id NVARCHAR(50),  
    pizza_type_id NVARCHAR(50),  
    pizza_name NVARCHAR(100),
    size NVARCHAR(50),
    price DECIMAL(10, 2),
    quantity INT,
    total_revenue DECIMAL(10, 2)
);

--making total rev permanent table

CREATE TABLE kpi_total_revenue_by_types (
    pizza_type_id NVARCHAR(50),  
    pizza_name NVARCHAR(100),
    total_revenue DECIMAL(10, 2)
);


INSERT INTO pizza_sale
SELECT *
FROM #temp_pizza_sales;


INSERT INTO kpi_total_revenue_by_types
SELECT *
FROM #kpi_total_revenue_by_type;


--KPI 3

USE Pizza_Data

SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.category,  
    p.size,
    p.price
FROM dbo.pizzas p  
LEFT JOIN dbo.pizza_types pt 
    ON p.pizza_type_id = pt.pizza_type_id;

DROP TABLE IF EXISTS #temp_pizza_data;

CREATE TABLE #temp_pizza_data (
    pizza_id NVARCHAR(50),
    pizza_type_id NVARCHAR(50),
    category NVARCHAR(100),
    size NVARCHAR(50),
    price DECIMAL(10, 2)
);

SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.category,
    p.size,
    p.price
FROM pizzas p
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;

INSERT INTO #temp_pizza_data (pizza_id, pizza_type_id, category, size, price)
SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.category,
    p.size,
    p.price
FROM pizzas p
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;

SELECT [category] FROM pizza_types;

INSERT INTO #temp_pizza_data (pizza_id, pizza_type_id, [category], size, price)
SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.[category],
    p.size,
    p.price
FROM pizzas p
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;

SELECT category FROM pizza_types;


SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.category,
    p.size,
    p.price
FROM pizzas p
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;


CREATE TABLE #temp_pizza_datas (
    pizza_id NVARCHAR(50),  
    pizza_type_id NVARCHAR(50),  
    category NVARCHAR(100), 
    size NVARCHAR(50),  
    price DECIMAL(10, 2)  
);

INSERT INTO #temp_pizza_datas (pizza_id, pizza_type_id, category, size, price)
SELECT 
    p.pizza_id,
    pt.pizza_type_id,
    pt.category,
    p.size,
    p.price
FROM pizzas p
LEFT JOIN pizza_types pt
    ON p.pizza_type_id = pt.pizza_type_id;

CREATE TABLE #kpi_avg_price_by_category (
    category NVARCHAR(100),  
    avg_price DECIMAL(10, 2) 
);

INSERT INTO #kpi_avg_price_by_category (category, avg_price)
SELECT 
    category,
    AVG(price) AS avg_price
FROM #temp_pizza_data
GROUP BY category;

SELECT * FROM #kpi_avg_price_by_category;

CREATE TABLE pizza_datass (
    pizza_id NVARCHAR(50),
    pizza_type_id NVARCHAR(50),
    category NVARCHAR(100),
    size NVARCHAR(50),
    price DECIMAL(10, 2)
);

INSERT INTO pizza_datass
SELECT * FROM #temp_pizza_data;

CREATE TABLE avg_price_by_category (
    category NVARCHAR(100),
    avg_price DECIMAL(10, 2)
);

INSERT INTO avg_price_by_category
SELECT * FROM #kpi_avg_price_by_category;


select * from avg_price_by_category;


SELECT @@SERVERNAME AS ServerName;
