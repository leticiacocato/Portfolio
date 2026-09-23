/*
===============================================================================
Business Analysis
Project: Retail Sales Analysis
Dataset: Sample Superstore
Database: PostgreSQL
Author: Letícia Cocato
===============================================================================

Objective:
Answer key business questions using SQL to generate actionable business insights.

Business Questions:
1. Summary
2. Sales Analysis
3. Profit Analysis
4. Customer Analysis
5. Product Analysis
6. Regional Analysis
7. Discount Analysis
===============================================================================
*/

-- ============================================================================
-- 1. Summary
-- ============================================================================
-- What was the total revenue?
SELECT 
    ROUND(sum(sales),2) AS total_sales
FROM superstore_data;

-- What was the total revenue?
SELECT 
    ROUND(sum(profit),2) AS total_profit
FROM superstore_data;

-- What was the profit margin?
SELECT
    ROUND((SUM(profit) / SUM(sales)) * 100, 2) AS profit_margin
FROM superstore_data;

-- How many orders were placed?
SELECT
    COUNT(DISTINCT order_id) AS total_orders
FROM superstore_data;

-- What was the average order value?
SELECT
    ROUND(
        SUM(sales)/COUNT(DISTINCT order_id),
        2) AS avg_order_value
FROM superstore_data

-- ============================================================================
-- 2. Sales Analysis
-- ============================================================================
-- Which product category generates the most sales? 
SELECT
    category,
    ROUND(SUM(sales),2) AS total_sales
FROM superstore_data
GROUP BY category
ORDER BY total_sales DESC;

-- Which product subcategories generates the most sales?
SELECT
    sub_category,
    ROUND(SUM(sales),2) AS total_sales
FROM superstore_data
GROUP BY sub_category
ORDER BY total_sales DESC;

-- Which customer segment generates the most sales?
SELECT
	segment,
	ROUND(SUM(sales),2) AS total_sales
FROM superstore_data
GROUP BY segment
ORDER BY total_sales DESC;

-- Which shipping mode is used most frequently?
SELECT
    ship_mode,
    COUNT(DISTINCT order_id) AS total_orders
FROM superstore_data
GROUP BY ship_mode
ORDER BY total_orders DESC;

-- What was the total sales by year?
SELECT
    EXTRACT(YEAR FROM order_date) AS year,
    ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY year
ORDER BY year;

-- How much did sales grow compared to the previous year?
WITH yearly_sales AS (
    SELECT
        EXTRACT(YEAR FROM order_date) AS year,
        SUM(sales) AS total_sales
    FROM superstore_data
    GROUP BY year
),

sales_with_previous_year AS (
    SELECT
        year,
        total_sales,
        LAG(total_sales) OVER (ORDER BY year) AS previous_year_sales
    FROM yearly_sales
)

SELECT
    year,
    ROUND(total_sales, 2) AS total_sales,
    ROUND(previous_year_sales, 2) AS previous_year_sales,
    ROUND(
        ((total_sales - previous_year_sales) / previous_year_sales) * 100,
        2
    ) AS sales_growth_percentage
FROM sales_with_previous_year
ORDER BY year;

-- How do sales vary over time on a monthly basis? 
SELECT 
	DATE_TRUNC('month', order_date) AS month,
	ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY month
ORDER BY month;

-- ============================================================================
-- 3. Profit Analysis
-- ============================================================================
-- Which product category generates the most profit?
SELECT 
	category,
	ROUND(SUM(profit), 2) as total_profit
FROM superstore_data
GROUP BY category
ORDER BY category DESC; 

-- Which product subcategories generate the most and least profit?
SELECT 
	sub_category,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY sub_category
ORDER BY total_profit DESC;

-- Profit Margin by Category
SELECT
    category,
    ROUND(
        (SUM(profit) / SUM(sales)) * 100,
        2
    ) AS profit_margin
FROM superstore_data
GROUP BY category
ORDER BY profit_margin DESC;

-- Most profitable products
SELECT 
	product_name,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY product_name
ORDER BY total_profit DESC
LIMIT 10;

-- Products that generate largest losses
SELECT 
	product_name,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY product_name
ORDER BY total_profit ASC
LIMIT 10;

-- Which customer segment generates the most profit?
SELECT
	segment,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY segment
ORDER BY total_profit DESC;

-- ============================================================================
-- 4. Customer Analysis
-- ============================================================================

-- Which customers generate the most sales?
SELECT 
    customer_id,
    customer_name,
    ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY customer_id, customer_name
ORDER BY total_sales DESC
LIMIT 10;

-- Which customers generate the most profit?
SELECT 
    customer_id,
    customer_name,
    ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY customer_id, customer_name
ORDER BY total_profit DESC
LIMIT 10;

-- Which customers place the most orders?
SELECT 
    customer_id,
    customer_name,
    COUNT(DISTINCT(order_id)) AS total_orders
FROM superstore_data
GROUP BY customer_id, customer_name
ORDER BY total_orders DESC
LIMIT 10;

-- Which customers have the highest average order value?
SELECT 
	customer_id,
	customer_name,
	ROUND(SUM(sales)/COUNT(DISTINCT order_id), 2) AS avg_order_value
FROM superstore_data
GROUP BY customer_id, customer_name
ORDER BY avg_order_value DESC
LIMIT 10;

-- How frequently do customers place orders?
WITH customer_orders AS(
	SELECT
    	customer_id,
		customer_name,
		COUNT(DISTINCT order_id) AS total_orders
	FROM superstore_data
	GROUP BY customer_id, customer_name
), 
customer_frequency AS(
	SELECT 
		CASE
			WHEN total_orders = 1 THEN '1 order'
			WHEN total_orders BETWEEN 2 AND 5 THEN '2-5 orders'
			WHEN total_orders BETWEEN 6 AND 10 THEN '6-10 orders'
			ELSE 'More than 10 orders'
		END AS purchase_frequency
FROM customer_orders
)
SELECT 
	purchase_frequency,
	COUNT(*) AS total_customers
FROM customer_frequency
GROUP BY purchase_frequency
ORDER BY total_customers DESC;

-- ============================================================================
-- 5. Product Analysis
-- ============================================================================

-- Which products generate the highest sales?
SELECT 
	product_id,
	product_name,
	ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY product_id, product_name
ORDER BY total_sales DESC
LIMIT 10;

-- Which products sell the most units? 
SELECT 
	product_id,
	product_name,
	SUM(quantity) AS total_quantity
FROM superstore_data
GROUP BY product_id, product_name
ORDER BY total_quantity DESC
LIMIT 10;

-- How do sales and profit compare across product categories?
SELECT
    category,
	ROUND(SUM(sales), 2) AS total_sales,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY category
ORDER BY total_profit DESC;

-- How do sales and profit compare across product subcategories?
SELECT
    sub_category,
	ROUND(SUM(sales), 2) AS total_sales,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY sub_category
ORDER BY total_profit DESC;

-- Which product subcategories have the highest average unit price? 
SELECT
    sub_category,
	ROUND(SUM(sales)/SUM(quantity), 2) AS avg_unit_price
FROM superstore_data
GROUP BY sub_category
ORDER BY avg_unit_price DESC;

-- ============================================================================
-- 6. Regional Analysis
-- ============================================================================

-- Which regions generate the highest sales?
SELECT
	region,
	ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY region
ORDER BY total_sales DESC;

-- Which region generates the highest profit?
SELECT
	region,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY region
ORDER BY total_profit DESC;

-- Which ststes generates the largest losses?
SELECT
	state,
	ROUND(SUM(profit), 2) AS total_profit
FROM superstore_data
GROUP BY state
HAVING SUM(profit) < 0 
ORDER BY total_profit ASC
LIMIT 10;

-- Which ststes generates the highest sales?
SELECT
	state,
	ROUND(SUM(sales), 2) AS total_sales
FROM superstore_data
GROUP BY state
ORDER BY total_sales DESC
LIMIT 10;

-- ============================================================================
-- 7. Discount Analysis
-- ============================================================================

-- Which product categories receive the highest average discounts?
SELECT
	category, 
	ROUND(AVG(discount)*100, 2) AS avg_discount_percentage
FROM superstore_data
GROUP BY category
ORDER BY avg_discount_percentage DESC;

-- How does the discount level affect profitability?
WITH discount_groups AS(
	SELECT
		sales,
		profit,
		CASE
			WHEN discount = 0 THEN 'No discount'
			WHEN discount <= 0.2 THEN 'Low discount'
			WHEN discount <= 0.4 THEN 'Medium discount'
			ELSE 'High discount'
		END AS discount_level
	FROM superstore_data
)
SELECT
	discount_level,
	COUNT(*) AS total_records,
	ROUND(AVG(profit), 2) AS avg_profit,
	ROUND(
		SUM(profit) / SUM(sales)*100,
		2
	) AS profit_margin
FROM discount_groups
GROUP BY discount_level
ORDER BY profit_margin DESC;
