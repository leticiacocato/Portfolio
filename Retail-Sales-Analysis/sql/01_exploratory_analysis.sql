/*
===============================================================================
Exploratory Data Analysis (EDA)
Project: Retail Sales Analysis
Dataset: Sample Superstore
Database: PostgreSQL
Author: Letícia Cocato
===============================================================================

Objective:
Explore the dataset, assess data quality, and identify its main characteristics before performing business analysis.

Steps:
1. General Information
2. Customer Overview
3. Product Overview
4. Geographic Coverage
5. Date Range
6. Data Quality 
7. Descriptive Statistics
8. Data Distribution
===============================================================================
*/

-- ============================================================================
-- 1. General Information
-- ============================================================================

-- Total number of records
SELECT COUNT(*) AS total_records
FROM superstore_data;

-- Total number of columns
SELECT COUNT(*) AS total_columns
FROM information_schema.columns
WHERE table_name = 'superstore_data';

-- Table schema
SELECT
    column_name,
    data_type
FROM information_schema.columns
WHERE table_name = 'superstore_data'
ORDER BY ordinal_position;


-- ============================================================================
-- 2. Customer Overview
-- ============================================================================

-- Total number of unique customers 
SELECT COUNT(DISTINCT customer_id) AS total_customers
FROM superstore_data;

-- Number of customers by segment
SELECT
    segment,
    COUNT(DISTINCT customer_id) AS total_customers
FROM superstore_data
GROUP BY segment
ORDER BY total_customers DESC;

-- ============================================================================
-- 3. Product Overview
-- ============================================================================

-- Total number of unique products
SELECT COUNT(DISTINCT product_id) AS total_products
FROM superstore_data;

-- Product categories
SELECT DISTINCT category
FROM superstore_data;

-- Product subcategories
SELECT DISTINCT
    category,
    sub_category
FROM superstore_data
ORDER BY category, sub_category;

-- Number of products by category
SELECT
    category,
    COUNT(DISTINCT product_id) AS total_products
FROM superstore_data
GROUP BY category
ORDER BY total_products DESC;

-- ============================================================================
-- 4. Geographic Coverage
-- ============================================================================

-- Countries
SELECT DISTINCT country
FROM superstore_data
ORDER BY country;

-- Regions
SELECT DISTINCT region
FROM superstore_data
ORDER BY region;

-- Total number of states
SELECT COUNT(DISTINCT state) AS total_states
FROM superstore_data;

-- Total number of cities
SELECT COUNT(DISTINCT city) AS total_cities
FROM superstore_data;

-- Number of orders by region 
SELECT
    region,
    COUNT(*) AS total_orders
FROM superstore_data
GROUP BY region
ORDER BY total_orders DESC;

-- Number of orders by state
SELECT
    state,
    COUNT(*) AS total_orders
FROM superstore_data
GROUP BY state
ORDER BY total_orders DESC;

-- ============================================================================
-- 5. Date Range
-- ============================================================================

SELECT
    MIN(order_date) AS first_sale,
    MAX(order_date) AS last_sale
FROM superstore_data;


-- ============================================================================
-- 6. Data Quality
-- ============================================================================

-- Check for missing values in key columns

SELECT
    COUNT(*) FILTER (WHERE order_id IS NULL) AS order_id_nulls,
    COUNT(*) FILTER (WHERE customer_id IS NULL) AS customer_id_nulls,
    COUNT(*) FILTER (WHERE product_id IS NULL) AS product_id_nulls,
    COUNT(*) FILTER (WHERE sales IS NULL) AS sales_nulls,
    COUNT(*) FILTER (WHERE profit IS NULL) AS profit_nulls;

-- Check for duplicate rows
SELECT
    COUNT(*) - COUNT(DISTINCT row_id) AS duplicate_rows
FROM superstore_data;

-- Check for invalid sales values

SELECT COUNT(*) AS negative_sales
FROM superstore_data
WHERE sales < 0;

-- Check for invalid quantities

SELECT COUNT(*) AS invalid_quantity
FROM superstore_data
WHERE quantity <= 0;

-- ============================================================================
-- 7. Descriptive Statistics
-- ============================================================================

-- Sales Statistics
SELECT
    MIN(sales) AS min_sales,
    MAX(sales) AS max_sales,
    ROUND(AVG(sales), 2) AS avg_sales
FROM superstore_data;

-- Profit Statistics
SELECT
    MIN(profit) AS min_profit,
    MAX(profit) AS max_profit,
    ROUND(AVG(profit), 2) AS avg_profit
FROM superstore_data;

-- Discount Statistics
SELECT
    MIN(discount) AS min_discount,
    MAX(discount) AS max_discount,
    ROUND(AVG(discount) * 100, 2) AS avg_discount_percentage
FROM superstore_data;

-- Sales Median
SELECT
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY sales) AS median_sales
FROM superstore_data;

-- ============================================================================
-- 8. Data Distribution
-- ============================================================================

-- Orders by Category
SELECT
    category,
    COUNT(*) AS total_orders
FROM superstore_data
GROUP BY category
ORDER BY total_orders DESC;

-- Orders by Region
SELECT
    region,
    COUNT(*) AS total_orders
FROM superstore_data
GROUP BY region
ORDER BY total_orders DESC;

-- Orders by Customer Segment
SELECT
    segment,
    COUNT(*) AS total_orders
FROM superstore_data
GROUP BY segment
ORDER BY total_orders DESC;
