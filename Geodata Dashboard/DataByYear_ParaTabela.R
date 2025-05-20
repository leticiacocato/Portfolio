# Table data
# year, town, avg_sale

# data1_by_region 1990/1999
# data2_by_region 2000/2012
# data3_by_region 2012/2014
# data4_by_region 2015/2016
# data5_by_region 2017/2023

data1_clean
data1_by_year = data.frame(ID= 1:25, Year = rep('1990/1999', 25))
data1_by_year$Year = as.character(data1_by_year$Year)
data1_by_year$AvgSalePrice <- data1_by_region$avg_resale_price
data1_by_year$Town <- data1_by_region$town
print(data1_by_year)


data2_by_year = data.frame(ID= 1:26, Year = rep('2000/2012', 26))
data2_by_year$Year = as.character(data2_by_year$Year)
data2_by_year$AvgSalePrice <- data2_by_region$avg_resale_price
data2_by_year$Town <- data2_by_region$town
print(data2_by_year)

data3_by_year = data.frame(ID= 1:26, Year = rep('2012/2014', 26))
data3_by_year$Year = as.character(data3_by_year$Year)
data3_by_year$AvgSalePrice <- data3_by_region$avg_resale_price
data3_by_year$Town <- data3_by_region$town
print(data3_by_year)


data4_by_year = data.frame(ID= 1:26, Year = rep('2015/2016', 26))
data4_by_year$Year = as.character(data4_by_year$Year)
data4_by_year$AvgSalePrice <- data4_by_region$avg_resale_price
data4_by_year$Town <- data4_by_region$town
print(data4_by_year)

data5_by_year = data.frame(ID= 1:26, Year = rep('2017/2023', 26))
data5_by_year$Year = as.character(data5_by_year$Year)
data5_by_year$AvgSalePrice <- data5_by_region$avg_resale_price
data5_by_year$Town <- data5_by_region$town
print(data5_by_year)


alldata_by_year = rbind(data1_by_year, data2_by_year, data3_by_year, data4_by_year, data5_by_year)
alldata_by_year



ggplot(alldata_by_year, aes(x = Year, y = AvgSalePrice)) +
  geom_jitter() +
  labs(title = "Average Sale Price by Town",
       x = "Average Sale Price",
       y = "Town") +
  theme_minimal()

library(ggplot2)

ggplot(alldata_by_year, aes(x = Year, y = AvgSalePrice)) +
  geom_jitter() +
  labs(title = "Average Sale Price by Year",
       x = "Year",
       y = "Average Sale Price") +
  theme_minimal()


library(ggplot2)


alldata_by_year = rbind(data1_by_year, data2_by_year, data3_by_year, data4_by_year, data5_by_year)
alldata_by_year

# Specify custom colors for each Year
ggplot(alldata_by_year, aes(x = as.factor(Year), y = AvgSalePrice, fill = as.factor(Year))) +
  geom_boxplot() +  
  labs(title = "Average Sale Price by Year",
       x = "Year",
       y = "Average Sale Price",
       fill = 'Year') +
  scale_fill_manual(values = c("1990/1999" = "red", "2000/2012" = "blue", "2012/2014" = "green", "2015/2016" = "yellow", '2017/2023' = 'pink')) +  # Custom colors for each Year
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        #legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x.bottom  = element_blank(),
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels for better readability
