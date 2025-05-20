library(readr)
library(dplyr)
library(sp)
library(geoR)
library(plotly)
library(dplyr)
# ~  processo espaço-temporal, Y(x, t)
# Dividir a análise em três frentes: 
  # Análise Espacial
  # Análise Temporal
  # Análise Espaço-Temporal
# ~ Y E N (valor de revenda de casas no local x) onde x E A C R² -> R² (espaço plano com longitude e latitude)
# Modelo: Y(x) | Z1(x), Z2(x)...
# Efeitos fixos + resíduos (os resíduos deverão ter correlação espacial)
# a correlação espacial dos resíduos deve ser modelada, que são os efeitos aleatórios (efeitos fixos + efeitos aleatorios = efeitos mistos)

options(scipen = 99999)
# 1990 - 1999 -----
data1 = read_csv("resale-flat-prices-based-on-approval-date-1990-1999_locationdata.csv")
data1$latitude = as.numeric(data1$latitude)
data1$longitude = as.numeric(data1$longitude)
data1$year = as.character(data1$year)
data1$flat_type = as.character(data1$flat_type)
data1$resale_price = (data1$resale_price)/100
#data1_observed = c(5771)



data1_clean = data1 %>% distinct(latitude, longitude, .keep_all = TRUE)
which(is.na(data1_clean), arr.ind = TRUE)
data1_clean = data1_clean[-12,]
data1_clean[36,]
data1_clean[257,]
data1_clean[1553,]
which(is.na(data1_clean), arr.ind = TRUE)

#data1_geo = as.geodata(data1_clean, coords.col = 1:2,
                      # data.col = 16,
                       #covar.col = c(3:15, 17:18))
#summary(data1_geo) 
# max.dist = 2.807091e-01 

#fig <- plot_ly(
 # data1_clean,
  #type = "scattermapbox",
#  lat = ~latitude,
 # lon = ~longitude,
#  text = ~paste("Town: ", town, "<br>Price: $", resale_price*100),  # Display town and price on hover
 # marker = list(size = ~resale_price / 200, color = "red", opacity = 0.7)
#) %>%
 # layout(
  #  mapbox = list(
   #   style = "open-street-map",  # You can also use "carto-positron", "satellite", etc.
    #  center = list(lat = mean(data1_clean$latitude), lon = mean(data1_clean$longitude)),  # Center the map on the mean of your data
     # zoom = 10  # Adjust zoom level, higher = more zoomed in
    #)
  #)

#fig  # Show the map with hover information


# 2000 - 2012 -----
data2 = read_csv("resale-flat-prices-based-on-approval-date-2000-feb-2012_locationdata.csv")
data2$resale_price = (data2$resale_price)/100
data2_clean = data2 %>% distinct(latitude, longitude, .keep_all = TRUE)
which(is.na(data2_clean), arr.ind = TRUE)
data2_clean = data2_clean[-13,]
data2_clean[39,]
data2_clean[324,]
data2_clean[2069,]
#data2_clean = data2_clean[, -3]
which(is.na(data2_clean), arr.ind = TRUE)
#data2_observed = 8248
#data2_geo = as.geodata(data2_clean, coords.col = 1:2,
                      # data.col = 16,
                       #covar.col = c(3:15, 17:18))
#summary(data2_geo)



# 2012 - 2014 -----
data3 = read_csv("resale-flat-prices-based-on-registration-date-from-mar-2012-to-dec-2014_locationdata.csv")
data3$resale_price = (data3$resale_price)/100
data3_clean = data3 %>% distinct(latitude, longitude, .keep_all = TRUE)
which(is.na(data3_clean), arr.ind = TRUE)
data3_clean = data3_clean[-1701,]
data3_clean[1992,]
data3_clean = data3_clean[-1991,]
#data3_clean = data3_clean[, -3] 
which(is.na(data3_clean), arr.ind = TRUE)
#data3_observed = 8021
 
#data3_geo = as.geodata(data3_clean, coords.col = 1:2,
                    #   data.col = 17,
                     #  covar.col = c(3:16, 18))
#summary(data3_geo)


# 2015 - 2016 -----
data4 = read_csv("resale-flat-prices-based-on-registration-date-from-jan-2015-to-dec-2016_locationdata.csv")
data4$resale_price = (data4$resale_price)/100
data4_clean = data4 %>% distinct(latitude, longitude, .keep_all = TRUE)
which(is.na(data4_clean), arr.ind = TRUE)
data4_clean = data4_clean[-1972,]
#data4_clean[1992,]
#data4_clean = data4_clean[-1991,]
#data4_clean = data4_clean[, -3] 
which(is.na(data4_clean), arr.ind = TRUE)


#data4_geo = as.geodata(data4_clean, coords.col = 1:2,
                #       data.col = 18,
                #       covar.col = c(3:17, 19:20))
#summary(data4_geo)

# 2017 -  -----
data5 = read_csv("resale-flat-prices-based-on-registration-date-from-jan-2017-onwards_locationdata.csv")
data5$resale_price = (data5$resale_price)/100
data5_clean = data5 %>% distinct(latitude, longitude, .keep_all = TRUE)
which(is.na(data5_clean), arr.ind = TRUE)
data5_clean = data5_clean[-2259,]
#data5_clean[1992,]
#data5_clean = data5_clean[-1991,]
#data5_clean = data5_clean[, -3] 
which(is.na(data5_clean), arr.ind = TRUE)
#data5_observed = 9294
 
#data5_geo = as.geodata(data5_clean, coords.col = 1:2,
                      # data.col = 18,
                       #covar.col = c(3:17, 19:20))
#summary(data5_geo)
