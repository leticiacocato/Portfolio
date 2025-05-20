#By_PArea
library(plotly)
# Function to set most frequent year 
get_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}



## Data1
data1_clean <- data1_clean %>%
  mutate(postal_area = substr(as.character(postal_code), 1, 3))

data1_by_parea <- data1_clean %>%
  group_by(postal_area) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )

fig1_area <- plot_ly(
  data1_by_parea,
  type = "scattermapbox",
  mode = "markers",
  lat = ~latitude,
  lon = ~longitude,
  text = ~paste("Postal Area: ", postal_area, "<br>Price: $", round(avg_resale_price, 2)),
  marker = list(size = ~avg_resale_price / 200, color = "red", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      center = list(lat = mean(data1_by_parea$latitude), lon = mean(data1_by_parea$longitude)),
      zoom = 10
    )
  )
fig1_area

## Data2
data2_clean <- data2_clean %>%
  mutate(postal_area = substr(as.character(postal_code), 1, 3))

data2_by_parea <- data2_clean %>%
  group_by(postal_area) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )

fig2_area <- plot_ly(
  data2_by_parea,
  type = "scattermapbox",
  mode = "markers",
  lat = ~latitude,
  lon = ~longitude,
  text = ~paste("Postal Area: ", postal_area, "<br>Price: $", round(avg_resale_price, 2)),
  marker = list(size = ~avg_resale_price / 200, color = "blue", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      center = list(lat = mean(data2_by_parea$latitude), lon = mean(data2_by_parea$longitude)),
      zoom = 10
    )
  )
fig2_area

## Data3
data3_clean <- data3_clean %>%
  mutate(postal_area = substr(as.character(postal_code), 1, 3))

data3_by_parea <- data3_clean %>%
  group_by(postal_area) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )

fig3_area <- plot_ly(
  data3_by_parea,
  type = "scattermapbox",
  mode = "markers",
  lat = ~latitude,
  lon = ~longitude,
  text = ~paste("Postal Area: ", postal_area, "<br>Price: $", round(avg_resale_price, 2)),
  marker = list(size = ~avg_resale_price / 200, color = "green", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      center = list(lat = mean(data3_by_parea$latitude), lon = mean(data3_by_parea$longitude)),
      zoom = 10
    )
  )
fig3_area

## Data4
data4_clean <- data4_clean %>%
  mutate(postal_area = substr(as.character(postal_code), 1, 3))

data4_by_parea <- data4_clean %>%
  group_by(postal_area) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )

fig4_area <- plot_ly(
  data4_by_parea,
  type = "scattermapbox",
  mode = "markers",
  lat = ~latitude,
  lon = ~longitude,
  text = ~paste("Postal Area: ", postal_area, "<br>Price: $", round(avg_resale_price, 2)),
  marker = list(size = ~avg_resale_price / 200, color = "yellow", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      center = list(lat = mean(data4_by_parea$latitude), lon = mean(data4_by_parea$longitude)),
      zoom = 10
    )
  )
fig4_area

## Data5
# extrai os 3 primeiros dígitos como "área"
data5_clean <- data5_clean %>%
  mutate(postal_area = substr(as.character(postal_code), 1, 3))

data5_by_parea <- data5_clean %>%
  group_by(postal_area) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )



# Dados: data5_by_parea
# Lista de áreas postais únicas
#areas5 <- unique(data5_by_parea$postal_area)

# Lista de botões para o dropdown
#dropdown_buttons <- lapply(seq_along(areas5), function(i) {
 # area5 <- areas5[i]
  #row <- data5_by_parea[data5_by_parea5$postal_area5 == area5, ]
  
  #list(
  #  method = "relayout",
   # args = list("mapbox.center", list(lat = row$latitude, lon = row$longitude)),
  #  label = paste("Área", area)
  #)
#})

# Cria o gráfico com todos os pontos
fig5_area <- plot_ly(
  data5_by_parea,
  type = "scattermapbox",
  mode = "markers",
  lat = ~latitude,
  lon = ~longitude,
  text = ~paste("Postal Area: ", postal_area, "<br>Price: $", round(avg_resale_price, 2)),
  marker = list(size = ~avg_resale_price / 200, color = "fuchsia", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",
      center = list(lat = mean(data5_by_parea$latitude), lon = mean(data5_by_parea$longitude)),
      zoom = 10
    )
  )
fig5_area


