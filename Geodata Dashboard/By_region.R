# 1990-1999 Town -----
data1_by_region <- data1_clean %>%
  group_by(town) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price))

print(data1_by_region$town)

fig1_town <- plot_ly(data1_by_region,
                type = "scattermapbox",
                mode = "markers",
                lat = ~latitude,
                lon = ~longitude,
                text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                marker = list(size = ~avg_resale_price / 200, color = "red", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",  
      center = list(lat = mean(data1_by_region$latitude), lon = mean(data1_by_region$longitude)),
      zoom = 10  
    )
  )
fig1_town

# Data2 Town -----
data2_by_region <- data2_clean %>%
  group_by(town) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price)
  )

print(data2_by_region$town)

fig2_town <- plot_ly(data2_by_region,
                     type = "scattermapbox",
                     mode = "markers",
                     lat = ~latitude,
                     lon = ~longitude,
                     text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                     marker = list(size = ~avg_resale_price / 200, color = "blue", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",  
      center = list(lat = mean(data2_by_region$latitude), lon = mean(data2_by_region$longitude)),
      zoom = 10  
    )
  )
fig2_town


# Data3 Town -----
data3_by_region <- data3_clean %>%
  group_by(town) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price)
  )

print(data3_by_region$town)

fig3_town <- plot_ly(data3_by_region,
                     type = "scattermapbox",
                     mode = "markers",
                     lat = ~latitude,
                     lon = ~longitude,
                     text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                     marker = list(size = ~avg_resale_price / 200, color = "green", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",  
      center = list(lat = mean(data3_by_region$latitude), lon = mean(data3_by_region$longitude)),
      zoom = 10  
    )
  )
fig3_town

# Data4 Town -----
data4_by_region <- data4_clean %>%
  group_by(town) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price)
  )

print(data4_by_region$town)

fig4_town <- plot_ly(data4_by_region,
                     type = "scattermapbox",
                     mode = "markers",
                     lat = ~latitude,
                     lon = ~longitude,
                     text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                     marker = list(size = ~avg_resale_price / 200, color = "green", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",  
      center = list(lat = mean(data4_by_region$latitude), lon = mean(data4_by_region$longitude)),
      zoom = 10  
    )
  )
fig4_town

# Data5 Town -----

# Agrupar os dados e calcular a média das variáveis, além do ano mais frequente
data5_by_region <- data5_clean %>%
  group_by(town) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    avg_resale_price = mean(resale_price),
    closest_mrt_dist = mean(closest_mrt_dist),
    cbd_dist = mean(cbd_dist), 
    mc_flat_type = get_mode(flat_type),
    mc_flat_model = get_mode(flat_model)
  )


print(data5_by_region)

fig5_town <- plot_ly(data5_by_region,
                     type = "scattermapbox",
                     mode = "markers",
                     lat = ~latitude,
                     lon = ~longitude,
                     text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                     marker = list(size = ~avg_resale_price / 200, color = "fuchsia", opacity = 0.7)
) %>%
  layout(
    mapbox = list(
      style = "open-street-map",  
      center = list(lat = mean(data5_by_region$latitude), lon = mean(data5_by_region$longitude)),
      zoom = 10  
    )
  )
fig5_town



