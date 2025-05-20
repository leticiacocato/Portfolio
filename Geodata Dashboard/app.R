library(shiny)
#install.packages("plotly")
library(plotly)
#install.packages("bs4Dash")
library(bs4Dash)
library(DT)


# Source external files
source("Singapore_resaleflat.R")
source("By_area.R")
source("By_region.R")
source("DataByYear_ParaTabela.R")


# Define server logic
server <- function(input, output, session) {
  #num1_observed <- 5771  
  #num2_observed <- 8248 
  #num3_observed <- 8021
  #num4_observed <- 7920
  #num5_observed <- 9439
  
  output$stats_textfig1 <- renderUI({
    avg_price <- mean(data1_clean$resale_price)
    median_price <- median(data1_clean$resale_price)
    max_price <- max(data1_clean$resale_price)
    min_price <- min(data1_clean$resale_price)
    
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Median Price: $", format_price(median_price)),
      br(),  # Quebra de linha
      paste("Max Price: $", format_price(max_price)),
      br(),  # Quebra de linha
      paste("Min Price: $", format_price(min_price))
    )
  })

  
  output$fig1 <- renderPlotly({
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    plot_ly(
    data1_clean,
    type = "scattermapbox",
    mode = "markers",
    lat = ~latitude,
    lon = ~longitude,
    text = ~paste("Town: ", town, 
                  "<br>Price: $", format_price(resale_price*100)), 
    hoverinfo = "text", 
    marker = list(size = ~resale_price / 200, color = "red", opacity = 0.7)
  ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",  
          center = list(lat = mean(data1_clean$latitude), lon = mean(data1_clean$longitude)),   
          zoom = 10  
        )
      )
  })
  output$stats_textfig2 <- renderUI({
    avg_price <- mean(data2_clean$resale_price)
    median_price <- median(data2_clean$resale_price)
    max_price <- max(data2_clean$resale_price)
    min_price <- min(data2_clean$resale_price)
    
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Median Price: $", format_price(median_price)),
      br(),  # Quebra de linha
      paste("Max Price: $", format_price(max_price)),
      br(),  # Quebra de linha
      paste("Min Price: $", format_price(min_price))
    )
  })
  output$fig2 <- renderPlotly({
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    plot_ly(data2_clean,
                  type = "scattermapbox",
                  mode = "markers",
                  lat = ~latitude,
                  lon = ~longitude,
                  text = ~paste("Town: ", town, 
                                "<br>Price: $", format_price(resale_price)), 
                  hoverinfo = "text",   # Display town and price on hover
                  marker = list(size = ~resale_price / 200, color = "blue", opacity = 0.7)
  ) %>%
    layout(
      mapbox = list(
        style = "open-street-map",  
        center = list(lat = mean(data2_clean$latitude), lon = mean(data2_clean$longitude)),
        zoom = 10  
      )
    )
  })
  output$stats_textfig3 <- renderUI({
    avg_price <- mean(data3_clean$resale_price)
    median_price <- median(data3_clean$resale_price)
    max_price <- max(data3_clean$resale_price)
    min_price <- min(data3_clean$resale_price)
    
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Median Price: $", format_price(median_price)),
      br(),  # Quebra de linha
      paste("Max Price: $", format_price(max_price)),
      br(),  # Quebra de linha
      paste("Min Price: $", format_price(min_price))
    )
  })
  output$fig3 <- renderPlotly({
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    plot_ly(data3_clean,
                                       type = "scattermapbox",
                                       mode = "markers",
                                       lat = ~latitude,
                                       lon = ~longitude,
                                       text = ~paste("Town: ", town, 
                                                     "<br>Price: $", format_price(resale_price)), 
                                       hoverinfo = "text",  # Display town and price on hover
                                       marker = list(size = ~resale_price / 200, color = "green", opacity = 0.7)
  ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",  
          center = list(lat = mean(data3_clean$latitude), lon = mean(data3_clean$longitude)),
          zoom = 10  
        )
      )
  })
  output$stats_textfig4 <- renderUI({
    avg_price <- mean(data4_clean$resale_price)
    median_price <- median(data4_clean$resale_price)
    max_price <- max(data4_clean$resale_price)
    min_price <- min(data4_clean$resale_price)
    
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Median Price: $", format_price(median_price)),
      br(),  # Quebra de linha
      paste("Max Price: $", format_price(max_price)),
      br(),  # Quebra de linha
      paste("Min Price: $", format_price(min_price))
    )
  })
  output$fig4 <- renderPlotly({
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    plot_ly(data4_clean,
                                       type = "scattermapbox",
                                       mode = "markers",
                                       lat = ~latitude,
                                       lon = ~longitude,
                                       text = ~paste("Town: ", town, 
                                                     "<br>Price: $", format_price(resale_price)), 
                                       hoverinfo = "text",   # Display town and price on hover
                                       marker = list(size = ~resale_price / 200, color = "yellow", opacity = 0.7)
  ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",  
          center = list(lat = mean(data4_clean$latitude), lon = mean(data4_clean$longitude)),
          zoom = 10  
        )
      )
    
  })
  output$stats_textfig5 <- renderUI({
    avg_price <- mean(data5_clean$resale_price)
    median_price <- median(data5_clean$resale_price)
    max_price <- max(data5_clean$resale_price)
    min_price <- min(data5_clean$resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Median Price: $", format_price(median_price)),
      br(),  # Quebra de linha
      paste("Max Price: $", format_price(max_price)),
      br(),  # Quebra de linha
      paste("Min Price: $", format_price(min_price))
    )
  })
  output$fig5 <- renderPlotly({  
    format_price <- function(x) {
    formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
  }
    plot_ly(data5_clean,
            type = "scattermapbox",
            mode = "markers",
            lat = ~latitude,
            lon = ~longitude,
            text = ~paste("Town: ", town, 
                          "<br>Price: $", format_price(resale_price)), 
            hoverinfo = "text",   # Display town and price on hover
            marker = list(size = ~resale_price / 200, color = "fuchsia", opacity = 0.7)
  ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",  
          center = list(lat = mean(data5_clean$latitude), lon = mean(data5_clean$longitude)),
          zoom = 10  
        )
      )
    
  })
  
  output$textfig1_town <- renderUI({
    avg_price <- mean(data1_by_region$avg_resale_price)
    var_price <- var(data1_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$fig1_town <- renderPlotly({plot_ly(data1_by_region,
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
  })
  output$textfig2_town <- renderUI({
    avg_price <- mean(data2_by_region$avg_resale_price)
    var_price <- var(data2_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$fig2_town <- renderPlotly({plot_ly(data2_by_region,
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
  })
  
  output$textfig3_town <- renderUI({
    avg_price <- mean(data3_by_region$avg_resale_price)
    var_price <- var(data3_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$fig3_town <- renderPlotly({plot_ly(data3_by_region,
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
  })
  
  output$textfig4_town <- renderUI({
    avg_price <- mean(data4_by_region$avg_resale_price)
    var_price <- var(data4_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$fig4_town <- renderPlotly({plot_ly(data4_by_region,
                                            type = "scattermapbox",
                                            mode = "markers",
                                            lat = ~latitude,
                                            lon = ~longitude,
                                            text = ~paste("Town: ", town, "<br>Price: $",  round(avg_resale_price, 2)),  # Display town and price on hover
                                            marker = list(size = ~avg_resale_price / 200, color = "yellow", opacity = 0.7)
  ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",  
          center = list(lat = mean(data4_by_region$latitude), lon = mean(data4_by_region$longitude)),
          zoom = 10  
        )
      )
  })
  
  output$textfig5_town <- renderUI({
    avg_price <- mean(data5_by_region$avg_resale_price)
    var_price <- var(data5_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$textfig5_town <- renderUI({
    avg_price <- mean(data5_by_region$avg_resale_price)
    var_price <- var(data5_by_region$avg_resale_price)
    format_price <- function(x) {
      formatC(x * 100, format = "f", big.mark = ".", decimal.mark = ",", digits = 2)
    }
    
    # Construir o conteúdo HTML com as estatísticas
    tagList(
      tags$strong(paste("Average Price: $", format_price(avg_price))),  
      br(), 
      paste("Price Variance: $", format_price(var_price))
    )
  })
  output$fig5_town <- renderPlotly({plot_ly(data5_by_region,
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
  })
  missing_towns <- setdiff(data1_clean$town, data2_clean$town)
  new_towns <- setdiff(data2_clean$town, data1_clean$town)
  non_residential_note <- " (Town transitioned to non-residential use by the early 2000s.)"
  new_towns_note <- " (Both were underdeveloped in the 1990s with very few resale flats and were newly developed towns in the 2000s.)" 
  output$town_differences<- renderText({
    paste0(
      "Towns missing in 2000-2023: ", paste(missing_towns, collapse = ", "),
      non_residential_note, "\n\n",
      "Towns added in 2000-2023: ", paste(new_towns, collapse = ", "),
      new_towns_note
    )
  })
  output$descriptive_table <- renderDT({
    formatted_data <- alldata_by_year[,-1]
    formatted_data$AvgSalePrice <- as.numeric(formatted_data$AvgSalePrice)
    formatted_data$AvgSalePrice <- round(formatted_data$AvgSalePrice, 2)  # Round to 2 decimal places
    
    
    # Create the interactive table with custom options
    datatable(
      formatted_data[, c(1:3)], 
      colnames = c('Year',"Average Sale Price", 'Town'), 
      rownames = FALSE, # Rename columns
      options = list(
        lengthChange = TRUE,  # Allow the user to change the number of rows per page
        pageLength = 10,  # Set the default number of rows per page
        dom = 'lfrtip',    # Filter, table, pagination (You can modify this if you want buttons or other features)
        searching = TRUE,  # Enable search box
        ordering = TRUE,  # Enable column sorting
        orderMulti = TRUE, 
        responsive = TRUE,  # Responsive design
        lengthMenu = list(c(10, 20, 30, -1), c('10', '20', '30', 'All')),  # Dropdown for rows per page
        scrollX = FALSE,  # Disable horizontal scrolling
        scrollY = '300px',
        order = list(list(1, 'asc'))# Set vertical scrolling
      )
    )
  })
  
  df1 <- bind_rows(
    data1_clean %>% select(resale_price) %>% mutate(Year = "1990/1999"),
    data2_clean %>% select(resale_price) %>% mutate(Year = "2000/2012"),
    data3_clean %>% select(resale_price) %>% mutate(Year = "2012/2014"),
    data4_clean %>% select(resale_price) %>% mutate(Year = "2015/2016"),
    data5_clean %>% select(resale_price) %>% mutate(Year = "2017/2023")
  )
  output$descriptive_plot <- renderPlotly({
    p2 <- ggplot(df1, aes(x = Year, y = resale_price, fill = Year))+
      geom_boxplot() +
      labs(
        title = "Summary of Resale Prices by Period",
        subtitle = "Boxplot showing resale price distribution per year group",
        x = "Period",
        y = "Resale Price (SGD)",
        fill = ""
      ) +
      scale_fill_manual(values = c(
        "1990/1999" = "red",
        "2000/2012" = "blue",
        "2012/2014" = "green",
        "2015/2016" = "yellow",
        "2017/2023" = "pink"
      )) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = function(x) scales::number(x, big.mark = ".")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#2C3E50"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#34495E"),
        axis.text.y = element_text(color = "#34495E"),
        panel.grid.major = element_line(color = "#D5D8DC"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p2) %>%
      layout(
        yaxis = list(
          tickformat = "~,.2f"  # two decimals with dot decimal separator
        )
      )
  })
  

  
  output$data_dictionary <- renderTable({
    data.frame(
      Variable = c("Latitude",
                   'Longitude',
                   'Postal Code',
                   'Address',
                  "Closest MRT Station", 
                   "Distance to closest MRT",
                   "Distance to Central Business District",
                  'Month',
                  'Town',
                  'Flat Type',
                  'Block',
                  'Street Name',
                  'Storey Range',
                  'Floor Area',
                  'Falt Model',
                  "Lease Commence",
                  'Remaining Lease',
                   "Resale Price", 
                  'Years Remaining'
                   ), 
      
      Description = c('Latitude coordinate.',
                      'Longitude coordinate.',
                      'Postal code (6-digit).',
                      'Street address.',
                      "Name of closest Mass Rapid Transit (MRT) station.", 
                      "Distance (m) to closest MRT station.", 
                      "Distance (m) to Central Business District.",
                      'Transaction date (YYYY-MM).',
                      "Name of the neighbourhood in which the flat is located.",
                      'Generic type of flat. ("3 ROOM", "4 ROOM"...)',
                      'Block number.',
                      'Street name.',
                      'Height of the flat.',
                      'Floor area (m²).',
                      'Model/subtype of flat.',
                      "The year that the leasehold contract of the flat began. (Most flats have 99-year leases).",
                      'Remaining lease of the contract in years and months, from Jan 2015 to Dec 2016. (Not all files have this column).',
                      "The transacted resale price as agreed between buyer and seller.",
                      'Years remaining on the lease.')
 ) })
  
  output$fig1_area <- renderPlotly({plot_ly(
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
      )})
  output$fig2_area <- renderPlotly({plot_ly(
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
      )})
  output$fig3_area <- renderPlotly({plot_ly(
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
      )})  
  output$fig4_area <- renderPlotly({plot_ly(
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
    )})
  
  output$fig5_area <- renderPlotly({plot_ly(
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
      )})
  
  

  
  df2 <- bind_rows(
    data1_clean %>% select(resale_price) %>% mutate(Year = "1990/1999"),
    data2_clean %>% select(resale_price) %>% mutate(Year = "2000/2012"),
    data3_clean %>% select(resale_price) %>% mutate(Year = "2012/2014"),
    data4_clean %>% select(resale_price) %>% mutate(Year = "2015/2016"),
    data5_clean %>% select(resale_price) %>% mutate(Year = "2017/2023")
  )
  output$lineplot <- renderPlotly({
    p2 <- ggplot(df2, aes(x = Year, y = resale_price, fill = Year)) +
      geom_col() +
      labs(
        title = "Distribution of Resale Prices by Period",
        subtitle = "Bar chart showing resale price per year group",
        x = "Period",
        y = "Resale Price (SGD)",
        fill = ""
      ) +
      scale_fill_manual(values = c(
        "1990/1999" = "red",
        "2000/2012" = "blue",
        "2012/2014" = "green",
        "2015/2016" = "yellow",
        "2017/2023" = "pink"
      )) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 6),
        labels = function(x) scales::number(x,decimal.mark = ",")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#2C3E50"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#34495E"),
        axis.text.y = element_text(color = "#34495E"),
        panel.grid.major = element_line(color = "#D5D8DC"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p2)
  })
  
  output$median_desc <- renderText({
    median_all <- median(df1$resale_price, na.rm = TRUE)
    format(median_all, big.mark = ".", decimal.mark = ",")
  })
  
  output$highest_desc <- renderText({
    max_all <- max(df1$resale_price, na.rm = TRUE)
    format(max_all, big.mark = ".", decimal.mark = ",")
  })
  
  output$transactions_desc <- renderText({
    n_all <- nrow(df1)
    format(n_all, big.mark = ".")
  })
  
  
  df_summary <- aggregate(resale_price ~ Year, data = df1, FUN = function(x) mean(x, na.rm = TRUE))
  names(df_summary)[2] <- "avg_price"

  
  output$avgPricePlot <- renderPlotly({
    df_summary <- aggregate(resale_price ~ Year, data = df1, FUN = function(x) mean(x, na.rm = TRUE))
    names(df_summary)[2] <- "avg_price"
    
    p <- ggplot(df_summary, aes(x = Year, y = avg_price, group = 1)) +
      geom_line(color = "#2C3E50", size = 1.2) +
      geom_point(color = "#E74C3C", size = 3) +
      geom_text(aes(label = scales::comma(avg_price, big.mark = ".", decimal.mark = ",")),
                vjust = -1, size = 4, color = "#34495E", fontface = "bold") +
      scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
      labs(
        title = "Evolution of Average Resale Price by Period",
        subtitle = "Analysis of average prices grouped by period",
        x = "Period",
        y = "Average Price (SGD)",
        caption = "Source: Resale housing data"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18, color = "#2C3E50"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#34495E"),
        axis.text.y = element_text(color = "#34495E"),
        panel.grid.major = element_line(color = "#D5D8DC"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p)
  })
   }

# UI Layout
ui <- dashboardPage(
  help = NULL,
  fullscreen = TRUE,
  
  # Header -----
  dashboardHeader(
    title = dashboardBrand(
      title = "Singapore Resell Prices"
    )),
  
  # Sidebar -----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem('About the Data', tabName = 'about_data', icon = icon('database')),
      menuItem("Total Resale Prices", tabName = "dashboard", icon = icon("database")), 
      menuItem("Area Average Resale Values", tabName = 'dashboard1', icon=icon('database')),
      menuItem("Town Average Resale Values", tabName = "dashboard2", icon = icon("database"))#,
     # menuItem("Model and Predictions (Area)", tabName = 'dashboard3', icon=icon('database'))
     
     )
  ),
  
  # Body -----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          # Custom Box for the "Welcome" section
          box(
            title = "Welcome!", 
            status = "success",  # You can use any status color here like info, success, danger, etc.
            width = 12, 
            solidHeader = F,
            collapsible = F,
            height = "auto",
            style = "background-color: #f4f4f4; color: #000;",  # Neutral background with black text
            HTML("<h3><b>Singapore home resale prices</b></h3>
              <p>This dashboard presents insights derived from data collected from official Singapore government sources.</p>
              <h6><b>📅 Years Covered:</b> 1990–2023</h6>
             <h6><b>🔄 Last Updated:</b> 2024</h6>
  
  <h6><b>Key Insights:</b></h6>
  <ul>
    <li>Steady price increase since 2012</li>
    <li>Significant variation across towns and areas</li>
    <li>Central regions tend to have higher resale prices</li>
  </ul>
              <p><a href='https://www.kaggle.com/datasets/talietzin/singapore-hdb-resale-prices-1990-to-2023' class='btn btn-success' target='_blank'>📥 Download Dataset</a></p>"
        ))),
          box(
          collapsible = TRUE,
          width = 12,
          title = "Data Dictionary",
          status = "white",
          tableOutput("data_dictionary")
        )
      ),
      tabItem(
       tabName = "about_data", 
    h2('About the Data', style = "text-align: center;"),
    fluidRow(
      # Custom Box for the "Welcome" section
      box(
        title = "", 
        status = "success",  # You can use any status color here like info, success, danger, etc.
        width = 12, 
        solidHeader = F,
        collapsible = F,
        height = "auto",
        style = "background-color: #f4f4f4; color: #000;",  # Neutral background with black text
        tagList(
          HTML("
      <h3><b>Exploratory Data Analysis</b></h3>
      <h6>Resale prices are grouped by the year or time period in which they were recorded or observed.</h6>
     
    "),
          hr(),
          div(
            style = "display: flex; justify-content: space-around; text-align: center;",
            div(style = "flex: 1; padding: 10px;",
                h4("Median Sale Price"),
                textOutput("median_desc")
            ),
            div(style = "flex: 1; padding: 10px;",
                h4("Highest Recorded Price"),
                textOutput("highest_desc")
            ),
            div(style = "flex: 1; padding: 10px;",
                h4("Number of Transactions"),
                textOutput("transactions_desc")
            )
          )
        )
      )),
          fluidRow(         
           box(
          collapsible =TRUE,
            width = 6,
            title = "Overview Analysis",
          HTML('<h6>Hold Shift to select multiple columns, then sort them in ascending or descending order.</h6>'),
           status = "purple",
           DTOutput("descriptive_table")
         ),
         tabBox(
           title = "Resale Prices by Year",
           width = 6,
           status = "purple",
           tabPanel("Boxplot", plotlyOutput("descriptive_plot")),
           tabPanel("Barplot", plotlyOutput("lineplot")),
           tabPanel('Time trends', plotlyOutput("avgPricePlot"))
         )
          )
      ),
    
      tabItem(
        tabName = "dashboard", h2("Total Resale Values for Each Observed Property", style = "text-align: center;"),
        # Info boxes -----
        fluidRow(
          class = "d-flex justify-content-center",  # Centraliza os elementos
          column(width = 3, infoBox(width = 12, title = "Properties observed in 1990-1999:", value = 5771, icon = icon("building"), color = "danger")),
          column(width = 3, infoBox(width = 12, title = "Properties observed in 2000-2012:", value = 8248, icon = icon("building"), color = "primary")),
          column(width = 3, infoBox(width = 12, title = "Properties observed in 2012-2014:", value = 8021, icon = icon("building"), color = "success")),
          column(width = 3, infoBox(width = 12, title = "Properties observed in 2017-2023:", value = 9439, icon = icon("building"), color = "fuchsia"))
        ),
        
        
        # Sortable boxes -----
        fluidRow(
          sortable(
            width = 12,
            style = "display: flex; flex-wrap: wrap;",  # Garante que as colunas fiquem em linha e quebrem
            
            # Primeira linha (1,1), (1,2), (1,3)
            div(
              class = "col-md-4",
              box(
                title = "2017-2023",  # (1,1)
                width = 12,
                status = "fuchsia",
                collapsible = FALSE,
                uiOutput("stats_textfig5"),
                plotlyOutput("fig5")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "2015-2016",  # (1,2)
                width = 12,
                status = "warning",
                collapsible = FALSE,
                uiOutput("stats_textfig4"),
                plotlyOutput("fig4")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "2012-2014",  # (1,3)
                width = 12,
                status = "success",
                collapsible = FALSE,
                uiOutput("stats_textfig3"),
                plotlyOutput("fig3")
              )
            ),
            
            # Segunda linha (2,1), (2,2), (2,3) [agora todos são "sortable"]
            div(
              class = "col-md-4",
              box(
                title = "2000-2012",  # (2,1)
                width = 12,
                status = "primary",
                collapsible = FALSE,
                uiOutput("stats_textfig2"),
                plotlyOutput("fig2")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "1990-1999",  # (2,2)
                width = 12,
                status = "danger",
                collapsible = FALSE,
                uiOutput("stats_textfig1"),
                plotlyOutput("fig1")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "Move this to arrange boxes",  # (2,3) caixa adicional, para tornar interativo
                width = 12,
                status = "secondary",
                collapsible = FALSE,
                " "
              )
            )
          )
        )
        
        
        
      ),
    tabItem(
      tabName='dashboard1', h2('Average Home Resale Values by Area ', style="text-align: center;"),
          # Sortable boxes -----
          fluidRow(
            sortable(
              width = 12,
              style = "display: flex; flex-wrap: wrap;",  # Garante que as colunas fiquem em linha e quebrem
              
              # Primeira linha (1,1), (1,2), (1,3)
              div(
                class = "col-md-4",
                box(
                  title = "2017-2023",  # (1,1)
                  width = 12,
                  status = "fuchsia",
                  collapsible = FALSE,
                  uiOutput('textfig5_town'),
                  plotlyOutput("fig5_area")
                )
              ),
              div(
                class = "col-md-4",
                box(
                  title = "2015-2016",  # (1,2)
                  width = 12,
                  status = "warning",
                  collapsible = FALSE,
                  uiOutput('textfig4_town'),
                  plotlyOutput("fig4_area")
                )
              ),
              div(
                class = "col-md-4",
                box(
                  title = "2012-2014",  # (1,3)
                  width = 12,
                  status = "success",
                  collapsible = FALSE,
                  uiOutput('textfig3_town'),
                  plotlyOutput("fig3_area")
                )
              ),
              
              # Segunda linha (2,1), (2,2), (2,3) [agora todos são "sortable"]
              div(
                class = "col-md-4",
                box(
                  title = "2000-2012",  # (2,1)
                  width = 12,
                  status = "primary",
                  collapsible = FALSE,
                  uiOutput('textfig2_town'),
                  plotlyOutput("fig2_area")
                )
              ),
              div(
                class = "col-md-4",
                box(
                  title = "1990-1999",  # (2,2)
                  width = 12,
                  status = "danger",
                  collapsible = FALSE,
                  uiOutput('textfig1_town'),
                  plotlyOutput("fig1_area")
                )
              ),
              div(
                class = "col-md-4",
                box(
                  title = "Move this to arrange boxes",  # (2,3) caixa adicional, para tornar interativo
                  width = 12,
                  status = "secondary",
                  collapsible = FALSE,
                  " "
                )
              )
            )
          )),
    
    
    tabItem(
      tabName = "dashboard2", h2("Average Home Resale Values by Town", style = "text-align: center;"),
      #fluidRow(
       #   class = "d-flex justify-content-center",  # Centraliza os elementos
        #  column(width = 2, infoBox(width = 12, title = "Towns in 1990-1999:", value = 25, icon = icon("building"), color = "danger")),
         # column(width = 2, infoBox(width = 12, title = "Towns in 2000-2023:", value = 26, icon = icon("building"), color = "primary")),
          #        ),
      fluidRow(
      box(
        title = "Differences in Towns Between 1990-1999 and 2000-2023",
        width = 12,
        status = "success",
        solidHeader = F,
        height = "auto",
        style = "background-color: #f4f4f4; color: #000;",  # Neutral background with black text
        verbatimTextOutput("town_differences")
      )
      ),
        # Sortable boxes -----
        fluidRow(
          sortable(
            width = 12,
            style = "display: flex; flex-wrap: wrap;",  # Garante que as colunas fiquem em linha e quebrem
            
            # Primeira linha (1,1), (1,2), (1,3)
            div(
              class = "col-md-4",
              box(
                title = "2017-2023",  # (2,2)
                width = 12,
                status = "fuchsia",
                collapsible = FALSE,
                plotlyOutput("fig5_town")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "2015-2016",  # (2,1)
                width = 12,
                status = "warning",
                collapsible = FALSE,
                plotlyOutput("fig4_town")
              )
            ),
     
            div(
              class = "col-md-4",
              box(
                title = "2012-2014",  # (1,3)
                width = 12,
                status = "success",
                collapsible = FALSE,
                plotlyOutput("fig3_town")
              )
            ),
            
            # Segunda linha (2,1), (2,2), (2,3) [agora todos são "sortable"]
            div(
              class = "col-md-4",
              box(
                title = "2000-2012",  # (1,2)
                width = 12,
                status = "primary",
                collapsible = FALSE,
                plotlyOutput("fig2_town")
              )
            ),
            
            div(
              class = "col-md-4",
              box(
                title = "1990-1999",  # (1,1)
                width = 12,
                status = "danger",
                collapsible = FALSE,
                plotlyOutput("fig1_town")
              )
            ),
            div(
              class = "col-md-4",
              box(
                title = "Move this to arrange boxes",  # (2,3) caixa adicional, para tornar interativo
                width = 12,
                status = "secondary",
                collapsible = FALSE, " "
              )
            )
          )
        )
      )#,
    #tabItem(       # dashboard3
    #  tabName = 'dashboard3',
     # h2("Model and Predictions (Area)", style="text-align: center;"),
      #fluidRow(
       # box(title = "Model and Predictions (Area)", width = 12)
      #)
    #)
    ) 
    
    
    ),  
  # Controlbar -----
  controlbar = dashboardControlbar(),
  
  # Footer -----
  dashboardFooter(left = "Letícia Cocato", right = "2025")
)

# Run the application 
shinyApp(ui = ui, server = server)
  
