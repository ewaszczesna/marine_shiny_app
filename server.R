server <- function(input, output, session) {
  
  observeEvent(input$vessel_type, {
    
    marine_data_limited <- marine_data[marine_data$ship_type %in% input$vessel_type, ]
    
    update_dropdown_input(session = session, input_id = "vessel_name",
                          choices = sort(unique(marine_data_limited$SHIPNAME)))
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$vessel_name, {
    
    data_filtered <- marine_data[ship_type == input$vessel_type &
                                 SHIPNAME == input$vessel_name]
    
    data_filtered <- data_filtered[order(DATETIME),]

    data_filtered <- setDT(mutate(data_filtered,
                                  LON_prev = lag(LON),
                                  LAT_prev = lag(LAT),
                                  DATETIME_prev = as.POSIXct(lag(DATETIME),
                                                             format="%Y-%m-%dT%H:%M:%SZ",
                                                             tz="UTC"),
                                  DATETIME_after = as.POSIXct(DATETIME,
                                                              format="%Y-%m-%dT%H:%M:%SZ",
                                                              tz="UTC"),
                                  Distance = distHaversine(cbind(LON, LAT), cbind(lag(LON), lag(LAT))),
                                  Time = difftime(as.POSIXct(DATETIME, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"), 
                                                  as.POSIXct(lag(DATETIME), format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
                                                  units = "mins", "secs")
                                  ))
    
    data_max <- data_filtered[ Distance  == max(data_filtered$Distance, na.rm = T),]
    data_max <- data_max[ DATETIME == max(data_max$DATETIME),]
    
    output$flag <- renderText({ 
      paste("Flag: ", data_max$FLAG)
    })
    
    output$size <- renderText({ 
      paste("Size:  ", data_max$LENGTH, "x", data_max$WIDTH, "meters")
    })
    
    output$gps_data <- renderText({ 
      paste("GPS records: ", nrow(data_filtered))
    })
    
    output$longest_distance_speed <- renderText({ 
      paste("Speed: ", max(data_max$SPEED, na.rm = T), "knots")
    })
    
    output$latest_port <- renderText({ 
      paste("Latest port:  ", data_filtered[ DATETIME == max(data_filtered$DATETIME)]$PORT)
    })
    
    output$longest_distance <- renderText({ 
      paste("Distance: ", round(data_max$Distance), "meters")
    })
    
    output$longest_distance_date <- renderText({ 
      paste("Date: ", data_max$date)
    })
    
    output$longest_distance_time <- renderText({ 
      paste("Time: ", round(data_max$Time, 2), "minutes")
    })
    
    
    output$mymap <- renderLeaflet({
      leaflet(data_max) %>%
        addTiles() %>%
        addMarkers(~LON, ~LAT, popup = "Finish") %>%
        addMarkers(~LON_prev, ~LAT_prev, popup = "Start") %>%
        addPolylines(data = data_max,
                     lng = ~c(LON, LON_prev),
                     lat = ~c(LAT, LAT_prev))
    })
    
  })

}


