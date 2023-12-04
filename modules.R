## Map Modules

# leaflet country map UI
countryMapModuleUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"))
}

# leaflet country map server

countryMapModule <- function(input, output, session, map_data, labels, indicator, onClick = NULL) {
  output$map <- renderLeaflet({
    map_data <- map_data()  # Reactive data
    labels <- labels()      # Reactive labels
    
    leaflet(map_data, 
            options = leafletOptions(minZoom = 1, maxBounds = list(c(-90, -180), c(90, 180)), worldCopyJump = TRUE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |> 
      addPolygons(
        fillColor = ~colorNumeric("magma", map_data[[indicator()]])(map_data[[indicator()]]),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "1",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 2, color = "#666", dashArray = "0", fillOpacity = 0.7, bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
        layerId = ~country_name  # Assign a unique ID to each polygon for click events
      ) |>
      addLegend(
        "bottomleft",
        pal = colorNumeric("magma", map_data[[indicator()]]),
        values = ~map_data[[indicator()]],
        title = NULL,
        opacity = 0.7
      )
  })
  
  # Observer for click events
  observeEvent(input$map_shape_click, {
    if (is.function(onClick)) {
      onClick(input$map_shape_click)
    }
  })
}



