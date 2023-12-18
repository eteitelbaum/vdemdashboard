## Map Modules ----

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

## Line Chart Modules ----

# plotly line chart UI
plotlyLineChartModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("linechart"))
}

# plotly line chart server
plotlyLineChartModule <- function(input, output, session, data, selected_year = NULL, title = NULL, x_title = "", y_title = "") {
  
  output$linechart <- renderPlotly({
    data <- data()  
    title <- title()
    selected_year <- selected_year()
    x_title <- x_title
    y_title <- y_title()
  
  # Initialize a Plotly object
  fig <- plot_ly()
  
  # Add a trace for the data
  fig <- fig |>
    add_trace(
      type = 'scatter',
      mode = 'lines+markers',
      x = data$year,  
      y = data$yvar,  
      line = list(color = magma(256)[1]),  # Set line color
      marker = list(color = magma(256)[1]),  # Set marker color
      hovertemplate = paste('<i>Year</i>: %{x}<br><i>Value</i>: %{y}<extra></extra>'),
      showlegend = FALSE
    )
  
  # Filter to get the data for the selected year
  if (!is.null(selected_year) && selected_year %in% data$year) {
    selected_data <- data[data$year == selected_year, ]
  
  fig <- fig |>
    add_trace(
      type = 'scatter',
      mode = 'markers',
      x = selected_year,  # X-axis is the selected year
      y = selected_data$yvar,  # Y-axis is the value of the indicator for the selected year
      marker = list(color = magma(256)[196], size = 12),  # Highlight with a different color and size
      hovertemplate = paste('<i>Year</i>: %{x}<br><i>Value</i>: %{y}<extra></extra>'),
      showlegend = FALSE
    )
  }
  
  # Add layout details for axis labels and title
  fig <- fig |>
    layout(
      title = title,  
      xaxis = list(title = x_title),  
      yaxis = list(title = y_title)  
    )
  
  # Customize the toolbar
  fig <- fig |>
    config(
      displaylogo = FALSE,  # Remove the plotly logo
      #displayModeBar = TRUE,  # Ensure that the mode bar is displayed
      modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "autoScale2d", "lasso2d", "select2d", "zoom2d",
        "pan2d", "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
  
  fig
  })
}  

## Bar Chart Modules ----

# plotly bar chart UI
plotlyBarChartModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("barchart"))
}

# plotly bar chart server

plotlyBarChartModule <- function(input, 
                                 output, 
                                 session, 
                                 data, 
                                 title = NULL, 
                                 x_title = "", 
                                 y_title = "", 
                                 highlight_col = NULL, 
                                 color1 = "red", 
                                 color2 = "steelblue") {
  output$barchart <- renderPlotly({
    # Access the reactive data and title 
    data <- data()
    x_title <- x_title()
      
    # Check if the highlight column exists in the data
    highlight_col_val <- if (!is.null(highlight_col) && highlight_col() %in% names(data)) highlight_col() else NULL
    
    # Define colors based on the highlight column
    # Note: let's come back later and maybe change this to dplyr
    colors <- if (!is.null(highlight_col_val)) {
      ifelse(data[[highlight_col_val]], color1, color2)
    } else {
      rep(color2, nrow(data))
    }
    
    # Create the plotly object
    fig <- plot_ly(data = data, x = ~xvar, y = ~yvar, type = 'bar', orientation = 'h',
                   marker = list(color = colors))
    
    # Add layout details for axis labels and title
    fig <- fig |>
      layout(
              title = title,
              xaxis = list(title = x_title),
              yaxis = list(
                title = y_title,
                ticktext = ~yvar, 
                tickvals = ~yvar,
                tickfont = list(size = 8)),
              hovermode = 'y'
              )
    
    # Apply configuration settings
    fig <- fig |>
      config(
        displaylogo = FALSE,  # Remove the plotly logo
        modeBarButtonsToRemove = list(
          "zoomIn2d", "zoomOut2d", "autoScale2d", "lasso2d", "select2d", "zoom2d",
          "pan2d", "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
    
    fig
  })
}

## Scatter Plot Modules---- 

# plotly scatter plot UI
plotlyScatterPlotModuleUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("scatterplot"))
}

# plotly scatter plot server
plotlyScatterPlotModule <- function(
    input, 
    output, 
    session, 
    data, 
    title = NULL, 
    x_title = "", 
    y_title = "", 
    size = NULL, 
    color = NULL, 
    symbol = NULL, 
    highlight_col = NULL,
    color1 = "red",
    color2 = "steelblue",
    selected_country = selected_country) {
  
  # Create scatter plotoutput
  # Inside your Shiny module server function
  output$scatterplot <- renderPlotly({
    # Access the reactive data and title 
    data <- data()
    y_title <- y_title()
    
    # # Fit a loess model
    # lm_fit <- lm(yvar ~ xvar, data = data)
    # 
    # # Predict values using the loess model
    # data$y_pred <- predict(lm_fit)
    # 
    # # Calculate 95% confidence interval
    # predictions <- predict(lm_fit, se = TRUE)
    # ci <- 1.96 * predictions$se.fit
    # data$y_upper <- data$y_pred + ci
    # data$y_lower <- data$y_pred - ci
    # 
    # #print(data)
    # 
    #   fig <- plot_ly(data = data, x = ~xvar,
    #                  text = ~paste("Country:", country_name, "<br>", 
    #                                x_title, ":", gdp_pc, "<br>", 
    #                                y_title, ":", yvar),
    #                  hoverinfo = 'text') |>
    #   add_markers(y = ~yvar, 
    #               marker = list(color = color1),
    #               showlegend = FALSE) |>
    #   add_lines(y = ~y_pred, 
    #             line = list(color = color2),
    #             showlegend = FALSE) |>
    #   add_ribbons(ymin = ~y_lower, ymax = ~y_upper, x = ~xvar, 
    #             line = list(color = 'transparent'), 
    #             fillcolor = 'rgba(173, 216, 230, 0.2)', 
    #             showlegend = FALSE)
    
    # Create the initial scatter plot
    fig <- plot_ly(data = data,
                   x = ~xvar, y = ~yvar,
                   type = 'scatter',
                   mode = 'markers',
                   marker = list(color = color1),
                   text = ~paste("Country:", country_name, "<br>",
                                 x_title, ":", gdp_pc, "<br>",
                                 y_title, ":", yvar),
                   hoverinfo = 'text')

    # # Add the loess curve to the plot
    # fig <- fig |>
    #   add_lines(
    #     y = ~y_pred,
    #     line = list(color = 'blue', width = 2))
    #   )

    # # # Add the loess curve to the plot
    # fig <- fig %>%
    #   add_trace(
    #     data = data,
    #     x = ~xvar, y = ~y_pred,
    #     type = 'scatter', mode = 'lines', line = list(color = 'blue', width = 2)
    #   )
    
    # # Plot upper CI
    # fig <- fig %>%
    #   add_trace(
    #     x = ~xvar, y = ~y_upper,
    #     type = 'scatter', mode = 'lines', line = list(color = 'transparent'),
    #     showlegend = FALSE
    #   )
    # 
    # # Plot lower CI and fill
    # fig <- fig %>%
    #   add_trace(
    #     x = ~xvar, y = ~y_lower,
    #     type = 'scatter', mode = 'lines', line = list(color = 'transparent'),
    #     fill = 'tonexty', fillcolor = 'rgba(0, 0, 255, 0.2)',
    #     showlegend = FALSE
    #   )
    
    # Add bigger point for selected country
    if (!is.null(selected_country()) && selected_country() %in% data$country_name) {
      selected_data <- data[data$country_name == selected_country(), ]
      
      fig <- fig |>
        add_trace(
          data = selected_data,
          type = 'scatter',
          mode = 'markers',
          x = ~xvar,  # X-axis data for selected country
          y = ~yvar,  # Y-axis data for selected country
          marker = list(color = color2, size = 12),  # Highlight with a different color and size
          showlegend = FALSE
        )
    }
      
    # Add layout details for axis labels and title
    fig <- fig |>
      layout(
        title = title,
        xaxis = list(
          type = "log",
          title = x_title,
          tickvals = c(1000, 2500, 5000, 10000, 20000, 40000, 100000),
          ticktext = c("$1k", "$2.5k", "$5k", "$10k", "$20k", "$40k", "$100k")
          ),
        yaxis = list(title = y_title),
        hovermode = 'closest'
      )
    
    # Apply configuration settings
    fig <- fig |>
      config(
        displaylogo = FALSE,  # Remove the plotly logo
        modeBarButtonsToRemove = list(
          "zoomIn2d", "zoomOut2d", "autoScale2d", "lasso2d", "select2d", "zoom2d",
          "pan2d", "hoverClosestCartesian", "hoverCompareCartesian"
        )
      )
    
    fig
  })
}
