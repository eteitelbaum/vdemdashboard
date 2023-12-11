## Setup

# load packages
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(plotly)
library(viridis)

source("modules.R")

# load data
vdem_data <- readr::read_rds("vdem_data.rds") |>
  st_set_crs("+proj=longlat +datum=WGS84")

# make non-sf version of df
non_spatial_vdem <- as.data.frame(vdem_data)
non_spatial_vdem <- subset(non_spatial_vdem, select = -geometry)

# definte variables for indicator dropdown
vars <- c("Electoral Democracy" = "v2x_polyarchy",
          "Liberal Democracy" = "v2x_libdem",
          "Participatory Democracy" = "v2x_partipdem",
          "Civil Liberties" = "v2x_civlib",
          "Clientelism" = "v2xnp_client",
          "Corruption" = "v2xnp_regcorr",
          "Women's Empowerment" = "v2x_gender")

# define region choices
region_choices = list(
  "Global" = "Global",
  "Eastern Europe" = "Eastern Europe",
  "Latin America" = "Latin America",
  "Middle East" = "Middle East",
  "Africa" = "Africa",
  "The West" = "The West",
  "Asia" = "Asia"
)

## UI Function

ui <- fluidPage(
  titlePanel("Democracy Around the World"),
  
  # Define the rows and columns for the layout
  fluidRow(
    # Define the sidebar with a wellPanel within a 3-column wide area
    column(3,
           wellPanel(
             sliderInput("year", "Year", min = 1990, max = max(vdem_data$year),
                         value = 2010, sep = ""),
             selectInput("indicator", "Indicator", choices = vars),
             radioButtons("regions", "Regions",
                          choices = region_choices,
                          selected = "Global"),
             helpText(HTML("Use the checkbox above to explore democracy trends in a particular region. 
                  Click on the map to view the trends for an individual country. 
                  <br><br>
                  <strong>Notes:</strong> All data from the Varieties of Democracy (V-Dem) dataset.
                  Country boundaries are taken from the `rnaturalearth` package.Please note 
                  that the author does not endorse any particular set of boundaries
                  where borders are the subject of dispute."))
           )
    ),
    
    # Define the main panel to take up the remaining 9 columns
    column(9,
           fluidRow(
             column(6, countryMapModuleUI("map1")),
             column(6, plotlyLineChartModuleUI("linechart1"))
           ),
           fluidRow(
             column(6, plotlyBarChartModuleUI("barchart1")),
             column(6, plotlyScatterPlotModuleUI("scatterplot1"))
           )
    )
  )
)

## Server Function

server <- function(input, output) {
  
  ## Region selection
  
  # Observe changes to region selection
  observeEvent(input$regions, {
    if (input$regions == "Global") {
      # Reset the selected country and display global trend when "Global" is chosen
      selected_country(NULL)
    } else {
      # Reset the selected country when any specific region is chosen
      selected_country(NULL)
    }
  })
  
  ## Map 
  
  # Reactive expression to filter the map data
  map_data <- reactive({
    filtered_map_data <- vdem_data[vdem_data$year == input$year, ]
    selected_map_data <- filtered_map_data[, c("country_name", input$indicator, "geometry"), drop = FALSE]
    return(selected_map_data)
    })
  
  # Create labels for map
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%s: %g",
      map_data()$country_name,
      names(vars)[which(vars == input$indicator)],
      map_data()[[input$indicator]]
    ) |> lapply(htmltools::HTML)
  })
  
  # Reactive value for selected country
  selected_country <- reactiveVal(NULL)
  
  # Update the selected country when the map is clicked
  onClick <- function(click_data) {
    clicked_country <- click_data$id
    selected_country(clicked_country)
    }
  
  # Call map module
  callModule(countryMapModule, "map1", 
             map_data = map_data, 
             labels = labels, 
             indicator = reactive(input$indicator),
             onClick = onClick)

  ## Line Chart
  
  # Reactive function to filter data for line chart
  line_chart_data <- reactive({
    
    if (is.null(selected_country()) && input$regions == "Global") {
      # Global selected and no country selected, show global average
      line_chart_data <- non_spatial_vdem |>
        group_by(year) |>
        summarize(yvar = round(mean(!!sym(input$indicator), na.rm = TRUE), 3))
      
      title = "Global Trend" 
      
    } else if (!is.null(selected_country())) {
      # Country selected
      line_chart_data <- non_spatial_vdem |>
        filter(country_name == selected_country()) |>
        select(year, yvar = !!sym(input$indicator))
      
      title = selected_country() 
      
    } else {
      # Region selected, show trends for countries in region
      line_chart_data <- non_spatial_vdem |>
        filter(region == input$regions) |>
        group_by(year) |>
        summarize(yvar = round(mean(!!sym(input$indicator), na.rm = TRUE), 3))
      
      title = input$regions
    }
    
    list(data = line_chart_data, title = title)

  })  

  # Call plotly line chart module
  callModule(plotlyLineChartModule, "linechart1",
             data = reactive({ line_chart_data()$data }),
             selected_year = reactive( input$year ),
             title = reactive({ line_chart_data()$title }),
             x_title = "Year",
             y_title = reactive(names(vars[which(vars == input$indicator)]))
  )
  
  ## Bar Chart
  
  # Reactive function to filter data for bar chart
  # Note: is some of this redundant to wrangling for line chart?
  # Think about redoing this so that all scenarios include a highlight column
  
  bar_chart_data <- reactive({

    if (is.null(selected_country()) && input$regions == "Global") {
      # Global selected and no country selected, show global average
      bar_chart_data <- non_spatial_vdem |>
        filter(year == input$year, !is.na(region)) |>  # Exclude rows where region is NA
        group_by(region) |>
        summarize(xvar = round(mean(!!sym(input$indicator), na.rm = TRUE), 3)) |>
        mutate(yvar = region) |>
        arrange(xvar) 
      
    }  else if (!is.null(selected_country())) {
      # Find the region of the selected country
      ctry_from_region <- non_spatial_vdem |>
        filter(country_name == selected_country()) |>
        pull(region) |>
        unique()
      
      # Filter data to include only countries from the selected region
      bar_chart_data <- non_spatial_vdem |>
        filter(region == ctry_from_region, 
               year == input$year) |>
        mutate(xvar = !!sym(input$indicator),
               yvar = country_name,
               # Add a column to highlight the selected country
               highlight = country_name == selected_country()) |>
        arrange(xvar)
      
    } else {
      # Region selected, show values for countries in region
      bar_chart_data <- non_spatial_vdem |>
        filter(region == input$regions, 
               year == input$year) |>
        mutate(xvar = !!sym(input$indicator),
               yvar = country_name) |>
        arrange(xvar)
    }

    bar_chart_data

  })
  
  # Call plotly bar chart module
  
  callModule(plotlyBarChartModule, "barchart1", 
             data = reactive({ bar_chart_data() }), 
             x_title = reactive(names(vars[which(vars == input$indicator)])),
             highlight_col = reactive("highlight"),
             color1 = magma(256)[128], #128
             color2 = magma(256)[192]) #192

## Scatter Plot

# Think about redoing this so that all scenarios include a highlight column
  
# Reactive expression for scatter plot data
scatter_plot_data <- reactive({
  
  # Filter by selected year, drop rows with NA region
  filtered_plot_data <- non_spatial_vdem |>
    filter(year == input$year)
  
  # Scenraio 1: Global view 
  if (input$regions == "Global") {
    plot_data <- filtered_plot_data |>
      select(country_name, yvar = .data[[input$indicator]], xvar = e_gdppc, gdp_pc)

  # Scenario 2: Region selected
  } else {
    plot_data <- filtered_plot_data |>
      filter(region == input$regions) |>
      select(country_name, yvar = .data[[input$indicator]], xvar = e_gdppc, gdp_pc)
  }
  
  plot_data |> drop_na()
})

# Call plotly scatter plot module
callModule(plotlyScatterPlotModule, 
           "scatterplot1", 
           data = scatter_plot_data, 
           x_title = "GDP per capita",
           y_title = reactive(names(vars[which(vars == input$indicator)])),
           color1 = magma(256)[192], #192
           color2 = magma(256)[128],
           selected_country = selected_country)
}

## Run the app
shinyApp(ui = ui, server = server)

