## Setup

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(plotly)

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
  titlePanel("Democracy Around the World!"),
  
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
             helpText("Use the checkbox above to explore democracy trends in a particular region. 
                  Click on the map to view the trends for an individual country. 
                  All data from the Varieties of Democracy (V-Dem) dataset.")
           )
    ),
    
    # Define the main panel to take up the remaining 9 columns
    column(9,
           fluidRow(
             column(6, countryMapModuleUI("map1")),
             column(6, plotlyOutput("lineChart"))
           ),
           fluidRow(
             column(6, plotOutput("barChart")),
             column(6, plotOutput("histogram"))
           )
    )
  )
)

## Server Function

server <- function(input, output) {
  
  # Reactive expression to filter the map data
  map_data <- reactive({
    filtered_map_data <- vdem_data[vdem_data$year == input$year, ]
    selected_map_data <- filtered_map_data[, c("country_name", input$indicator, "geometry"), drop = FALSE]
    return(selected_map_data)
    }) 
  
  # Create labels
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
  
  # Call leaflet module
  callModule(countryMapModule, "map1", 
             map_data = map_data, 
             labels = labels, 
             indicator = reactive(input$indicator),
             onClick = onClick)

# Create line chart
  
  output$lineChart <- renderPlotly({
    if (input$regions == "Global" && is.null(selected_country())) {
      # Global selected and no country selected, show global average
      avg_data <- non_spatial_vdem |>
        group_by(year) |>
        summarize(mean = mean(!!sym(input$indicator), na.rm = TRUE))
      
      line_chart <- ggplot(avg_data, aes(x = year, y = mean)) +
        geom_line() +
        labs(x = "Year", 
             y = "Score", 
             title = paste("Global Trend in ", names(vars[which(vars == input$indicator)]))) +
        theme_minimal()
      
      ggplotly(line_chart)
      
    } else if(input$regions != "Global" && is.null(selected_country())) {
      # Region selected, show trends for countries in region
      region_data <- non_spatial_vdem |>
        filter(region == input$regions) |>
        group_by(year) |>
        summarize(mean = mean(!!sym(input$indicator), na.rm = TRUE))
      
      line_chart <- ggplot(region_data, aes(x = year, y = mean)) +
        geom_line() +
        labs(x = "Year",
             y = "Score",
             title = paste("Trend in", input$regions)) +
        theme_minimal() +
        scale_color_viridis_d(option = "magma")
      
      ggplotly(line_chart)
      
    }
      else{
      # Country selected, show its trend
      country_data <- non_spatial_vdem |>
        filter(country_name == selected_country()) |>
        select(year, !!sym(input$indicator))
      
      ggplot(country_data, aes(x = year, y = !!sym(input$indicator))) +
        geom_line() +
        labs(x = "Year", 
             y = "Score", 
             title = paste("Trend in", selected_country())) +
        theme_minimal()
    }
  })
}
  
## Run the app
shinyApp(ui = ui, server = server)


