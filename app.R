# Load global settings
source("global.R")

# Define UI
source("ui.R")

# Define server logic
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)

