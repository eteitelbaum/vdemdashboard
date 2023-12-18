# load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tidyr)
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
          "Deliberative Democracy" = "v2x_delibdem",
          "Egalitarian Democracy" = "v2x_egaldem",
          "Civil Liberties" = "v2x_civlib",
          "Rule of Law" = "v2x_rule",
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
