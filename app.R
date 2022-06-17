# this app provides an interface to explore and analyse
# DHS data for countries in Eastern and Southern Africa

# config settings
config <- config::get(file = "config.yml", config = "default")

# shiny libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

# i/o
library(rgdal)
library(spdep)
library(arrow)

# data wrangling
library(tidyverse)
library(magrittr)

# data visualisation
library(plotly)
library(leaflet)

# utils
library(waiter)

# plotting settings
theme_set(theme_minimal())

# container object for datasets
datasets <- load_datasets(config)

# defining UI
ui <- fluidPage(
  
  # enabling dashboard elements without dashboard layout + waiter
  useShinydashboard(),
  useWaiter(),
  
  # setting css font styles
  includeCSS("./assets/css/styling.css"),
  
  # title panel using shiny modules
  titlePanel(
    title = "Demographic and Health Surveys in Eastern and Southern Africa",
    windowTitle = "ESA DHS Data App"
  ),
  tabsetPanel(
    tabPanel(
      title = HTML("<i class='fas fa-map-marked-alt'></i> Exploration"),
      explorationTabUI(id = "exploration", config = config, datasets = datasets)
    ),
    tabPanel(
      title = HTML("<i class='far fa-chart-bar'></i> Modelling"),
      modellingTabUI(id = "modelling", config = config, datasets = datasets)
    ),
    tabPanel(
      title = HTML("<i class='far fa-chart-bar'></i> About"),
      br(),
      includeHTML("./assets/html/about.html")
    ),
    footer = includeHTML("./assets/html/footer.html")
  ),
  theme = shinytheme("united")
)

server <- function(input, output, session) {
  # server functions from shiny modules
  explorationTabServer(id = "exploration", config = config, datasets = datasets)
  modellingTabServer(id = "modelling", config = config, datasets = datasets)
}

shinyApp(ui, server)