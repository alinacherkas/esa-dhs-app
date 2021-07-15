# this app uses extracted indicators geo-spatial data from DHS programme for selected African countries

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
source("plotting.R")

# ggplot settings
theme_set(theme_minimal())

# data preparation
df.geometry <- readOGR(dsn = "esa-dhs-geometry-v21-04-04.geojson")
df.data <- read_parquet("esa-dhs-data-v21-04-04.parquet")
df.indicators <- read_parquet("esa-dhs-indicators-v21-04-04.parquet")
indicators <- df.indicators %>% select(IndicatorId, Definition) %>% distinct %$% split(IndicatorId, Definition)

ui <- fluidPage(
  
  # changing switch color
  tags$head(
    tags$style(
      HTML(".bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-on,
            .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-on {
              background: #d94b41;
              color: #FFFFFF;
              }")
    )
  ),
  
  # enabling dashboard elements without dashboard layout
  useShinydashboard(),
  titlePanel(title = HTML("Demographic and Health Surveys in Eastern and Southern Africa"),
             windowTitle = "ESA DHS Data App"),
  theme = shinytheme("united"), #simplex lumen
  
  # configuring the sidebar
  sidebarLayout(
    sidebarPanel(
      width = 3, # out of 12 units
      
      conditionalPanel(
        condition = "input.tabselected == 1",
        
        h3("Select Filters"),
        
        pickerInput(inputId = "group",
                    label = h4("Indicator Group"),
                    choices = df.indicators %>% pull(Level1) %>% unique() %>% sort(),
                    multiple = F,
                    selected = NULL),
        
        uiOutput(outputId = "indicators"),
        
        switchInput(
          inputId = "reg",
          label = "Run Regression?", 
          labelWidth = "80px",
          onLabel = "Yes",
          offLabel = "No",
          onStatus = "on",
          offStatus = "off"
        ),
        
        uiOutput(outputId = "covars")
      ),
      
      hr(),
      h4("About the app"),
      includeHTML("www/about.html"),
      hr(),
      includeHTML("www/feedback.html")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(id = "tabselected",
                  tabPanel(title = HTML("<i class='fas fa-map-marked-alt'></i> Map"), value = 1,
                           br(),
                           
                           fluidRow(
                             valueBoxOutput(outputId = "macrocvg", width = 3),
                             valueBoxOutput(outputId = "microcvg", width = 3),
                             infoBoxOutput(outputId = "macroavg", width = 3),
                             infoBoxOutput(outputId = "microavg", width = 3)
                           ),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"),
                                         column(width = 12,
                                                leafletOutput(outputId = "map", width = "100%", height = 600)
                                         ),
                                         column(width = 12,
                                                fluidRow(
                                                  plotlyOutput(outputId = "hist", height = 300),
                                                  plotlyOutput(outputId = "scatter", height = 300)
                                                )
                                         )
                             ),
                             
                             br(),
                             verbatimTextOutput(outputId = "regression")
                           )
                  ), 
                  
                  
                  tabPanel(title =  HTML("<i class='far fa-chart-bar'></i> Overview"), value = 2,
                           br(),
                           HTML("<b>This section is under development.</b>")
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$indicators <- renderUI({
    req(input$group)
    choices <- df.indicators %>% filter(Level1 == input$group) %>% pull(Definition) %>% unique() %>% sort()
    pickerInput(inputId = "indicator",
                label = h4("Indicator"),
                choices = choices,
                multiple = F,
                selected = NULL,
                choicesOpt = list(content = str_trunc(choices, width = 150, side = "right", ellipsis = "..."))
    )
  })
  
  output$covars <- renderUI({
    req(input$indicator)
    choices <- df.indicators %>% filter(Definition != input$indicator) %>% 
      pull(Definition) %>% unique() %>% sort()
    
    if (input$reg){
      tagList(
        withMathJax("Spatial regression is estimated with: $$y = \\rho W_{queen} y + X \\beta + \\epsilon$$"),
        pickerInput(inputId = "covars",
                    label = h4("Dependent Variables (up to 5)"),
                    choices = choices,
                    multiple = T,
                    selected = NULL,
                    choicesOpt = list(content = str_trunc(choices, width = 150, side = "right", ellipsis = "...")),
                    options = pickerOptions(
                      noneSelectedText = "None",
                      maxOptions = 5,
                      actionsBox = T)
        )
      )
    } else {
      return(NULL)
    }
  })
  
  select_data <- reactive({
    indicator <- indicators[[input$indicator]][1]
    
    df.lambda <- df.data %>% rename(IndicatorValue = !!indicator) %>%  
      select(RegionID, IndicatorValue) %>% 
      merge(df.geometry, ., by = "RegionID", duplicateGeoms = T)
    
    return(df.lambda)
  })
  
  select_covars <- reactive({
    depvar <- indicators[[input$indicator]][1]
    covars <- indicators[input$covars] %>% unlist(use.names = F)
    
    df.lambda <- df.data %>% rename(IndicatorValue = !!depvar) %>%  
      select(c(RegionID, IndicatorValue, covars)) %>% 
      merge(df.geometry, ., by = "RegionID", duplicateGeoms = T)
    
    df.lambda <- df.lambda[complete.cases(df.lambda@data),]
    
    return(df.lambda)
  })
  
  # outputs
  output$macrocvg <- renderValueBox({
    req(input$indicator)
    value <- select_data() %>% 
      slot(name = "data") %>% 
      filter(!is.na(IndicatorValue)) %>%
      pull(CountryCode) %>% 
      n_distinct()
    
    valueBox(value = value, subtitle = "out of 21 ESA countries reported this indicator",
             icon = icon("fas fa-globe-africa"), width = 3, color = "red", href = NULL)
  })
  
  output$microcvg <- renderValueBox({
    req(input$indicator)
    value <- select_data() %>% 
      slot(name = "data") %>% 
      pull(IndicatorValue) %>% 
      is.na() %>% 
      not() %>% 
      sum()
    
    valueBox(value = value, subtitle = "out of 175 ESA regions reported this indicator", 
             icon = icon("fas fa-globe-africa"), width = 3, color = "red", href = NULL)
  })
  
  output$macroavg <- renderInfoBox({
    req(input$indicator)
    value <- select_data() %>% 
      slot(name = "data") %>% 
      select(CountryCode, IndicatorValue) %>% 
      drop_na() %>% 
      group_by(CountryCode) %>% 
      summarise(IndicatorValue = mean(IndicatorValue)) %>%
      pull(IndicatorValue) %>%
      mean()
    
    infoBox(title = "Macro-average", value = sprintf("%.2f%%", value), subtitle = "i.e. unweighted mean",
            icon = icon("fas fa-calculator"), width = 3, color = "red", href = NULL, fill = T)
  })
  
  output$microavg <- renderInfoBox({
    req(input$indicator)
    value <- select_data() %>% 
      slot(name = "data") %>% 
      pull(IndicatorValue) %>%
      na.omit() %>%
      mean()
    
    infoBox(title = "Micro-average", value = sprintf("%.2f%%", value), subtitle = "i.e. weighted mean",
            icon = icon("fas fa-calculator"), width = 3, color = "red", href = NULL, fill = T)
  })
  
  output$map <- renderLeaflet({
    req(input$indicator)
    m <- select_data() %>% plot_map()
    return(m)
  })
  
  output$hist <- renderPlotly({
    req(input$indicator)
    p <- select_data() %>% slot(name = "data") %>% plot_hist()
    return(p)
  })
  
  output$scatter <- renderPlotly({
    req(input$indicator)
    p <- select_data() %>% plot_scatterplot()
    return(p)
  })
  
  output$regression <- renderPrint({
    req(input$covars)
    to.drop <- c("RegionID", "CountryCode", "SurveyId", "SurveyType", "SurveyYear", "CountryName")
    df.lambda <- select_covars()
    df.lambda@data %<>% select(-all_of(to.drop))
    
    list.queen <- poly2nb(df.lambda, queen = T)
    W <- nb2listw(list.queen, style = "W", zero.policy = T)
    sar.chi <- spatialreg::lagsarlm(IndicatorValue ~ ., data = df.lambda@data, W, zero.policy = T)
    
    return(summary(sar.chi))
  })
  
}

shinyApp(ui, server)