# this module provides interface and functionality for the exploration tab

explorationTabUI <- function(id, config, datasets) {
  ns <- NS(id)
  
  tagList(
    # configuring the sidebar
    column(
      width = 3,
      wellPanel(
        h3("Select Filters"),
        
        pickerInput(
          inputId = ns("group"),
          label = h4("Indicator Group"),
          choices = datasets$df.indicators %>% pull(Level1) %>% unique() %>% sort(),
          multiple = F,
          selected = NULL
        ),
        
        pickerInput(
          inputId = ns("indicator"),
          label = h4("Indicator"),
          choices = NULL,
          multiple = F,
          selected = NULL
        ),
        
        hr(),
        includeHTML("./assets/html/feedback.html")
      )
    ),
    
    column(
      width = 9,
      br(),
      fluidRow(
        valueBoxOutput(outputId = ns("macrocvg"), width = 3),
        valueBoxOutput(outputId = ns("microcvg"), width = 3),
        infoBoxOutput(outputId = ns("macroavg"), width = 3),
        infoBoxOutput(outputId = ns("microavg"), width = 3)
      ),
      
      fluidRow(
        splitLayout(
          cellWidths = c("50%", "50%"),
          column(
            width = 12,
            leafletOutput(outputId = ns("map"), width = "100%", height = 600)
          ),
          column(
            width = 12,
            fluidRow(
              plotlyOutput(outputId = ns("hist"), height = 300),
              plotlyOutput(outputId = ns("scatter"), height = 300)
            )
          )
        )
      )
    )
  )
}

explorationTabServer <- function(id, config, datasets) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$group, {
        choices <- datasets$df.indicators %>% 
          filter(Level1 == input$group) %>% 
          pull(Definition) %>% unique() %>% 
          sort()
        
        updatePickerInput(
          session = session,
          inputId = "indicator",
          choices = choices,
          choicesOpt = list(content = str_trunc(choices, width = 150, side = "right", ellipsis = "..."))
        )
      })
      
      select_data <- reactive({
        indicator <- datasets$indicators[[input$indicator]][1]
        
        df.lambda <- datasets$df.data %>% 
          rename(IndicatorValue = !!indicator) %>%  
          select(RegionID, IndicatorValue) %>% 
          merge(datasets$df.geometry, ., by = "RegionID", duplicateGeoms = T)
        
        return(df.lambda)
      })
      
      # widgets
      output$macrocvg <- renderValueBox({
        req(input$indicator)
        
        value <- select_data() %>% 
          slot(name = "data") %>% 
          filter(!is.na(IndicatorValue)) %>%
          pull(CountryCode) %>% 
          n_distinct()
        
        valueBox(
          value = value,
          subtitle = "out of 21 ESA countries reported this indicator",
          icon = icon("fas fa-globe-africa"),
          width = 3,
          color = "red",
          href = NULL
        )
      })
      
      output$microcvg <- renderValueBox({
        req(input$indicator)
        value <- select_data() %>% 
          slot(name = "data") %>% 
          pull(IndicatorValue) %>% 
          is.na() %>% 
          not() %>% 
          sum()
        
        valueBox(
          value = value,
          subtitle = "out of 175 ESA regions reported this indicator", 
          icon = icon("fas fa-globe-africa"),
          width = 3,
          color = "red",
          href = NULL
        )
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
        
        infoBox(
          title = "Macro-average",
          value = sprintf("%.2f%%", value),
          subtitle = "i.e. unweighted mean",
          icon = icon("fas fa-calculator"),
          width = 3,
          color = "red",
          href = NULL,
          fill = T
        )
      })
      
      output$microavg <- renderInfoBox({
        req(input$indicator)
        value <- select_data() %>% 
          slot(name = "data") %>% 
          pull(IndicatorValue) %>%
          na.omit() %>%
          mean()
        
        infoBox(
          title = "Micro-average",
          value = sprintf("%.2f%%", value),
          subtitle = "i.e. weighted mean",
          icon = icon("fas fa-calculator"),
          width = 3,
          color = "red",
          href = NULL,
          fill = T
        )
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
      
    }
  )    
}
