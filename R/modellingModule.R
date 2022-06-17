# this module provides interface and functionality for the modelling tab

modellingTabUI <- function(id, config, datasets) {
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
          label = h4("Dependent Variable"),
          choices = NULL,
          multiple = F,
          selected = NULL
        ),
        
        pickerInput(
          inputId = ns("covars"),
          label = h4("Independent Variables (up to 5)"),
          choices = NULL,
          multiple = T,
          selected = NULL,
          options = pickerOptions(
            noneSelectedText = "None",
            maxOptions = 5,
            actionsBox = T
          )
        ),
        actionButton(inputId = ns("button"), label = "Run regression"),
        br(),
        withMathJax("Spatial regression is estimated with: $$y = \\rho W_{queen} y + X \\beta + \\epsilon$$"),
        hr(),
        includeHTML("./assets/html/feedback.html")
      )
    ),
    
    column(
      width = 9,
      tags$div(id = "results", style = "height:100px;"), # placeholder for waiter spin
      br(),
      verbatimTextOutput(outputId = ns("regression"))
    )
  )
}

modellingTabServer <- function(id, config, datasets) {
  moduleServer(
    id,
    function(input, output, session) {
      
      w <- Waiter$new(id = "results", html = spin_3(), color = transparent(alpha = 0))
      
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
      
      observeEvent(input$indicator, {
        choices <- datasets$df.indicators %>% 
          filter(Definition != input$indicator) %>% 
          pull(Definition) %>% 
          unique() %>% 
          sort()
        
        updatePickerInput(
          session = session,
          inputId = "covars",
          choices = choices,
          choicesOpt = list(content = str_trunc(choices, width = 150, side = "right", ellipsis = "..."))
        )
      })
      
      select_covars <- reactive({
        depvar <- datasets$indicators[[input$indicator]][1]
        covars <- datasets$indicators[input$covars] %>% unlist(use.names = F)
        
        df.lambda <- datasets$df.data %>% rename(IndicatorValue = !!depvar) %>%  
          select(c(RegionID, IndicatorValue, covars)) %>% 
          merge(datasets$df.geometry, ., by = "RegionID", duplicateGeoms = T)
        
        df.lambda <- df.lambda[complete.cases(df.lambda@data),]
        
        return(df.lambda)
      })
      
      run_regression <- eventReactive(input$button, {
        req(input$covars)
        w$show()
        to.drop <- c("RegionID", "CountryCode", "SurveyId", "SurveyType", "SurveyYear", "CountryName")
        df.lambda <- select_covars()
        df.lambda@data %<>% select(-all_of(to.drop))
        
        sf_use_s2(FALSE)
        list.queen <- poly2nb(df.lambda, queen = T)
        W <- nb2listw(list.queen, style = "W", zero.policy = T)
        sar.chi <- spatialreg::lagsarlm(IndicatorValue ~ ., data = df.lambda@data, listw = W, zero.policy = T)
        
        on.exit({
          w$hide()
        })
        
        sar.chi
      })
      
      output$regression <- renderPrint({
        sar.chi <- run_regression()
        return(summary(sar.chi))
      })
    }
  )    
}
