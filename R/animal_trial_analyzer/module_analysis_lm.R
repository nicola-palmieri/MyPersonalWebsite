# ===============================================================
# 🧮 Animal Trial Analyzer — Linear Model (LM) Module
# ===============================================================

source("R/animal_trial_analyzer/module_analysis_lm_helpers.R")

lm_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      h4("Linear Model (LM) Configuration"),
      uiOutput(ns("variable_selectors")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run Linear Model", class = "btn-primary", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download All Results", class = "btn-default", width = "100%"))
      )
    ),
    results = tagList(
      verbatimTextOutput(ns("summary")),
      br(),
      fluidRow(
        column(6, plotOutput(ns("resid_plot"))),
        column(6, plotOutput(ns("qq_plot")))
      )
    )
  )
}


lm_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selectors <- renderUI({
      req(data())
      df <- data()
      num_vars <- names(df)[sapply(df, is.numeric)]
      fac_vars <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
      
      tagList(
        selectInput(ns("dep"), "Dependent variable:", choices = num_vars),
        selectInput(ns("fixed"), "Fixed factors:", choices = fac_vars, multiple = TRUE),
        selectInput(ns("covar"), "Covariates (optional):", choices = num_vars, multiple = TRUE)
      )
    })
    
    model <- eventReactive(input$run, {
      req(data(), input$dep, input$fixed)
      formula <- make_formula(input$dep, input$fixed, input$covar)
      lm(formula, data = data())
    })
    
    output$summary <- renderPrint({
      req(model())
      summary(model())
    })
    
    output$resid_plot <- renderPlot({
      req(model())
      plot(model(), which = 1)
    })
    
    output$qq_plot <- renderPlot({
      req(model())
      plot(model(), which = 2)
    })
    
    output$download_model <- downloadHandler(
      filename = function() paste0("lm_summary_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        download_model_summary(model(), file)
      }
    )
    
    return(model)
  })
}
