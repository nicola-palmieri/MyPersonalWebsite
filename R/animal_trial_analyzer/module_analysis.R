# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Coordinator
# ===============================================================
library(shiny)

source("R/animal_trial_analyzer/module_analysis_anova.R")
source("R/animal_trial_analyzer/module_analysis_utils.R")

analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("3. Statistical Analysis"),
    selectInput(
      ns("analysis_type"),
      "Select analysis type:",
      choices = c("One-way ANOVA"),  # more will be added here later
      selected = "One-way ANOVA"
    ),
    uiOutput(ns("analysis_panel"))
  )
}

analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # Dynamically show the correct submodule based on selection
    output$analysis_panel <- renderUI({
      req(input$analysis_type)
      if (input$analysis_type == "One-way ANOVA") {
        one_way_anova_ui(ns("anova"))
      } else {
        p("Analysis type not implemented yet.")
      }
    })
    
    # Dynamically run submodule
    model_fit <- reactiveVal(NULL)

    observeEvent(input$analysis_type, {
      req(input$analysis_type)

      if (input$analysis_type == "One-way ANOVA") {
        model_fit(one_way_anova_server("anova", df))
      } else {
        model_fit(NULL)
      }
    }, ignoreNULL = FALSE)

    # Expose a reactive that yields the fitted model object once available
    reactive({
      model_reactive <- model_fit()
      req(model_reactive)
      model <- model_reactive()
      req(model)
      model
    })
  })
}
