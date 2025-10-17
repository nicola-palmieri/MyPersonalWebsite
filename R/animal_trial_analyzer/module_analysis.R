# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Coordinator
# ===============================================================
source("R/animal_trial_analyzer/module_analysis_one-way_anova.R")
source("R/animal_trial_analyzer/module_analysis_two-way_anova.R")
source("R/animal_trial_analyzer/module_analysis_utils.R")

analysis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 3 — Analyze Results"),
      p("Choose the statistical approach that fits your trial design, then inspect the summaries on the right."),
      hr(),
      selectInput(
        ns("analysis_type"),
        "Select analysis type:",
        choices = c("One-way ANOVA", "Two-way ANOVA"),
        selected = "One-way ANOVA"
      ),
      hr(),
      uiOutput(ns("config_panel")),
      hr(),
      div(
        class = "d-flex justify-content-between gap-2",
        actionButton(ns("back_filter"), "← Back"),
        actionButton(ns("go_visualize"), "Continue →", class = "btn-primary")
      )
    ),
    mainPanel(
      width = 8,
      h4("Analysis Results"),
      uiOutput(ns("results_panel"))
    )
  )
}

analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    current_module_ui <- reactive({
      req(input$analysis_type)
      if (input$analysis_type == "One-way ANOVA") {
        one_way_anova_ui(ns("anova_one"))
      } else if (input$analysis_type == "Two-way ANOVA") {
        two_way_anova_ui(ns("anova_two"))
      } else {
        list(
          config = p("Analysis type not implemented yet."),
          results = p("Analysis type not implemented yet.")
        )
      }
    })

    output$config_panel <- renderUI({
      ui <- current_module_ui()
      req(ui$config)
      ui$config
    })

    output$results_panel <- renderUI({
      ui <- current_module_ui()
      req(ui$results)
      ui$results
    })
    
    
    # Dynamically run submodule
    model_fit <- reactiveVal(NULL)

    observeEvent(input$analysis_type, {
      req(input$analysis_type)
      if (input$analysis_type == "One-way ANOVA") {
        model_fit(one_way_anova_server("anova_one", df))
      } else if (input$analysis_type == "Two-way ANOVA") {
        model_fit(two_way_anova_server("anova_two", df))
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
