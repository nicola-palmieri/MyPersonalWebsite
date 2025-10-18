# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Coordinator (fixed + cleaned)
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
      uiOutput(ns("config_panel"))
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
    df <- reactive(filtered_data())
    
    # --- Mapping between analysis type and submodules ---
    submodules <- list(
      "One-way ANOVA" = list(
        id = "anova_one",
        ui = one_way_anova_ui,
        server = one_way_anova_server
      ),
      "Two-way ANOVA" = list(
        id = "anova_two",
        ui = two_way_anova_ui,
        server = two_way_anova_server
      )
    )
    
    # --- Render selected submodule UI dynamically ---
    current_module_ui <- reactive({
      req(input$analysis_type)
      mod <- submodules[[input$analysis_type]]
      req(mod)
      mod$ui(ns(mod$id))
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
    
    # --- Dynamically call selected server ---
    model_fit <- reactiveVal(NULL)
    
    observeEvent(input$analysis_type, {
      mod <- submodules[[input$analysis_type]]
      if (!is.null(mod)) {
        model_fit(mod$server(mod$id, df))
      } else {
        model_fit(NULL)
      }
    }, ignoreNULL = FALSE)
    
    # --- Expose final fitted model ---
    reactive({
      model_reactive <- model_fit()
      req(model_reactive)
      model <- model_reactive()
      req(model)
      model
    })
  })
}
