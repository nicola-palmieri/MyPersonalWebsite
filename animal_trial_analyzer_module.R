# ===============================================================
# 🧪 Animal Trial Analyzer — Main Coordinator
# ===============================================================

source("R/animal_trial_analyzer/module_upload.R")
source("R/animal_trial_analyzer/module_filter.R")
source("R/animal_trial_analyzer/module_analysis.R")
source("R/animal_trial_analyzer/module_visualize.R")

animal_trial_app_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("Animal Trial Analyzer"),
    uiOutput(ns("upload_section")),
    uiOutput(ns("filter_section")),
    uiOutput(ns("analysis_section")),
    uiOutput(ns("visualize_section"))
  )
}

animal_trial_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1️⃣ Upload (always visible)
    uploaded <- upload_server("upload")
    output$upload_section <- renderUI({
      upload_ui(ns("upload"))
    })
    
    # --- 2️⃣ Filter (requires uploaded data)
    output$filter_section <- renderUI({
      req(uploaded())
      filter_ui(ns("filter"))
    })
    filtered <- reactive({
      req(uploaded())
      filter_server("filter", uploaded)
    })
    
    # --- 3️⃣ Analysis (requires uploaded data, not model yet)
    output$analysis_section <- renderUI({
      req(uploaded())
      analysis_ui(ns("analysis"))
    })
    analyzed <- analysis_server("analysis", filtered)
    
    # --- 4️⃣ Visualization (requires fitted model)
    output$visualize_section <- renderUI({
      req(analyzed())
      visualize_ui(ns("visualize"))
    })
    visualize_server("visualize", filtered, analyzed)
  })
}
