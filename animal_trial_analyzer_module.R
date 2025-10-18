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
    tabsetPanel(
      id = ns("main_tabs"),
      tabPanel("1️⃣ Upload", upload_ui(ns("upload"))),
      tabPanel("2️⃣ Filter", filter_ui(ns("filter"))),
      tabPanel("3️⃣ Analyze", analysis_ui(ns("analysis"))),
      tabPanel("4️⃣ Visualize", visualize_ui(ns("visualize")))
    )
  )
}

animal_trial_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    uploaded  <- upload_server("upload")
    filtered  <- filter_server("filter", uploaded)
    analyzed  <- analysis_server("analysis", filtered)
    visualize_server("visualize", filtered, analyzed)

  })
}
