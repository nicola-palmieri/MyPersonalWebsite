# ===============================================================
# 🧪 Animal Trial Analyzer — main module (coordinator)
# ===============================================================

source("R/animal_trial_analyzer/module_upload.R")
#source("R/animal_trial_analyzer/module_filter.R")
#source("R/animal_trial_analyzer/module_analysis.R")
#source("R/animal_trial_analyzer/module_visualize.R")

animal_trial_app_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    upload_ui(ns("upload"))
    # filter_ui(ns("filter")),
    # analysis_ui(ns("analysis")),
    # visualize_ui(ns("visualize"))
  )
}

animal_trial_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # 🔥 Correct call: wrap the submodule ID in ns()
    uploaded <- upload_server(session$ns("upload"))
    # filtered <- filter_server(session$ns("filter"), uploaded)
    # analyzed <- analysis_server(session$ns("analysis"), filtered)
    # visualize_server(session$ns("visualize"), analyzed)
  })
}
