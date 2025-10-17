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

    observeEvent(input[["upload-go_filter"]], {
      updateTabsetPanel(session, "main_tabs", selected = "2️⃣ Filter")
    }, ignoreNULL = TRUE)

    observeEvent(input[["filter-back_upload"]], {
      updateTabsetPanel(session, "main_tabs", selected = "1️⃣ Upload")
    }, ignoreNULL = TRUE)

    observeEvent(input[["filter-go_analysis"]], {
      updateTabsetPanel(session, "main_tabs", selected = "3️⃣ Analyze")
    }, ignoreNULL = TRUE)

    observeEvent(input[["analysis-back_filter"]], {
      updateTabsetPanel(session, "main_tabs", selected = "2️⃣ Filter")
    }, ignoreNULL = TRUE)

    observeEvent(input[["analysis-go_visualize"]], {
      updateTabsetPanel(session, "main_tabs", selected = "4️⃣ Visualize")
    }, ignoreNULL = TRUE)

    observeEvent(input[["visualize-back_analysis"]], {
      updateTabsetPanel(session, "main_tabs", selected = "3️⃣ Analyze")
    }, ignoreNULL = TRUE)

    analysis_notified <- reactiveVal(FALSE)

    observeEvent(analyzed(), {
      req(!analysis_notified())
      showNotification(
        ui = div(
          style = "background-color: #d1e7dd; color: #0f5132; padding: 10px 16px; border-radius: 6px;",
          "✅ Analysis complete — proceed to visualization."
        ),
        duration = 5,
        type = "message"
      )
      analysis_notified(TRUE)
    }, ignoreNULL = TRUE)

    observeEvent(input[["visualize-finish"]], {
      showModal(modalDialog("🎉 All done!", easyClose = TRUE))
    }, ignoreNULL = TRUE)
  })
}
