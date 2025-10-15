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
    titlePanel("🧪 Animal Trial Analyzer"),
    tabsetPanel(
      id = ns("main_tabs"),
      tabPanel(
        title = "1️⃣ Upload",
        div(
          class = "pt-3",
          h3("Step 1: Upload Data"),
          upload_ui(ns("upload")),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 16px;",
            div(style = "width: 120px;"),
            actionButton(ns("go_filter"), "Continue →", class = "btn-primary")
          )
        )
      ),
      tabPanel(
        title = "2️⃣ Filter",
        div(
          class = "pt-3",
          h3("Step 2: Filter & Prepare"),
          filter_ui(ns("filter")),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 16px;",
            actionButton(ns("back_upload"), "← Back"),
            actionButton(ns("go_analysis"), "Continue →", class = "btn-primary")
          )
        )
      ),
      tabPanel(
        title = "3️⃣ Analyze",
        div(
          class = "pt-3",
          h3("Step 3: Analyze Results"),
          analysis_ui(ns("analysis")),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 16px;",
            actionButton(ns("back_filter"), "← Back"),
            actionButton(ns("go_visualize"), "Continue →", class = "btn-primary")
          )
        )
      ),
      tabPanel(
        title = "4️⃣ Visualize",
        div(
          class = "pt-3",
          h3("Step 4: Visualize & Share"),
          visualize_ui(ns("visualize")),
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-top: 16px; gap: 8px;",
            actionButton(ns("back_analysis"), "← Back"),
            div(
              style = "display: flex; gap: 8px;",
              div(style = "width: 120px;"),
              actionButton(ns("finish"), "Finish", class = "btn-success")
            )
          )
        )
      )
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

    observeEvent(input$go_filter, {
      updateTabsetPanel(session, "main_tabs", selected = "2️⃣ Filter")
    })

    observeEvent(input$back_upload, {
      updateTabsetPanel(session, "main_tabs", selected = "1️⃣ Upload")
    })

    observeEvent(input$go_analysis, {
      updateTabsetPanel(session, "main_tabs", selected = "3️⃣ Analyze")
    })

    observeEvent(input$back_filter, {
      updateTabsetPanel(session, "main_tabs", selected = "2️⃣ Filter")
    })

    observeEvent(input$go_visualize, {
      updateTabsetPanel(session, "main_tabs", selected = "4️⃣ Visualize")
    })

    observeEvent(input$back_analysis, {
      updateTabsetPanel(session, "main_tabs", selected = "3️⃣ Analyze")
    })

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

    observeEvent(input$finish, {
      showModal(modalDialog("🎉 All done!", easyClose = TRUE))
    })
  })
}
