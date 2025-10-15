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
      tabPanel(
        title = "1️⃣ Upload",
        {
          upload_components <- upload_ui(ns("upload"))
          sidebarLayout(
            sidebarPanel(
              width = 4,
              h4("Step 1 — Upload Data"),
              p("Upload your Excel file in long format, choose the worksheet, and follow the guidance before proceeding."),
              hr(),
              upload_components[[2]],
              upload_components[[3]],
              hr(),
              div(
                class = "d-flex justify-content-end",
                actionButton(ns("go_filter"), "Continue →", class = "btn-primary")
              )
            ),
            mainPanel(
              width = 8,
              h4("Data Preview"),
              upload_components[[5]],
              hr(),
              upload_components[[6]]
            )
          )
        }
      ),
      tabPanel(
        title = "2️⃣ Filter",
        {
          filter_components <- filter_ui(ns("filter"))
          sidebarLayout(
            sidebarPanel(
              width = 4,
              h4("Step 2 — Filter Records"),
              p("Pick the columns to focus on and adjust the filters to refine the dataset for analysis."),
              hr(),
              filter_components[[2]],
              hr(),
              filter_components[[3]],
              hr(),
              div(
                class = "d-flex justify-content-between gap-2",
                actionButton(ns("back_upload"), "← Back"),
                actionButton(ns("go_analysis"), "Continue →", class = "btn-primary")
              )
            ),
            mainPanel(
              width = 8,
              h4("Filtered Data Preview"),
              filter_components[[5]]
            )
          )
        }
      ),
      tabPanel(
        title = "3️⃣ Analyze",
        {
          analysis_components <- analysis_ui(ns("analysis"))
          tagList(
            sidebarLayout(
              sidebarPanel(
                width = 4,
                h4("Step 3 — Analyze Results"),
                p("Choose the statistical approach that fits your trial design, configure the model, then review the outputs to the right."),
                hr(),
                h5("Analysis Configuration"),
                analysis_components[[2]],
                hr(),
                div(
                  id = ns("analysis_controls_placeholder"),
                  class = "analysis-controls"
                ),
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
                p(
                  class = "text-muted",
                  "Run the ANOVA to populate the summaries and fixed effects tables."
                ),
                div(
                  id = ns("analysis_results_placeholder"),
                  class = "analysis-results"
                )
              )
            ),
            div(
              id = ns("analysis_module_container"),
              class = "visually-hidden",
              analysis_components[[3]]
            ),
            tags$style(
              HTML(
                paste0(
                  "#", ns("analysis_module_container"), " { position: absolute; left: -9999px; width: 1px; height: 1px; overflow: hidden; }",
                  "#", ns("analysis_controls_placeholder"), " > * { margin-bottom: 12px; }",
                  "#", ns("analysis_controls_placeholder"), " > *:last-child { margin-bottom: 0; }",
                  "#", ns("analysis_controls_placeholder"), " .btn { width: 100%; }",
                  "#", ns("analysis_results_placeholder"), " > * { margin-bottom: 16px; }",
                  "#", ns("analysis_results_placeholder"), " > *:last-child { margin-bottom: 0; }"
                )
              )
            ),
            tags$script(
              HTML(
                sprintf(
                  "(function(){\n  var containerId = '%s';\n  var controlsId = '%s';\n  var resultsId = '%s';\n  function moveElements(){\n    var container = document.getElementById(containerId);\n    var controls = document.getElementById(controlsId);\n    var results = document.getElementById(resultsId);\n    if(!container || !controls || !results){ return; }\n    var inner = container.firstElementChild;\n    if(!inner){ return; }\n    var children = Array.from(inner.children);\n    if(!children.length){ return; }\n    children.forEach(function(el){\n      if(!(el instanceof HTMLElement)){ return; }\n      var id = el.id || '';\n      if(id){\n        var existing = document.getElementById(id);\n        if(existing && existing !== el && existing.parentElement){\n          existing.parentElement.removeChild(existing);\n        }\n      }\n      if(/(inputs|level_order|level_order_1|level_order_2)$/.test(id)){\n        if(el.parentElement !== controls){ controls.appendChild(el); }\n        return;\n      }\n      if(/-run$/.test(id)){\n        if(el.parentElement !== controls){ controls.appendChild(el); }\n        return;\n      }\n      if(/(summary_ui|fixed_effects_ui)$/.test(id)){\n        if(el.parentElement !== results){ results.appendChild(el); }\n        return;\n      }\n    });\n    Array.from(inner.querySelectorAll('br')).forEach(function(br){ br.remove(); });\n  }\n  function setupObserver(){\n    var container = document.getElementById(containerId);\n    if(!container){\n      setTimeout(setupObserver, 100);\n      return;\n    }\n    moveElements();\n    var observer = new MutationObserver(function(){ moveElements(); });\n    observer.observe(container, { childList: true, subtree: true });\n  }\n  if(document.readyState === 'loading'){\n    document.addEventListener('DOMContentLoaded', setupObserver);\n  } else {\n    setupObserver();\n  }\n  document.addEventListener('shiny:value', function(event){\n    var id = (event && event.target && event.target.id) || '';\n    if(id.endsWith('analysis_panel') || id.endsWith('analysis_type')){\n      setTimeout(moveElements, 75);\n    }\n  });\n  document.addEventListener('shiny:inputchanged', function(event){\n    if(event && event.name && event.name.endsWith('analysis_type')){\n      setTimeout(moveElements, 75);\n    }\n  });\n})();",
                  ns("analysis_module_container"),
                  ns("analysis_controls_placeholder"),
                  ns("analysis_results_placeholder")
                )
              )
            )
          )
        }
      ),

      tabPanel(
        title = "4️⃣ Visualize",
        {
          visualize_components <- visualize_ui(ns("visualize"))
          sidebarLayout(
            sidebarPanel(
              width = 4,
              h4("Step 4 — Visualize Outcomes"),
              p("Tweak the layout of the mean ± SE plots, download the figure, and wrap up your workflow."),
              hr(),
              visualize_components[[2]],
              hr(),
              visualize_components[[3]],
              hr(),
              visualize_components[[4]],
              hr(),
              div(
                class = "d-flex justify-content-between gap-2",
                actionButton(ns("back_analysis"), "← Back"),
                actionButton(ns("finish"), "Finish", class = "btn-success")
              )
            ),
            mainPanel(
              width = 8,
              h4("Mean ± SE Plot"),
              visualize_components[[5]]
            )
          )
        }
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
