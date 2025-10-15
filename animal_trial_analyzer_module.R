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
                div(
                  id = ns("analysis_outputs_container"),
                  class = "analysis-results",
                  analysis_components[[3]]
                )
              )
            ),
            tags$style(
              HTML(
                paste0(
                  "#", ns("analysis_controls_placeholder"), " > * { margin-bottom: 12px; }",
                  "#", ns("analysis_controls_placeholder"), " > *:last-child { margin-bottom: 0; }"
                )
              )
            ),
            tags$script(
              HTML(
                sprintf(
                  "(function(){\n                     var controlsId = '%s';\n                     var outputsId = '%s';\n                     function relocateAnalysisElements(){\n                       var $controls = $('#'+controlsId);\n                       var $outputs = $('#'+outputsId);\n                       if(!$controls.length || !$outputs.length){ return; }\n                       var baseId = outputsId.replace(/-analysis_outputs_container$/, '');\n                       var knownControlIds = [\n                         baseId + '-anova_one-inputs',\n                         baseId + '-anova_one-level_order',\n                         baseId + '-anova_one-run',\n                         baseId + '-anova_two-inputs',\n                         baseId + '-anova_two-level_order_1',\n                         baseId + '-anova_two-level_order_2',\n                         baseId + '-anova_two-run'\n                       ];\n                       var escapeSelector = function(id){\n                         return id.replace(/([:\\.[\\],=:@])/g, '\\\\$1');\n                       };\n                       knownControlIds.forEach(function(id){\n                         var selector = '#' + escapeSelector(id);\n                         var $existing = $(selector);\n                         if($existing.length && $existing.closest('#'+controlsId).length){\n                           $existing.remove();\n                         }\n                       });\n                       var controlPattern = /(inputs|level_order|level_order_1|level_order_2)$/;\n                       $outputs.find('[id]').filter(function(){\n                         var id = this.id || '';\n                         return controlPattern.test(id);\n                       }).each(function(){\n                         var $el = $(this);\n                         if(!$el.closest('#'+controlsId).length){\n                           $el.appendTo($controls);\n                         }\n                       });\n                       $outputs.find('[id]').filter(function(){\n                         var id = this.id || '';\n                         return /-run$/.test(id);\n                       }).each(function(){\n                         var $el = $(this);\n                         if(!$el.closest('#'+controlsId).length){\n                           $el.appendTo($controls);\n                         }\n                       });\n                       $controls.find('br').remove();\n                       $outputs.children('br').slice(0, 2).remove();\n                     }\n                     function scheduleRelocate(){ setTimeout(relocateAnalysisElements, 75); }\n                     $(document).on('shiny:value', function(event){\n                       var id = event.target && event.target.id ? event.target.id : '';\n                       if(id.endsWith('analysis_panel') || id.endsWith('analysis_type')){\n                         scheduleRelocate();\n                       }\n                     });\n                     $(document).on('shiny:inputchanged', function(event){\n                       if((event.name || '').endsWith('analysis_type')){\n                         scheduleRelocate();\n                       }\n                     });\n                     $(document).on('shiny:sessioninitialized', function(){ scheduleRelocate(); });\n                     $(document).ready(function(){ scheduleRelocate(); });\n                   })();",
                  ns("analysis_controls_placeholder"),
                  ns("analysis_outputs_container")
                )
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
