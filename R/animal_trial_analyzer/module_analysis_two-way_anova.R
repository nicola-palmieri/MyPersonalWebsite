# ===============================================================
# 🧪 Animal Trial Analyzer — Two-way ANOVA Module
# ===============================================================

library(shiny)
library(DT)
library(broom)
library(officer)
library(flextable)

two_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order_1")),
      uiOutput(ns("level_order_2")),
      br(),
      actionButton(ns("run"), "Run Two-way ANOVA")
    ),
    results = tagList(
      uiOutput(ns("summary_ui")),
      br(),
      uiOutput(ns("fixed_effects_ui"))
    )
  )
}

two_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # ----------------------------------------------
    # Dynamic input selectors for response and factors
    # ----------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        # checkbox always present
        checkboxInput(ns("multi_resp"), "Enable multiple response variables", value = FALSE),
        
        # placeholder for the response selector that updates reactively
        uiOutput(ns("response_selector")),
        
        selectInput(
          ns("factor1"),
          "First factor (x-axis):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        ),
        selectInput(
          ns("factor2"),
          "Second factor (line color):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 1) cat_cols[2] else NULL
        )
      )
    })
    
    # --- reactive response selector ---
    output$response_selector <- renderUI({
      req(df())
      data <- df()
      num_cols <- names(data)[sapply(data, is.numeric)]
      
      if (isTRUE(input$multi_resp)) {
        selectizeInput(
          ns("response"),
          "Response variables (numeric):",
          choices = num_cols,
          selected = head(num_cols, 1),
          multiple = TRUE,
          options = list(maxItems = 10)
        )
      } else {
        selectInput(
          ns("response"),
          "Response variable (numeric):",
          choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL
        )
      }
    })
    
    
    # ----------------------------------------------
    # Level order selectors for both factors
    # ----------------------------------------------
    output$level_order_1 <- renderUI({
      req(df(), input$factor1)
      levels1 <- unique(as.character(df()[[input$factor1]]))
      selectInput(
        ns("order1"),
        paste("Order of levels for", input$factor1, "(x-axis):"),
        choices = levels1,
        selected = levels1,
        multiple = TRUE
      )
    })
    
    output$level_order_2 <- renderUI({
      req(df(), input$factor2)
      levels2 <- unique(as.character(df()[[input$factor2]]))
      selectInput(
        ns("order2"),
        paste("Order of levels for", input$factor2, "(lines):"),
        choices = levels2,
        selected = levels2,
        multiple = TRUE
      )
    })
    
    # ----------------------------------------------
    # Fit two-way ANOVA model (fixed effects output)
    # ----------------------------------------------
    models <- eventReactive(input$run, {
      req(df(), input$response, input$factor1, input$factor2, input$order1, input$order2)
      responses <- input$response
      if (!isTRUE(input$multi_resp)) {
        responses <- responses[1]
      }
      responses <- unique(responses)
      req(length(responses) > 0)

      if (length(responses) > 10) {
        validate(need(FALSE, "Please select at most 10 response variables."))
      }

      data <- df()
      data[[input$factor1]] <- factor(data[[input$factor1]], levels = input$order1)
      data[[input$factor2]] <- factor(data[[input$factor2]], levels = input$order2)

      model_list <- list()
      for (resp in responses) {
        model_formula <- as.formula(paste(resp, "~", input$factor1, "*", input$factor2))
        model_list[[resp]] <- lm(model_formula, data = data)
      }

      list(
        models = model_list,
        responses = responses,
        factors = list(factor1 = input$factor1, factor2 = input$factor2),
        orders = list(order1 = input$order1, order2 = input$order2)
      )
    })

    output$summary_ui <- renderUI({
      model_info <- models()
      if (is.null(model_info)) {
        return(NULL)
      }

      responses <- model_info$responses

      tabs <- lapply(seq_along(responses), function(i) {
        tabPanel(
          title = responses[i],
          tags$div(
            tags$details(
              open = FALSE,
              tags$summary(strong("R Output")),
              verbatimTextOutput(ns(paste0("summary_", i)))
            ),
            br(),
            h4("Coefficient Table"),
            DTOutput(ns(paste0("fixed_effects_", i))),
            br(),
            h4("Download Results"),
            downloadButton(ns(paste0("download_", i)), "Download Results (Word)")
          )
        )
      })

      do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs))
    })

    output$fixed_effects_ui <- renderUI({
      NULL
    })

    observeEvent(models(), {
      model_info <- models()
      if (is.null(model_info)) {
        return()
      }

      responses <- model_info$responses
      model_list <- model_info$models

      for (i in seq_along(responses)) {
        resp <- responses[i]
        local({
          idx <- i
          response_name <- resp
          model_obj <- model_list[[response_name]]

          tidy_df <- broom::tidy(model_obj)
          numeric_cols <- vapply(tidy_df, is.numeric, logical(1))
          tidy_df[numeric_cols] <- lapply(tidy_df[numeric_cols], function(x) round(x, 4))

          output[[paste0("summary_", idx)]] <- renderPrint({
            summary(model_obj)
          })

          output[[paste0("fixed_effects_", idx)]] <- renderDT({
            datatable(
              tidy_df,
              options = list(scrollX = TRUE, pageLength = 5),
              rownames = FALSE
            )
          })

          output[[paste0("download_", idx)]] <- downloadHandler(
            filename = function() {
              safe_resp <- gsub("[^A-Za-z0-9]+", "_", response_name)
              safe_resp <- gsub("_+", "_", safe_resp)
              safe_resp <- gsub("^_|_$", "", safe_resp)
              if (!nzchar(safe_resp)) {
                safe_resp <- paste0("response_", idx)
              }
              paste0("anova_results_", safe_resp, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
              tidy_export <- broom::tidy(model_obj)
              numeric_cols_export <- vapply(tidy_export, is.numeric, logical(1))
              tidy_export[numeric_cols_export] <- lapply(tidy_export[numeric_cols_export], function(x) round(x, 4))

              doc <- officer::read_docx()
              doc <- officer::body_add_par(doc, paste("ANOVA results for:", response_name), style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Model formula:", format(formula(model_obj))), style = "heading 2")
              doc <- officer::body_add_par(doc, "R Output", style = "heading 2")
              summary_lines <- capture.output(summary(model_obj))
              for (line in summary_lines) {
                doc <- officer::body_add_par(doc, line, style = "Normal")
              }
              doc <- officer::body_add_par(doc, "Coefficient Table", style = "heading 2")
              ft <- flextable::flextable(tidy_export)
              ft <- flextable::autofit(ft)
              doc <- officer::body_add_flextable(doc, ft)

              officer::print(doc, target = file)
            }
          )
        })
      }
    })

    return(models)
  })
}
