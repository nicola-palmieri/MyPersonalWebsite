# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA
# ===============================================================
one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      br(),
      actionButton(ns("run"), "Run ANOVA")
    ),
    results = tagList(
      uiOutput(ns("summary_ui")),
      br(),
      uiOutput(ns("fixed_effects_ui"))
    )
  )
}

one_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # Dynamic selectors
    output$inputs <- renderUI({
      req(df())
      data <- df()
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        # --- checkbox always exists ---
        checkboxInput(ns("multi_resp"), "Enable multiple response variables", value = FALSE),
        
        # --- placeholder for response selector ---
        uiOutput(ns("response_selector")),
        
        selectInput(
          ns("group"),
          "Grouping variable (factor):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    # --- render response selector reactively ---
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
    
    
    # Level order selection
    output$level_order <- renderUI({
      req(df(), input$group)
      levels <- unique(as.character(df()[[input$group]]))
      selectInput(
        ns("order"),
        "Order of levels (first = reference):",
        choices = levels,
        selected = levels,
        multiple = TRUE
      )
    })
    
    # Fit model(s)
    models <- eventReactive(input$run, {
      req(df(), input$response, input$group, input$order)
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
      data[[input$group]] <- factor(data[[input$group]], levels = input$order)

      model_list <- list()
      for (resp in responses) {
        model_formula <- as.formula(paste(resp, "~", input$group))
        model_list[[resp]] <- lm(model_formula, data = data)
      }

      list(
        models = model_list,
        responses = responses,
        factors = list(factor1 = input$group, factor2 = NULL),
        orders = list(order1 = input$order, order2 = NULL)
      )
    })

    # Summary outputs
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
              doc <- flextable::body_add_flextable(doc, ft)

              print(doc, target = file)
            }
          )
        })
      }
    })

    return(models)
  })
}
