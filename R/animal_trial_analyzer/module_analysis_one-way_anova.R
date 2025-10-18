# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA
# ===============================================================
one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      uiOutput(ns("advanced_options")),
      br(),
      actionButton(ns("run"), "Run One-way ANOVA")
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

    output$advanced_options <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      choices <- c("None", setdiff(unique(cat_cols), "None"))

      tags$details(
        tags$summary(strong("Advanced options")),
        selectInput(
          ns("stratify_var"),
          "Stratify analysis by:",
          choices = choices,
          selected = "None"
        ),
        uiOutput(ns("strata_order_ui"))
      )
    })

    output$strata_order_ui <- renderUI({
      req(df())
      strat_var <- input$stratify_var

      if (is.null(strat_var) || identical(strat_var, "None")) {
        return(NULL)
      }

      data <- df()
      values <- data[[strat_var]]
      if (is.null(values)) {
        return(NULL)
      }

      if (is.factor(values)) {
        strata_levels <- levels(values)
      } else {
        values <- values[!is.na(values)]
        strata_levels <- unique(as.character(values))
      }

      if (length(strata_levels) == 0) {
        return(NULL)
      }

      selectInput(
        ns("strata_order"),
        paste("Order of levels for", strat_var, "(strata):"),
        choices = strata_levels,
        selected = strata_levels,
        multiple = TRUE
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
      req(!is.null(input$stratify_var))
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

      strat_var <- input$stratify_var

      if (!is.null(strat_var) && !identical(strat_var, "None")) {
        if (!is.null(input$strata_order) && length(input$strata_order) > 0) {
          data[[strat_var]] <- factor(data[[strat_var]], levels = input$strata_order)
        } else {
          data[[strat_var]] <- factor(data[[strat_var]])
        }
      }

      if (is.null(strat_var) || identical(strat_var, "None")) {
        model_list <- list()
        for (resp in responses) {
          model_formula <- as.formula(paste(resp, "~", input$group))
          model_list[[resp]] <- lm(model_formula, data = data)
        }

        return(list(
          models = model_list,
          responses = responses,
          strata = NULL,
          factors = list(factor1 = input$group, factor2 = NULL),
          orders = list(order1 = input$order, order2 = NULL)
        ))
      }

      strata_counts <- table(data[[strat_var]])
      strata <- names(strata_counts)[strata_counts > 0]
      if (length(strata) == 0) {
        validate(need(FALSE, "No valid strata found for the selected variable."))
      }

      if (length(strata) > 10) {
        validate(need(FALSE, "Stratified analysis supports up to 10 strata."))
      }

      model_list <- list()
      for (stratum in strata) {
        subset_data <- data[data[[strat_var]] == stratum, , drop = FALSE]
        subset_data[[input$group]] <- factor(subset_data[[input$group]], levels = input$order)

        model_list[[stratum]] <- list()
        for (resp in responses) {
          model_formula <- as.formula(paste(resp, "~", input$group))
          model_list[[stratum]][[resp]] <- lm(model_formula, data = subset_data)
        }
      }

      list(
        models = model_list,
        responses = responses,
        strata = list(var = strat_var, levels = strata),
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
      strata_info <- model_info$strata

      if (is.null(strata_info)) {
        tabs <- lapply(seq_along(responses), function(i) {
          tabPanel(
            title = responses[i],
            tags$div(
              verbatimTextOutput(ns(paste0("summary_", i))),
              br(),
              h4("Coefficient Table"),
              DTOutput(ns(paste0("fixed_effects_", i))),
              br(),
              h4("Download Results"),
              downloadButton(ns(paste0("download_", i)), "Download Results (Word)")
            )
          )
        })

        return(do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs)))
      }

      strata_levels <- strata_info$levels
      tabs <- lapply(seq_along(responses), function(i) {
        response_name <- responses[i]
        stratum_tabs <- lapply(seq_along(strata_levels), function(j) {
          stratum_name <- strata_levels[j]
          tabPanel(
            title = stratum_name,
            tags$div(
              tags$details(
                open = FALSE,
                tags$summary(strong("R Output")),
                verbatimTextOutput(ns(paste0("summary_", i, "_", j)))
              ),
              br(),
              h4("Coefficient Table"),
              DTOutput(ns(paste0("fixed_effects_", i, "_", j))),
              br(),
              h4("Download Results"),
              downloadButton(
                ns(paste0("download_", i, "_", j)),
                "Download Results (Word)"
              )
            )
          )
        })

        tabPanel(
          title = response_name,
          do.call(tabsetPanel, c(list(id = ns(paste0("strata_tabs_", i))), stratum_tabs))
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
      strata_info <- model_info$strata

      if (is.null(strata_info)) {
        for (i in seq_along(responses)) {
          resp <- responses[i]
          local({
            idx <- i
            response_name <- resp
            model_obj <- model_list[[response_name]]

            tidy_df <- broom::tidy(model_obj)
            numeric_cols <- vapply(tidy_df, is.numeric, logical(1))
            tidy_df[numeric_cols] <- lapply(tidy_df[numeric_cols], function(x) round(x, 4))

            factor_names <- unlist(model_info$factors, use.names = FALSE)

            output[[paste0("summary_", idx)]] <- renderPrint({
              results <- prepare_anova_outputs(model_obj, factor_names)
              print(results$anova_object)

              if (length(results$posthoc_details) == 0) {
                cat("\nNo post-hoc Tukey comparisons were generated.\n")
              } else {
                for (factor_nm in names(results$posthoc_details)) {
                  details <- results$posthoc_details[[factor_nm]]
                  if (!is.null(details$error)) {
                    cat(
                      "\nPost-hoc Tukey comparisons for",
                      factor_nm,
                      "could not be computed:",
                      details$error,
                      "\n"
                    )
                  } else if (!is.null(details$table)) {
                    cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
                    tbl <- details$table
                    if ("Factor" %in% names(tbl)) {
                      tbl$Factor <- NULL
                    }
                    print(tbl)
                  }
                }
              }
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
                results <- prepare_anova_outputs(model_obj, factor_names)
                write_anova_docx(file, results, model_obj, response_name)
              }
            )
          })
        }
        return()
      }

      strata_levels <- strata_info$levels
      for (i in seq_along(responses)) {
        resp <- responses[i]
        for (j in seq_along(strata_levels)) {
          stratum_name <- strata_levels[j]
          local({
            idx <- i
            stratum_idx <- j
            response_name <- resp
            stratum_label <- stratum_name
            model_obj <- model_list[[stratum_label]][[response_name]]

            tidy_df <- broom::tidy(model_obj)
            numeric_cols <- vapply(tidy_df, is.numeric, logical(1))
            tidy_df[numeric_cols] <- lapply(tidy_df[numeric_cols], function(x) round(x, 4))

            factor_names <- unlist(model_info$factors, use.names = FALSE)

            output[[paste0("summary_", idx, "_", stratum_idx)]] <- renderPrint({
              results <- prepare_anova_outputs(model_obj, factor_names)
              print(results$anova_object)

              if (length(results$posthoc_details) == 0) {
                cat("\nNo post-hoc Tukey comparisons were generated.\n")
              } else {
                for (factor_nm in names(results$posthoc_details)) {
                  details <- results$posthoc_details[[factor_nm]]
                  if (!is.null(details$error)) {
                    cat(
                      "\nPost-hoc Tukey comparisons for",
                      factor_nm,
                      "could not be computed:",
                      details$error,
                      "\n"
                    )
                  } else if (!is.null(details$table)) {
                    cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
                    tbl <- details$table
                    if ("Factor" %in% names(tbl)) {
                      tbl$Factor <- NULL
                    }
                    print(tbl)
                  }
                }
              }
            })

            output[[paste0("fixed_effects_", idx, "_", stratum_idx)]] <- renderDT({
              datatable(
                tidy_df,
                options = list(scrollX = TRUE, pageLength = 5),
                rownames = FALSE
              )
            })

            output[[paste0("download_", idx, "_", stratum_idx)]] <- downloadHandler(
              filename = function() {
                safe_resp <- gsub("[^A-Za-z0-9]+", "_", response_name)
                safe_resp <- gsub("_+", "_", safe_resp)
                safe_resp <- gsub("^_|_$", "", safe_resp)
                if (!nzchar(safe_resp)) {
                  safe_resp <- paste0("response_", idx)
                }

                safe_stratum <- gsub("[^A-Za-z0-9]+", "_", stratum_label)
                safe_stratum <- gsub("_+", "_", safe_stratum)
                safe_stratum <- gsub("^_|_$", "", safe_stratum)
                if (!nzchar(safe_stratum)) {
                  safe_stratum <- paste0("stratum_", stratum_idx)
                }

                paste0(
                  "anova_results_",
                  safe_resp,
                  "_stratum_",
                  safe_stratum,
                  "_",
                  Sys.Date(),
                  ".docx"
                )
              },
              content = function(file) {
                results <- prepare_anova_outputs(model_obj, factor_names)
                write_anova_docx(file, results, model_obj, response_name, stratum_label)
              }
            )
          })
        }
      }
    })

    return(models)
  })
}
