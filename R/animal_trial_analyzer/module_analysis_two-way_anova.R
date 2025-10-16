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
          ns("subgroup_var"),
          "Subgroup variable (optional):",
          choices = c("None" = "", cat_cols),
          selected = ""
        ),

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
    overview_data <- reactiveVal(NULL)

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

      subgroup_var <- input$subgroup_var
      if (!length(subgroup_var) || !nzchar(subgroup_var)) {
        subgroup_var <- NULL
      }

      subgroup_levels <- NULL
      if (!is.null(subgroup_var)) {
        subgroup_data <- stats::na.omit(data[, subgroup_var, drop = FALSE])
        subgroup_levels <- unique(as.character(subgroup_data[[subgroup_var]]))
        if (length(subgroup_levels) > 10) {
          validate(need(FALSE, "Please select a subgroup variable with at most 10 levels."))
        }
      }

      model_list <- list()
      overview_rows <- list()

      for (resp in responses) {
        model_formula <- as.formula(paste(resp, "~", input$factor1, "*", input$factor2))

        if (is.null(subgroup_var)) {
          model_obj <- lm(model_formula, data = data)
          model_list[[resp]] <- list("Overall" = model_obj)

          glance_row <- tryCatch({
            broom::glance(model_obj)
          }, error = function(e) NULL)

          if (!is.null(glance_row)) {
            overview_rows[[length(overview_rows) + 1]] <- data.frame(
              Response = resp,
              Subgroup = "All data",
              DF = round(glance_row$df, 3),
              `F value` = round(glance_row$statistic, 3),
              `p value` = signif(glance_row$p.value, 3),
              Significance = ifelse(
                is.na(glance_row$p.value),
                "",
                ifelse(glance_row$p.value < 0.001, "***",
                       ifelse(glance_row$p.value < 0.01, "**",
                              ifelse(glance_row$p.value < 0.05, "*", "ns")))
              ),
              stringsAsFactors = FALSE
            )
          }
        } else {
          subgroup_models <- list()

          for (lvl in subgroup_levels) {
            subset_idx <- which(!is.na(data[[subgroup_var]]) & as.character(data[[subgroup_var]]) == lvl)
            if (length(subset_idx) == 0) {
              next
            }

            subset_data <- data[subset_idx, , drop = FALSE]
            subset_data <- subset_data[!is.na(subset_data[[resp]]), , drop = FALSE]
            if (nrow(subset_data) == 0) {
              next
            }

            model_obj <- tryCatch({
              lm(model_formula, data = subset_data)
            }, error = function(e) NULL)

            if (!is.null(model_obj)) {
              subgroup_models[[lvl]] <- model_obj

              glance_row <- tryCatch({
                broom::glance(model_obj)
              }, error = function(e) NULL)

              if (!is.null(glance_row)) {
                overview_rows[[length(overview_rows) + 1]] <- data.frame(
                  Response = resp,
                  Subgroup = lvl,
                  DF = round(glance_row$df, 3),
                  `F value` = round(glance_row$statistic, 3),
                  `p value` = signif(glance_row$p.value, 3),
                  Significance = ifelse(
                    is.na(glance_row$p.value),
                    "",
                    ifelse(glance_row$p.value < 0.001, "***",
                           ifelse(glance_row$p.value < 0.01, "**",
                                  ifelse(glance_row$p.value < 0.05, "*", "ns")))
                  ),
                  stringsAsFactors = FALSE
                )
              }
            }
          }

          if (length(subgroup_models) > 0) {
            model_list[[resp]] <- subgroup_models
          }
        }
      }

      if (length(model_list) == 0) {
        validate(need(FALSE, "No models could be fitted for the selected configuration."))
      }

      if (length(overview_rows) > 0) {
        overview_df <- do.call(rbind, overview_rows)
        overview_df$Significance[is.na(overview_df$Significance)] <- ""
        overview_data(overview_df)
      } else {
        overview_data(NULL)
      }

      list(
        models = model_list,
        responses = names(model_list),
        subgroups = if (is.null(subgroup_var)) character(0) else unique(unlist(lapply(model_list, names))),
        subgroup_var = subgroup_var,
        factors = list(factor1 = input$factor1, factor2 = input$factor2),
        orders = list(order1 = input$order1, order2 = input$order2)
      )
    })

    output$overview_table <- renderDT({
      req(overview_data())
      datatable(
        overview_data(),
        options = list(pageLength = 5, scrollX = TRUE, dom = "tip"),
        rownames = FALSE
      )
    })

    output$summary_ui <- renderUI({
      model_info <- models()
      if (is.null(model_info)) {
        return(NULL)
      }

      responses <- model_info$responses
      subgroup_var <- model_info$subgroup_var

      if (length(responses) == 0) {
        return(NULL)
      }

      overview_block <- NULL
      if (!is.null(overview_data())) {
        overview_block <- tagList(
          h4("Overview of Results"),
          DTOutput(ns("overview_table")),
          br()
        )
      }

      model_list <- model_info$models

      if (is.null(subgroup_var)) {
        tabs <- lapply(seq_along(responses), function(i) {
          tabPanel(
            title = responses[i],
            tags$div(
              h5("Summary"),
              verbatimTextOutput(ns(paste0("summary_", i, "_1"))),
              br(),
              h5("Fixed effects"),
              DTOutput(ns(paste0("fixed_effects_", i, "_1"))),
              br(),
              downloadButton(ns(paste0("download_", i, "_1")), "Download Results (Word)")
            )
          )
        })

        tagList(
          overview_block,
          do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs))
        )
      } else {
        tabs <- lapply(seq_along(responses), function(i) {
          resp <- responses[i]
          resp_models <- model_list[[resp]]
          if (is.null(resp_models) || length(resp_models) == 0) {
            return(tabPanel(title = resp, tags$p("No data available for this response.")))
          }

          subgroup_names <- names(resp_models)

          subgroup_tabs <- lapply(seq_along(resp_models), function(j) {
            tabPanel(
              title = paste("Subgroup:", subgroup_names[j]),
              tags$div(
                h5("Summary"),
                verbatimTextOutput(ns(paste0("summary_", i, "_", j))),
                br(),
                h5("Fixed effects"),
                DTOutput(ns(paste0("fixed_effects_", i, "_", j))),
                br(),
                downloadButton(ns(paste0("download_", i, "_", j)), "Download Results (Word)")
              )
            )
          })

          do.call(
            tabPanel,
            c(
              list(title = resp),
              list(
                do.call(
                  tabsetPanel,
                  c(list(id = ns(paste0("subtabs_", i))), subgroup_tabs)
                )
              )
            )
          )
        })

        tagList(
          overview_block,
          do.call(tabsetPanel, c(list(id = ns("response_tabs")), tabs))
        )
      }
    })

    observeEvent(models(), {
      model_info <- models()
      if (is.null(model_info)) {
        return()
      }

      responses <- model_info$responses
      model_list <- model_info$models
      subgroup_var <- model_info$subgroup_var

      for (i in seq_along(responses)) {
        resp <- responses[i]
        resp_models <- model_list[[resp]]
        if (is.null(resp_models) || length(resp_models) == 0) {
          next
        }

        subgroup_names <- names(resp_models)

        for (j in seq_along(resp_models)) {
          model_obj <- resp_models[[j]]
          subgroup_name <- if (!is.null(subgroup_var) && length(subgroup_names) >= j) subgroup_names[j] else NULL

          local({
            idx_resp <- i
            idx_sub <- j
            response_name <- resp
            subgroup_label <- subgroup_name
            model_copy <- model_obj

            tidy_df <- broom::tidy(model_copy)
            numeric_cols <- vapply(tidy_df, is.numeric, logical(1))
            tidy_df[numeric_cols] <- lapply(tidy_df[numeric_cols], function(x) round(x, 4))

            output[[paste0("summary_", idx_resp, "_", idx_sub)]] <- renderPrint({
              summary(model_copy)
            })

            output[[paste0("fixed_effects_", idx_resp, "_", idx_sub)]] <- renderDT({
              datatable(
                tidy_df,
                options = list(scrollX = TRUE, pageLength = 5),
                rownames = FALSE
              )
            })

            output[[paste0("download_", idx_resp, "_", idx_sub)]] <- downloadHandler(
              filename = function() {
                safe_resp <- gsub("[^A-Za-z0-9]+", "_", response_name)
                safe_resp <- gsub("_+", "_", safe_resp)
                safe_resp <- gsub("^_|_$", "", safe_resp)
                if (!nzchar(safe_resp)) {
                  safe_resp <- paste0("response_", idx_resp)
                }

                if (!is.null(subgroup_label)) {
                  safe_sub <- gsub("[^A-Za-z0-9]+", "_", subgroup_label)
                  safe_sub <- gsub("_+", "_", safe_sub)
                  safe_sub <- gsub("^_|_$", "", safe_sub)
                  if (nzchar(safe_sub)) {
                    return(paste0("anova_results_", safe_resp, "_", safe_sub, "_", Sys.Date(), ".docx"))
                  }
                }

                paste0("anova_results_", safe_resp, "_", Sys.Date(), ".docx")
              },
              content = function(file) {
                tidy_export <- broom::tidy(model_copy)
                numeric_cols_export <- vapply(tidy_export, is.numeric, logical(1))
                tidy_export[numeric_cols_export] <- lapply(tidy_export[numeric_cols_export], function(x) round(x, 4))

                doc <- officer::read_docx()
                heading <- if (is.null(subgroup_label)) {
                  paste("ANOVA results for:", response_name)
                } else {
                  paste("ANOVA results for:", response_name, "| Subgroup:", subgroup_label)
                }
                doc <- officer::body_add_par(doc, heading, style = "heading 1")
                doc <- officer::body_add_par(doc, paste("Model formula:", format(formula(model_copy))), style = "heading 2")
                doc <- officer::body_add_par(doc, "R Output", style = "heading 2")
                summary_lines <- capture.output(summary(model_copy))
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
      }
    })

    return(models)
  })
}
