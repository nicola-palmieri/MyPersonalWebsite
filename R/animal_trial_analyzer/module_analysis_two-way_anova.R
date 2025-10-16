# ===============================================================
# 🧪 Animal Trial Analyzer — Two-way ANOVA Module
# ===============================================================
two_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order_1")),
      uiOutput(ns("level_order_2")),
      uiOutput(ns("advanced_options")),
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

    format_p_value <- function(p_values) {
      vapply(
        p_values,
        function(p) {
          if (is.na(p)) {
            return(NA_character_)
          }
          if (p < 0.001) {
            "<0.001"
          } else {
            sprintf("%.2f", round(p, 2))
          }
        },
        character(1)
      )
    }

    add_significance_marker <- function(formatted_p, raw_p) {
      mapply(
        function(fp, rp) {
          if (is.na(rp)) {
            return(fp)
          }
          if (rp < 0.05) {
            paste0(fp, "*")
          } else {
            fp
          }
        },
        formatted_p,
        raw_p,
        USE.NAMES = FALSE
      )
    }

    prepare_anova_outputs <- function(model_obj, factor_names) {
      old_contrasts <- options("contrasts")
      on.exit(options(old_contrasts), add = TRUE)
      options(contrasts = c("contr.sum", "contr.poly"))

      anova_obj <- car::Anova(model_obj, type = 3)
      anova_df <- as.data.frame(anova_obj)
      anova_df$Effect <- rownames(anova_df)
      rownames(anova_df) <- NULL
      anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]

      p_col <- grep("^Pr", names(anova_df), value = TRUE)
      p_col <- if (length(p_col) > 0) p_col[1] else NULL
      raw_p <- if (!is.null(p_col)) anova_df[[p_col]] else rep(NA_real_, nrow(anova_df))

      for (col in names(anova_df)) {
        if (is.numeric(anova_df[[col]])) {
          anova_df[[col]] <- round(anova_df[[col]], 2)
        }
      }

      anova_significant <- !is.na(raw_p) & raw_p < 0.05
      if (!is.null(p_col)) {
        formatted_p <- format_p_value(raw_p)
        anova_df[[p_col]] <- add_significance_marker(formatted_p, raw_p)
        names(anova_df)[names(anova_df) == p_col] <- "p.value"
      } else {
        anova_df$p.value <- NA_character_
      }

      factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
      posthoc_details <- list()
      posthoc_combined <- NULL
      posthoc_significant <- numeric(0)

      for (factor_nm in factor_names) {
        if (!factor_nm %in% names(model_obj$model)) {
          next
        }

        res <- tryCatch({
          emm <- emmeans::emmeans(model_obj, specs = factor_nm)
          contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
          as.data.frame(summary(contrasts))
        }, error = function(e) {
          list(error = e$message)
        })

        if (is.data.frame(res)) {
          res$Factor <- factor_nm
          posthoc_details[[factor_nm]] <- list(table = res, error = NULL)
          posthoc_combined <- rbind(posthoc_combined, res)
        } else {
          posthoc_details[[factor_nm]] <- list(table = NULL, error = res$error)
        }
      }

      if (!is.null(posthoc_combined)) {
        posthoc_combined <- posthoc_combined[, c("Factor", setdiff(names(posthoc_combined), "Factor"))]
        numeric_cols <- names(posthoc_combined)[sapply(posthoc_combined, is.numeric)]
        if (length(numeric_cols) > 0) {
          for (col in numeric_cols) {
            posthoc_combined[[col]] <- round(posthoc_combined[[col]], 2)
          }
        }

        if ("p.value" %in% names(posthoc_combined)) {
          raw_posthoc_p <- posthoc_combined$p.value
          posthoc_significant <- !is.na(raw_posthoc_p) & raw_posthoc_p < 0.05
          formatted_posthoc_p <- format_p_value(raw_posthoc_p)
          posthoc_combined$p.value <- add_significance_marker(formatted_posthoc_p, raw_posthoc_p)
        } else {
          posthoc_significant <- rep(FALSE, nrow(posthoc_combined))
        }
      }

      list(
        anova_object = anova_obj,
        anova_table = anova_df,
        anova_significant = anova_significant,
        posthoc_details = posthoc_details,
        posthoc_table = posthoc_combined,
        posthoc_significant = posthoc_significant
      )
    }

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
      data[[input$factor1]] <- factor(data[[input$factor1]], levels = input$order1)
      data[[input$factor2]] <- factor(data[[input$factor2]], levels = input$order2)

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
          model_formula <- as.formula(paste(resp, "~", input$factor1, "*", input$factor2))
          model_list[[resp]] <- lm(model_formula, data = data)
        }

        return(list(
          models = model_list,
          responses = responses,
          strata = NULL,
          factors = list(factor1 = input$factor1, factor2 = input$factor2),
          orders = list(order1 = input$order1, order2 = input$order2)
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
        subset_data[[input$factor1]] <- factor(subset_data[[input$factor1]], levels = input$order1)
        subset_data[[input$factor2]] <- factor(subset_data[[input$factor2]], levels = input$order2)

        model_list[[stratum]] <- list()
        for (resp in responses) {
          model_formula <- as.formula(paste(resp, "~", input$factor1, "*", input$factor2))
          model_list[[stratum]][[resp]] <- lm(model_formula, data = subset_data)
        }
      }

      list(
        models = model_list,
        responses = responses,
        strata = list(var = strat_var, levels = strata),
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

                doc <- officer::read_docx()
                doc <- officer::body_add_par(doc, paste("ANOVA results for:", response_name), style = "heading 1")
                doc <- officer::body_add_par(doc, paste("Model formula:", format(formula(model_obj))), style = "heading 2")
                doc <- officer::body_add_par(doc, "Type III ANOVA results", style = "heading 2")

                ft_anova <- flextable::flextable(results$anova_table)
                if (nrow(results$anova_table) > 0) {
                  ft_anova <- flextable::bold(ft_anova, i = which(results$anova_significant), j = "p.value", bold = TRUE)
                }
                ft_anova <- flextable::autofit(ft_anova)
                doc <- flextable::body_add_flextable(doc, ft_anova)

                doc <- officer::body_add_par(doc, "Post-hoc Tukey comparisons", style = "heading 2")

                if (is.null(results$posthoc_table) || nrow(results$posthoc_table) == 0) {
                  doc <- officer::body_add_par(doc, "No post-hoc Tukey comparisons were generated.", style = "Normal")
                } else {
                  ft_posthoc <- flextable::flextable(results$posthoc_table)
                  if (length(results$posthoc_significant) > 0) {
                    ft_posthoc <- flextable::bold(ft_posthoc, i = which(results$posthoc_significant), j = "p.value", bold = TRUE)
                  }
                  ft_posthoc <- flextable::autofit(ft_posthoc)
                  doc <- flextable::body_add_flextable(doc, ft_posthoc)
                }

                doc <- officer::body_add_par(doc, "Significant differences are indicated by p < 0.05.", style = "Normal")

                print(doc, target = file)
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

                doc <- officer::read_docx()
                doc <- officer::body_add_par(doc, paste("ANOVA results for:", response_name), style = "heading 1")
                doc <- officer::body_add_par(doc, paste("Stratum:", stratum_label), style = "heading 2")
                doc <- officer::body_add_par(doc, paste("Model formula:", format(formula(model_obj))), style = "heading 2")
                doc <- officer::body_add_par(doc, "Type III ANOVA results", style = "heading 2")

                ft_anova <- flextable::flextable(results$anova_table)
                if (nrow(results$anova_table) > 0) {
                  ft_anova <- flextable::bold(ft_anova, i = which(results$anova_significant), j = "p.value", bold = TRUE)
                }
                ft_anova <- flextable::autofit(ft_anova)
                doc <- flextable::body_add_flextable(doc, ft_anova)

                doc <- officer::body_add_par(doc, "Post-hoc Tukey comparisons", style = "heading 2")

                if (is.null(results$posthoc_table) || nrow(results$posthoc_table) == 0) {
                  doc <- officer::body_add_par(doc, "No post-hoc Tukey comparisons were generated.", style = "Normal")
                } else {
                  ft_posthoc <- flextable::flextable(results$posthoc_table)
                  if (length(results$posthoc_significant) > 0) {
                    ft_posthoc <- flextable::bold(ft_posthoc, i = which(results$posthoc_significant), j = "p.value", bold = TRUE)
                  }
                  ft_posthoc <- flextable::autofit(ft_posthoc)
                  doc <- flextable::body_add_flextable(doc, ft_posthoc)
                }

                doc <- officer::body_add_par(doc, "Significant differences are indicated by p < 0.05.", style = "Normal")

                print(doc, target = file)
              }
            )
          })
        }
      }
    })

    output$advanced_options <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      choices <- c("None", setdiff(unique(cat_cols), "None"))

      tags$details(
        open = FALSE,
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

    return(models)
  })
}
