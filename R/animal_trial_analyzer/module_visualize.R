# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module (Simplified adaptive sizing)
# ===============================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)

visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("4. Visualization"),
    uiOutput(ns("subgroup_selector")),
    radioButtons(
      ns("display_mode"),
      label = "Display mode:",
      choices = c("Single response tabs" = "single", "All responses in one panel" = "multi"),
      selected = "single",
      inline = TRUE
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          ns("grid_rows"),
          label = "Grid rows (optional)",
          value = 0,        # 0 means auto
          min = 0, step = 1
        )
      ),
      column(
        width = 6,
        numericInput(
          ns("grid_cols"),
          label = "Grid columns (optional)",
          value = 0,        # 0 means auto
          min = 0, step = 1
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          ns("subplot_width"),
          label = "Subplot width (px)",
          value = 300,
          min = 200,
          max = 1200,
          step = 50
        )
      ),
      column(
        width = 6,
        numericInput(
          ns("subplot_height"),
          label = "Subplot height (px)",
          value = 200,
          min = 200,
          max = 1200,
          step = 50
        )
      )
    ),
    downloadButton(ns("download_plot"), "Download PNG (300 dpi)"),
    uiOutput(ns("plot_container"))
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })

    output$subgroup_selector <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      selected_value <- input$subgroup_var
      if (is.null(selected_value) || !(selected_value %in% cat_cols)) {
        selected_value <- ""
      }

      selectInput(
        ns("subgroup_var"),
        "Subgroup variable (optional):",
        choices = c("None" = "", cat_cols),
        selected = selected_value
      )
    })

    model_info <- reactive({
      req(model_fit())
      model_fit()
    })

    subgroup_details <- reactive({
      req(df())
      var <- input$subgroup_var
      if (is.null(var) || !nzchar(var)) {
        return(NULL)
      }

      data <- df()
      if (!(var %in% names(data))) {
        return(NULL)
      }

      levels <- unique(as.character(stats::na.omit(data[[var]])))
      list(var = var, levels = levels)
    })
    
    plot_list <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$models) || length(info$models) == 0) {
        return(NULL)
      }

      data <- df()
      req(data)

      factor1 <- info$factors$factor1
      factor2 <- info$factors$factor2
      order1 <- info$orders$order1
      order2 <- info$orders$order2

      if (!is.null(factor1) && !is.null(order1)) {
        data[[factor1]] <- factor(data[[factor1]], levels = order1)
      }
      if (!is.null(factor2) && !is.null(order2)) {
        data[[factor2]] <- factor(data[[factor2]], levels = order2)
      }

      subgroup_info <- subgroup_details()
      subgroup_var <- NULL
      subgroup_levels <- NULL
      if (!is.null(subgroup_info)) {
        subgroup_var <- subgroup_info$var
        subgroup_levels <- subgroup_info$levels
        if (length(subgroup_levels) > 10) {
          validate(need(FALSE, "Please select a subgroup variable with at most 10 categories."))
        }
      }

      responses <- info$responses
      plot_objects <- list()

      for (resp in responses) {
        resp_data <- data
        resp_data <- resp_data[!is.na(resp_data[[resp]]), , drop = FALSE]
        if (!is.null(subgroup_var)) {
          resp_data <- resp_data[!is.na(resp_data[[subgroup_var]]), , drop = FALSE]
        }

        if (nrow(resp_data) == 0) {
          next
        }

        if (is.null(factor2)) {
          if (is.null(subgroup_var)) {
            stats <- resp_data |>
              group_by(.data[[factor1]]) |>
              summarise(
                mean = mean(.data[[resp]], na.rm = TRUE),
                se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
                .groups = "drop"
              )
          } else {
            stats <- resp_data |>
              group_by(.data[[subgroup_var]], .data[[factor1]]) |>
              summarise(
                mean = mean(.data[[resp]], na.rm = TRUE),
                se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
                .groups = "drop"
              )
          }

          if (nrow(stats) == 0) {
            next
          }

          if (is.null(subgroup_var)) {
            p <- ggplot(stats, aes_string(x = factor1, y = "mean")) +
              geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +
              geom_point(size = 3, color = "steelblue") +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                            width = 0.15, color = "gray40") +
              theme_minimal(base_size = 14) +
              labs(x = factor1, y = "Mean ± SE") +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
              ) +
              ggtitle(resp)
          } else {
            available_levels <- unique(as.character(stats[[subgroup_var]]))
            available_levels <- subgroup_levels[subgroup_levels %in% available_levels]
            if (length(available_levels) == 0) {
              next
            }

            stats$subgroup_label <- factor(
              paste0("Subgroup: ", stats[[subgroup_var]]),
              levels = paste0("Subgroup: ", available_levels)
            )

            facet_rows <- if (length(available_levels) <= 5) 1 else 2

            p <- ggplot(stats, aes_string(x = factor1, y = "mean")) +
              geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +
              geom_point(size = 3, color = "steelblue") +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                            width = 0.15, color = "gray40") +
              theme_minimal(base_size = 14) +
              labs(x = factor1, y = "Mean ± SE") +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
              ) +
              ggtitle(resp) +
              facet_wrap(~ subgroup_label, scales = "fixed", nrow = facet_rows)
          }
        } else {
          if (is.null(subgroup_var)) {
            stats <- resp_data |>
              group_by(.data[[factor1]], .data[[factor2]]) |>
              summarise(
                mean = mean(.data[[resp]], na.rm = TRUE),
                se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
                .groups = "drop"
              )
          } else {
            stats <- resp_data |>
              group_by(.data[[subgroup_var]], .data[[factor1]], .data[[factor2]]) |>
              summarise(
                mean = mean(.data[[resp]], na.rm = TRUE),
                se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
                .groups = "drop"
              )
          }

          if (nrow(stats) == 0) {
            next
          }

          if (is.null(subgroup_var)) {
            p <- ggplot(stats, aes_string(
              x = factor1,
              y = "mean",
              color = factor2,
              group = factor2
            )) +
              geom_line(linewidth = 1) +
              geom_point(size = 3) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                            width = 0.15) +
              theme_minimal(base_size = 14) +
              labs(
                x = factor1,
                y = "Mean ± SE",
                color = factor2
              ) +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
              ) +
              ggtitle(resp)
          } else {
            available_levels <- unique(as.character(stats[[subgroup_var]]))
            available_levels <- subgroup_levels[subgroup_levels %in% available_levels]
            if (length(available_levels) == 0) {
              next
            }

            stats$subgroup_label <- factor(
              paste0("Subgroup: ", stats[[subgroup_var]]),
              levels = paste0("Subgroup: ", available_levels)
            )

            facet_rows <- if (length(available_levels) <= 5) 1 else 2

            p <- ggplot(stats, aes_string(
              x = factor1,
              y = "mean",
              color = factor2,
              group = factor2
            )) +
              geom_line(linewidth = 1) +
              geom_point(size = 3) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                            width = 0.15) +
              theme_minimal(base_size = 14) +
              labs(
                x = factor1,
                y = "Mean ± SE",
                color = factor2
              ) +
              theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank()
              ) +
              ggtitle(resp) +
              facet_wrap(~ subgroup_label, scales = "fixed", nrow = facet_rows)
          }
        }

        plot_objects[[resp]] <- p
      }

      if (length(plot_objects) == 0) {
        return(list())
      }

      if (is.null(names(plot_objects))) {
        names(plot_objects) <- paste0("Response_", seq_along(plot_objects))
      }

      plot_objects
    })
    
    # Combine plots for multi-display mode
    plot_obj <- reactive({
      plots <- plot_list()
      if (is.null(plots) || length(plots) == 0) {
        return(NULL)
      }

      if (!identical(input$display_mode, "multi")) {
        return(NULL)
      }

      n <- length(plots)
      n_row <- suppressWarnings(as.numeric(input$grid_rows))
      n_col <- suppressWarnings(as.numeric(input$grid_cols))

      if (is.na(n_row) || n_row < 1) n_row <- ceiling(sqrt(n))
      if (is.na(n_col) || n_col < 1) n_col <- ceiling(n / n_row)

      patchwork::wrap_plots(plotlist = plots, nrow = n_row, ncol = n_col)
    })

    # ---- Dynamic sizing logic (pixels) ----
    plot_size <- reactive({
      plots <- plot_list()
      w_sub <- input$subplot_width
      h_sub <- input$subplot_height

      if (is.null(plots) || length(plots) == 0) {
        return(list(w = w_sub, h = h_sub))
      }

      if (!identical(input$display_mode, "multi") || length(plots) == 1) {
        return(list(w = w_sub, h = h_sub))
      }

      n <- length(plots)
      n_row <- suppressWarnings(as.numeric(input$grid_rows))
      n_col <- suppressWarnings(as.numeric(input$grid_cols))
      if (is.na(n_row) || n_row < 1) n_row <- ceiling(sqrt(n))
      if (is.na(n_col) || n_col < 1) n_col <- ceiling(n / n_row)

      list(
        w = w_sub * n_col,
        h = h_sub * n_row
      )
    })

    output$plot_container <- renderUI({
      plots <- plot_list()
      if (is.null(plots)) {
        return(NULL)
      }

      if (length(plots) == 0) {
        return(tags$p("No plots available for the selected configuration."))
      }

      responses <- names(plots)
      if (length(responses) == 0) {
        responses <- paste("Response", seq_along(plots))
      }

      if (identical(input$display_mode, "single")) {
        tabs <- lapply(seq_along(plots), function(i) {
          tabPanel(
            title = responses[i],
            plotOutput(ns(paste0("plot_", i)))
          )
        })

        return(do.call(tabsetPanel, c(list(id = ns("response_plots")), tabs)))
      }

      plotOutput(ns("combined_plot"), res = 96)
    })

    observeEvent(plot_list(), {
      plots <- plot_list()
      if (is.null(plots) || length(plots) == 0) {
        return()
      }

      responses <- names(plots)
      if (length(responses) == 0) {
        responses <- paste("Response", seq_along(plots))
      }

      for (i in seq_along(plots)) {
        response_id <- responses[i]
        local({
          idx <- i
          resp_name <- response_id
          output[[paste0("plot_", idx)]] <- renderPlot({
            req(identical(input$display_mode, "single"))
            plots_current <- plot_list()
            if (is.null(plots_current) || length(plots_current) == 0) {
              return(NULL)
            }

            plot_names <- names(plots_current)
            target_name <- resp_name
            if (!is.null(plot_names) && length(plot_names) >= idx) {
              if (!(target_name %in% plot_names)) {
                target_name <- plot_names[idx]
              }
            } else if (!is.null(plot_names) && length(plot_names) > 0) {
              target_name <- plot_names[min(idx, length(plot_names))]
            }

            plot_to_draw <- plots_current[[target_name]]
            req(plot_to_draw)
            plot_to_draw
          },
          width = function() input$subplot_width,
          height = function() input$subplot_height,
          res = 96)
        })
      }
    })

    output$combined_plot <- renderPlot({
      req(identical(input$display_mode, "multi"))
      req(plot_obj())
      plot_obj()
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    # ---- Download Plot ----
    output$download_plot <- downloadHandler(
      filename = function() {
        plots <- plot_list()
        req(!is.null(plots), length(plots) > 0)

        if (identical(input$display_mode, "single")) {
          responses <- names(plots)
          active <- input$response_plots
          if (is.null(active) || !nzchar(active) || !(active %in% responses)) {
            active <- if (length(responses) > 0) responses[1] else names(plots)[1]
          }

          safe_resp <- gsub("[^A-Za-z0-9]+", "_", active)
          safe_resp <- gsub("_+", "_", safe_resp)
          safe_resp <- gsub("^_|_$", "", safe_resp)
          if (!nzchar(safe_resp)) {
            safe_resp <- paste0("response_", 1)
          }

          paste0("mean_se_plot_", safe_resp, "_", Sys.Date(), ".png")
        } else {
          paste0("mean_se_plot_composite_", Sys.Date(), ".png")
        }
      },
      content = function(file) {
        plots <- plot_list()
        req(!is.null(plots), length(plots) > 0)

        if (identical(input$display_mode, "single")) {
          responses <- names(plots)
          active <- input$response_plots
          if (is.null(active) || !nzchar(active) || !(active %in% responses)) {
            active <- if (length(responses) > 0) responses[1] else names(plots)[1]
          }

          plot_to_save <- plots[[active]]
          req(plot_to_save)

          width_in <- input$subplot_width / 96
          height_in <- input$subplot_height / 96

          ggsave(
            filename = file,
            plot = plot_to_save,
            device = "png",
            dpi = 300,
            width = width_in,
            height = height_in,
            units = "in",
            limitsize = FALSE
          )
        } else {
          req(plot_obj())
          dims <- plot_size()
          width_in <- dims$w / 96
          height_in <- dims$h / 96

          ggsave(
            filename = file,
            plot = plot_obj(),
            device = "png",
            dpi = 300,
            width = width_in,
            height = height_in,
            units = "in",
            limitsize = FALSE
          )
        }
      }
    )
  })
}
