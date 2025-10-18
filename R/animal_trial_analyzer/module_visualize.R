# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module (Simplified adaptive sizing)
# ===============================================================

visualize_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize Outcomes"),
      p("Tweak the layout of the mean ± SE plots, download the figure, and wrap up your workflow."),
      hr(),
      uiOutput(ns("layout_controls")),
      hr(),
      fluidRow(
        column(
          6,
          numericInput(ns("subplot_width"), "Subplot width (px)", 300, 200, 1200, 50)
        ),
        column(
          6,
          numericInput(ns("subplot_height"), "Subplot height (px)", 200, 200, 1200, 50)
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download PNG (300 dpi)"),
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
      plotOutput(ns("mean_se_plot"))
    )
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactive(filtered_data())
    model_info <- reactive(model_fit())

    # --- Small helpers ---
    valid_num <- function(x, default = 0) {
      v <- suppressWarnings(as.numeric(x))
      if (is.null(v) || is.na(v) || v <= 0) default else v
    }

    compute_layout <- function(n_items, rows_input, cols_input) {
      if (is.null(n_items) || n_items <= 0) return(list(nrow = 1, ncol = 1))
      r <- valid_num(rows_input, 0)
      c <- valid_num(cols_input, 0)
      if (r > 0 && c > 0) {
        nrow <- r
        ncol <- max(c, ceiling(n_items / r))
      } else if (r > 0) {
        nrow <- r
        ncol <- ceiling(n_items / r)
      } else if (c > 0) {
        ncol <- c
        nrow <- ceiling(n_items / c)
      } else {
        nrow <- ifelse(n_items <= 5, 1, 2)
        ncol <- ceiling(n_items / nrow)
      }
      list(nrow = max(1, nrow), ncol = max(1, ncol))
    }

    # --- Reactive holders ---
    layout_overrides <- reactiveValues(strata_rows = 0, strata_cols = 0, resp_rows = 0, resp_cols = 0)
    layout_manual    <- reactiveValues(strata_rows = FALSE, strata_cols = FALSE, resp_rows = FALSE, resp_cols = FALSE)
    suppress_updates <- reactiveValues(strata_rows = TRUE, strata_cols = TRUE, resp_rows = TRUE, resp_cols = TRUE)

    # --- Observe numeric inputs (DRY) ---
    observe_numeric_input <- function(id_name) {
      observeEvent(input[[id_name]], {
        if (isTRUE(suppress_updates[[id_name]])) {
          suppress_updates[[id_name]] <- FALSE
          return()
        }
        val <- valid_num(input[[id_name]], 0)
        layout_overrides[[id_name]] <- as.integer(val)
        layout_manual[[id_name]] <- val > 0
      })
    }

    lapply(c("strata_rows", "strata_cols", "resp_rows", "resp_cols"), observe_numeric_input)

    effective_input <- function(name) if (layout_manual[[name]]) layout_overrides[[name]] else 0

    # --- Layout controls ---
    output$layout_controls <- renderUI({
      info <- model_info()
      has_strata <- !is.null(info$strata$var)
      n_responses <- length(info$responses %||% 0)

      make_grid_controls <- function(prefix, title) {
        tagList(
          h5(title),
          fluidRow(
            column(
              6,
              numericInput(ns(paste0(prefix, "_rows")), "Grid rows",
                           value = isolate(valid_num(input[[paste0(prefix, "_rows")]], 1)),
                           min = 0, step = 1)
            ),
            column(
              6,
              numericInput(ns(paste0(prefix, "_cols")), "Grid columns",
                           value = isolate(valid_num(input[[paste0(prefix, "_cols")]], 1)),
                           min = 0, step = 1)
            )
          )
        )
      }

      tagList(
        h4("Layout Controls"),
        if (has_strata) make_grid_controls("strata", "Within each response (across strata):"),
        if (n_responses > 1) make_grid_controls("resp", "Across responses:")
      )
    })

    # --- Plot computation ---
    plot_obj_info <- reactive({
      info <- model_info()
      if (is.null(info) || length(info$models) == 0) return(NULL)

      data <- df()
      factor1 <- info$factors$factor1
      factor2 <- info$factors$factor2
      order1  <- info$orders$order1
      order2  <- info$orders$order2

      if (!is.null(factor1) && !is.null(order1)) data[[factor1]] <- factor(data[[factor1]], levels = order1)
      if (!is.null(factor2) && !is.null(order2)) data[[factor2]] <- factor(data[[factor2]], levels = order2)

      responses <- info$responses
      has_strata <- !is.null(info$strata$var)
      strat_var  <- if (has_strata) info$strata$var else NULL
      strata_levels <- if (has_strata) info$strata$levels %||% unique(na.omit(as.character(data[[strat_var]]))) else character(0)

      compute_stats <- function(df_sub, resp_name) {
        if (is.null(factor2))
          df_sub |>
            group_by(.data[[factor1]]) |>
            summarise(
              mean = mean(.data[[resp_name]], na.rm = TRUE),
              se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
              .groups = "drop"
            )
        else
          df_sub |>
            group_by(.data[[factor1]], .data[[factor2]]) |>
            summarise(
              mean = mean(.data[[resp_name]], na.rm = TRUE),
              se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
              .groups = "drop"
            )
      }

      build_plot <- function(stats_df, title_text, y_limits) {
        if (is.null(factor2)) {
          p <- ggplot(stats_df, aes_string(x = factor1, y = "mean")) +
            geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +
            geom_point(size = 3, color = "steelblue") +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                          width = 0.15, color = "gray40")
        } else {
          p <- ggplot(stats_df, aes_string(
            x = factor1, y = "mean", color = factor2, group = factor2
          )) +
            geom_line(linewidth = 1) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                          width = 0.15)
        }
        
        p <- p +
          theme_minimal(base_size = 14) +
          labs(x = factor1, y = "Mean ± SE", color = factor2) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()
          )
        
        if (!is.null(y_limits) && all(is.finite(y_limits))) {
          p <- p + scale_y_continuous(limits = y_limits)
        }
        
        p + ggtitle(title_text) +
          theme(plot.title = element_text(size = 12, face = "bold"))
      }
      

      response_plots <- list()

      for (resp in responses) {
        if (has_strata) {
          strata_plots <- list()
          y_values <- c()

          for (s in strata_levels) {
            sub <- data[data[[strat_var]] == s & !is.na(data[[strat_var]]), , drop = FALSE]
            if (nrow(sub) == 0) next
            st <- compute_stats(sub, resp)
            if (nrow(st) == 0) next
            y_values <- c(y_values, st$mean - st$se, st$mean + st$se)
            strata_plots[[s]] <- st
          }

          if (length(strata_plots) == 0) next
          y_lim <- range(y_values, na.rm = TRUE)
          if (!all(is.finite(y_lim))) y_lim <- NULL

          strata_plot_objs <- lapply(names(strata_plots), function(nm)
            build_plot(strata_plots[[nm]], nm, y_lim)
          )

          lay <- compute_layout(length(strata_plot_objs),
                                effective_input("strata_rows"),
                                effective_input("strata_cols"))

          combined <- patchwork::wrap_plots(strata_plot_objs, nrow = lay$nrow, ncol = lay$ncol)
          title_plot <- ggplot() + theme_void() +
            ggtitle(resp) +
            theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.margin = margin(t = 0, b = 6))
          response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))

        } else {
          st <- compute_stats(data, resp)
          if (nrow(st) == 0) next
          y_vals <- c(st$mean - st$se, st$mean + st$se)
          y_lim <- if (all(is.finite(y_vals))) range(y_vals, na.rm = TRUE) else NULL
          response_plots[[resp]] <- build_plot(st, resp, y_lim)
        }
      }

      if (length(response_plots) == 0) return(NULL)

      resp_layout <- compute_layout(length(response_plots),
                                    effective_input("resp_rows"),
                                    effective_input("resp_cols"))

      final_plot <- if (length(response_plots) == 1)
        response_plots[[1]]
      else
        patchwork::wrap_plots(response_plots,
                              nrow = resp_layout$nrow,
                              ncol = resp_layout$ncol) &
          patchwork::plot_layout(guides = "collect")

      list(plot = final_plot, layout = list(responses = resp_layout), has_strata = has_strata)
    })

    # --- Sync inputs when plots update ---
    observeEvent(plot_obj_info(), {
      info <- plot_obj_info()
      if (is.null(info)) return()

      sync_input <- function(id, value, manual_key) {
        v <- ifelse(is.null(value) || value <= 0, 1, value)
        if (!layout_manual[[manual_key]]) {
          suppress_updates[[id]] <- TRUE
          updateNumericInput(session, id, value = v)
        }
      }

      if (info$has_strata) {
        sync_input("strata_rows", 1, "strata_rows")
        sync_input("strata_cols", 1, "strata_cols")
      }
      sync_input("resp_rows", info$layout$responses$nrow, "resp_rows")
      sync_input("resp_cols", info$layout$responses$ncol, "resp_cols")
    })

    # --- Plot rendering + download ---
    plot_obj <- reactive(plot_obj_info()$plot)

    plot_size <- reactive({
      s <- plot_obj_info()
      w <- valid_num(input$subplot_width, 300)
      h <- valid_num(input$subplot_height, 200)
      if (is.null(s)) return(list(w = w, h = h))
      list(w = w * s$layout$responses$ncol, h = h * s$layout$responses$nrow)
    })

    output$mean_se_plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() paste0("mean_se_plot_", Sys.Date(), ".png"),
      content = function(file) {
        s <- plot_size()
        ggsave(file, plot = plot_obj(), device = "png", dpi = 300,
               width = s$w / 96, height = s$h / 96, units = "in", limitsize = FALSE)
      }
    )
  })
}
