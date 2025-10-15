# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module
# ===============================================================
library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)

visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("4. Visualization"),
    fluidRow(
      column(
        width = 6,
        numericInput(
          ns("plot_width"),
          label = "Plot width (inches)",
          value = 8,
          min = 3,
          max = 24,
          step = 0.5
        )
      ),
      column(
        width = 6,
        numericInput(
          ns("plot_height"),
          label = "Plot height (inches)",
          value = 4.5,
          min = 3,
          max = 16,
          step = 0.5
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          ns("grid_rows"),
          label = "Grid rows (optional)",
          value = NA,
          min = 1,
          step = 1
        )
      ),
      column(
        width = 6,
        numericInput(
          ns("grid_cols"),
          label = "Grid columns (optional)",
          value = NA,
          min = 1,
          step = 1
        )
      )
    ),
    downloadButton(ns("download_plot"), "Download PNG (300 dpi)"),
    plotOutput(ns("mean_se_plot"))
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactive({
      req(filtered_data())
      filtered_data()
    })

    model_info <- reactive({
      req(model_fit())
      model_fit()
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

      responses <- info$responses
      plot_objects <- list()

      for (resp in responses) {
        if (is.null(factor2)) {
          stats <- data |>
            group_by(.data[[factor1]]) |>
            summarise(
              mean = mean(.data[[resp]], na.rm = TRUE),
              se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
              .groups = "drop"
            )

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
          stats <- data |>
            group_by(.data[[factor1]], .data[[factor2]]) |>
            summarise(
              mean = mean(.data[[resp]], na.rm = TRUE),
              se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
              .groups = "drop"
            )

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
        }

        plot_objects[[resp]] <- p
      }

      plot_objects
    })

    plot_obj <- reactive({
      plots <- plot_list()
      if (is.null(plots) || length(plots) == 0) {
        return(NULL)
      }

      n <- length(plots)
      n_row <- input$grid_rows
      n_col <- input$grid_cols

      if (is.null(n_row) || is.na(n_row) || n_row < 1) {
        n_row <- ceiling(sqrt(n))
      }
      if (is.null(n_col) || is.na(n_col) || n_col < 1) {
        n_col <- ceiling(n / n_row)
      }

      patchwork::wrap_plots(plotlist = plots, nrow = n_row, ncol = n_col)
    })


    output$mean_se_plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    },
    width = function() input$plot_width * 96,
    height = function() input$plot_height * 96,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("mean_se_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(plot_obj(), input$plot_width, input$plot_height)
        width_in <- input$plot_width
        height_in <- input$plot_height
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
    )
  })
}
