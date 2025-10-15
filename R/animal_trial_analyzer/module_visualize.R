# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module
# ===============================================================
library(shiny)
library(ggplot2)
library(dplyr)

visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("4. Visualization"),
    p("Line plot showing group means ± standard error (for one-way ANOVA)."),
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
    
    # -----------------------------------------------------------
    # Extract variable names from fitted model
    # -----------------------------------------------------------
    vars <- reactive({
      req(model_fit())
      form <- formula(model_fit())
      all_vars <- all.vars(form)
      
      # Handle one-way or two-way formulas
      if (length(all_vars) == 2) {
        list(response = all_vars[1], factor1 = all_vars[2], factor2 = NULL)
      } else if (length(all_vars) == 3) {
        list(response = all_vars[1], factor1 = all_vars[2], factor2 = all_vars[3])
      } else {
        stop("Unsupported model structure.")
      }
    })
    
    
    # -----------------------------------------------------------
    # Compute summary statistics
    # -----------------------------------------------------------
    summary_stats <- reactive({
      req(df(), vars())
      data <- df()
      resp <- vars()$response
      f1 <- vars()$factor1
      f2 <- vars()$factor2
      
      if (is.null(f2)) {
        # One-way ANOVA
        data |>
          group_by(.data[[f1]]) |>
          summarise(
            mean = mean(.data[[resp]], na.rm = TRUE),
            se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
            .groups = "drop"
          )
      } else {
        # Two-way ANOVA
        data |>
          group_by(.data[[f1]], .data[[f2]]) |>
          summarise(
            mean = mean(.data[[resp]], na.rm = TRUE),
            se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
            .groups = "drop"
          )
      }
    })
    
    
    # -----------------------------------------------------------
    # Plot mean ± SE
    # -----------------------------------------------------------
    plot_obj <- reactive({
      req(summary_stats(), vars())
      stats <- summary_stats()
      f1 <- vars()$factor1
      f2 <- vars()$factor2
      
      if (is.null(f2)) {
        # One-way plot (same as before)
        ggplot(stats, aes(x = !!sym(f1), y = mean, group = 1)) +
          geom_line(color = "steelblue", linewidth = 1) +
          geom_point(size = 3, color = "steelblue") +
          geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                        width = 0.15, color = "gray40") +
          theme_minimal(base_size = 14) +
          labs(x = f1, y = "Mean ± SE") +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()
          )
      } else {
        # Two-way plot (factor1 on x-axis, factor2 as color)
        ggplot(stats, aes(
          x = !!sym(f1),
          y = mean,
          color = !!sym(f2),
          group = !!sym(f2)
        )) +
          geom_line(linewidth = 1) +
          geom_point(size = 3) +
          geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                        width = 0.15) +
          theme_minimal(base_size = 14) +
          labs(
            x = f1,
            y = "Mean ± SE",
            color = f2
          ) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()
          )
      }
    })
    

    output$mean_se_plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    },
    width = function() input$plot_width,
    height = function() input$plot_height,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("mean_se_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(plot_obj(), input$plot_width, input$plot_height)
        width_in <- input$plot_width / 300
        height_in <- input$plot_height / 300
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
