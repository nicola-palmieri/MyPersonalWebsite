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
    plotOutput(ns("mean_se_plot"), height = "400px")
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
      list(response = all_vars[1], group = all_vars[2])
    })
    
    # -----------------------------------------------------------
    # Compute summary statistics
    # -----------------------------------------------------------
    summary_stats <- reactive({
      req(df(), vars())
      data <- df()
      resp <- vars()$response
      grp <- vars()$group
      
      data |>
        group_by(.data[[grp]]) |>
        summarise(
          mean = mean(.data[[resp]], na.rm = TRUE),
          se = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
          .groups = "drop"
        )
    })
    
    # -----------------------------------------------------------
    # Plot mean ± SE
    # -----------------------------------------------------------
    output$mean_se_plot <- renderPlot({
      req(summary_stats(), vars())
      stats <- summary_stats()
      grp <- vars()$group
      
      ggplot(stats, aes(x = !!sym(grp), y = mean, group = 1)) +
        geom_line(color = "steelblue", linewidth = 1) +
        geom_point(size = 3, color = "steelblue") +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.15, color = "gray40") +
        theme_minimal(base_size = 14) +
        labs(x = grp, y = "Mean ± SE") +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
    })
  })
}
