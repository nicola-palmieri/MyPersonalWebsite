# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Module
# ===============================================================
library(shiny)
library(DT)
library(broom)

analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("3. Statistical Analysis"),
    selectInput(
      ns("analysis_type"),
      "Select analysis type:",
      choices = c("ANOVA"),
      selected = "ANOVA"
    ),
    uiOutput(ns("analysis_inputs")),
    uiOutput(ns("level_order_ui")),
    br(),
    actionButton(ns("run_anova"), "Run ANOVA"),
    br(), br(),
    verbatimTextOutput(ns("model_summary")),
    DTOutput(ns("fixed_effects_table"))
  )
}

analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # -----------------------------------------------------------
    # Dynamic inputs for ANOVA
    # -----------------------------------------------------------
    output$analysis_inputs <- renderUI({
      req(df(), input$analysis_type == "ANOVA")
      data <- df()
      
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        selectInput(
          ns("response_var"),
          "Response variable (numeric):",
          choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL
        ),
        selectInput(
          ns("group_var"),
          "Grouping variable (factor):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    # -----------------------------------------------------------
    # Level order selector (after choosing grouping variable)
    # -----------------------------------------------------------
    output$level_order_ui <- renderUI({
      req(df(), input$group_var)
      data <- df()
      group_values <- unique(as.character(data[[input$group_var]]))
      tagList(
        orderInput <- selectInput(
          ns("level_order"),
          "Order of levels (first = reference):",
          choices = group_values,
          selected = group_values,
          multiple = TRUE,
          selectize = TRUE
        )
      )
    })
    
    # -----------------------------------------------------------
    # Run ANOVA
    # -----------------------------------------------------------
    model_fit <- eventReactive(input$run_anova, {
      req(df(), input$response_var, input$group_var)
      data <- df()
      
      # Ensure factor level order
      group <- factor(
        data[[input$group_var]],
        levels = input$level_order
      )
      data[[input$group_var]] <- group
      
      model_formula <- as.formula(paste(input$response_var, "~", input$group_var))
      lm(model_formula, data = data)
      
    })
    
    # -----------------------------------------------------------
    # Show summary(lm)
    # -----------------------------------------------------------
    output$model_summary <- renderPrint({
      req(model_fit())
      summary(model_fit())
    })
    
    # -----------------------------------------------------------
    # Fixed effects table
    # -----------------------------------------------------------
    output$fixed_effects_table <- renderDT({
      req(model_fit())
      coefs <- broom::tidy(model_fit())
      datatable(
        coefs,
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE
      )
    })
    
    # Return model
    return(model_fit)
  })
}
