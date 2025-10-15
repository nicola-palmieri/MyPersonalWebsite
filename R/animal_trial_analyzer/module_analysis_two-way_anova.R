# ===============================================================
# 🧪 Animal Trial Analyzer — Two-way ANOVA Module
# ===============================================================


two_way_anova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("inputs")),
    uiOutput(ns("level_order_1")),
    uiOutput(ns("level_order_2")),
    br(),
    actionButton(ns("run"), "Run Two-way ANOVA"),
    br(), br(),
    verbatimTextOutput(ns("summary")),
    DTOutput(ns("fixed_effects"))
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
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        selectInput(
          ns("response"),
          "Response variable (numeric):",
          choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL
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
    model <- eventReactive(input$run, {
      req(df(), input$response, input$factor1, input$factor2)
      data <- df()
      data[[input$factor1]] <- factor(data[[input$factor1]], levels = input$order1)
      data[[input$factor2]] <- factor(data[[input$factor2]], levels = input$order2)
      model_formula <- as.formula(paste(input$response, "~", input$factor1, "*", input$factor2))
      lm(model_formula, data = data)
    })
    
    # ----------------------------------------------
    # Outputs — consistent with one-way ANOVA
    # ----------------------------------------------
    output$summary <- renderPrint({
      req(model())
      summary(model())
    })
    
    output$fixed_effects <- renderDT({
      req(model())
      datatable(
        broom::tidy(model()),
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE
      )
    })
    
    # Return model reactive for visualization
    return(model)
  })
}
