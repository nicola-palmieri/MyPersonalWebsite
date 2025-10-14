# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA
# ===============================================================
library(shiny)
library(DT)
library(broom)

one_way_anova_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("inputs")),
    uiOutput(ns("level_order")),
    br(),
    actionButton(ns("run"), "Run ANOVA"),
    br(), br(),
    verbatimTextOutput(ns("summary")),
    DTOutput(ns("fixed_effects"))
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
        selectInput(
          ns("response"),
          "Response variable (numeric):",
          choices = num_cols,
          selected = if (length(num_cols) > 0) num_cols[1] else NULL
        ),
        selectInput(
          ns("group"),
          "Grouping variable (factor):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
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
    
    # Fit model
    model <- eventReactive(input$run, {
      req(df(), input$response, input$group)
      data <- df()
      data[[input$group]] <- factor(data[[input$group]], levels = input$order)
      model_formula <- as.formula(paste(input$response, "~", input$group))
      lm(model_formula, data = data)
    })
    
    # Outputs
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
    
    return(model)
  })
}
