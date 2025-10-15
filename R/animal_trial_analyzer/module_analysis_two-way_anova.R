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

      model_list <- list()
      for (resp in responses) {
        model_formula <- as.formula(paste(resp, "~", input$factor1, "*", input$factor2))
        model_list[[resp]] <- lm(model_formula, data = data)
      }

      list(
        models = model_list,
        responses = responses,
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

      if (length(responses) == 1) {
        verbatimTextOutput(ns("summary_single"))
      } else {
        tabs <- lapply(seq_along(responses), function(i) {
          tabPanel(responses[i], verbatimTextOutput(ns(paste0("summary_", i))))
        })
        do.call(tabsetPanel, c(list(id = ns("summary_tabs")), tabs))
      }
    })

    output$fixed_effects_ui <- renderUI({
      model_info <- models()
      if (is.null(model_info)) {
        return(NULL)
      }

      responses <- model_info$responses

      if (length(responses) == 1) {
        DTOutput(ns("fixed_effects_single"))
      } else {
        tabs <- lapply(seq_along(responses), function(i) {
          tabPanel(responses[i], DTOutput(ns(paste0("fixed_effects_", i))))
        })
        do.call(tabsetPanel, c(list(id = ns("fixed_effects_tabs")), tabs))
      }
    })

    observeEvent(models(), {
      model_info <- models()
      if (is.null(model_info)) {
        return()
      }

      responses <- model_info$responses
      model_list <- model_info$models

      if (length(responses) == 1) {
        resp <- responses[1]
        output$summary_single <- renderPrint({
          summary(model_list[[resp]])
        })
        output$fixed_effects_single <- renderDT({
          datatable(
            broom::tidy(model_list[[resp]]),
            options = list(scrollX = TRUE, pageLength = 5),
            rownames = FALSE
          )
        })
      } else {
        for (i in seq_along(responses)) {
          resp <- responses[i]
          local({
            idx <- i
            response_name <- resp
            output[[paste0("summary_", idx)]] <- renderPrint({
              summary(model_list[[response_name]])
            })
            output[[paste0("fixed_effects_", idx)]] <- renderDT({
              datatable(
                broom::tidy(model_list[[response_name]]),
                options = list(scrollX = TRUE, pageLength = 5),
                rownames = FALSE
              )
            })
          })
        }
      }
    })

    return(models)
  })
}
