# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA
# ===============================================================
library(shiny)
library(DT)
library(broom)

one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      br(),
      actionButton(ns("run"), "Run ANOVA")
    ),
    results = tagList(
      uiOutput(ns("summary_ui")),
      br(),
      uiOutput(ns("fixed_effects_ui"))
    )
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
        # --- checkbox always exists ---
        checkboxInput(ns("multi_resp"), "Enable multiple response variables", value = FALSE),
        
        # --- placeholder for response selector ---
        uiOutput(ns("response_selector")),
        
        selectInput(
          ns("group"),
          "Grouping variable (factor):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    # --- render response selector reactively ---
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
    
    # Fit model(s)
    models <- eventReactive(input$run, {
      req(df(), input$response, input$group, input$order)
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
      data[[input$group]] <- factor(data[[input$group]], levels = input$order)

      model_list <- list()
      for (resp in responses) {
        model_formula <- as.formula(paste(resp, "~", input$group))
        model_list[[resp]] <- lm(model_formula, data = data)
      }

      list(
        models = model_list,
        responses = responses,
        factors = list(factor1 = input$group, factor2 = NULL),
        orders = list(order1 = input$order, order2 = NULL)
      )
    })

    # Summary outputs
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

    # Fixed effects outputs
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
