# ===============================================================
# 🧮 Animal Trial Analyzer — Linear Model (LM) Module
# ===============================================================

source("R/animal_trial_analyzer/module_analysis_lm_helpers.R")

lm_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      h4("Linear Model (LM) Configuration"),
      uiOutput(ns("variable_selectors")),
      uiOutput(ns("interaction_select")),  # <-- new dynamic checkbox panel
      hr(),
      uiOutput(ns("formula_preview")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run Linear Model", class = "btn-primary", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download Results", class = "btn-default", width = "100%"))
      )
    ),
    results = tagList(
      verbatimTextOutput(ns("full_summary")),
      h5("Diagnostics"),
      fluidRow(
        column(6, plotOutput(ns("resid_plot"))),
        column(6, plotOutput(ns("qq_plot")))
      )
    )
  )
}

lm_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selectors <- renderUI({
      req(data())
      df <- data()
      num_vars <- names(df)[sapply(df, is.numeric)]
      fac_vars <- names(df)[sapply(df, is.factor) | sapply(df, is.character)]
      
      tagList(
        selectInput(ns("dep"), "Dependent variable:", choices = num_vars),
        selectInput(ns("fixed"), "Fixed factors:", choices = fac_vars, multiple = TRUE),
        selectInput(ns("covar"), "Covariates (optional):", choices = num_vars, multiple = TRUE)
      )
    })
    
    # --- Dynamic list of possible 2-way interactions ---
    output$interaction_select <- renderUI({
      req(input$fixed)
      if (length(input$fixed) < 2) return(NULL)
      
      pairs <- combn(input$fixed, 2, simplify = FALSE)
      pair_labels <- sapply(pairs, function(p) paste(p, collapse = " × "))
      pair_values <- sapply(pairs, function(p) paste(p, collapse = ":"))
      
      checkboxGroupInput(
        ns("interactions"),
        label = "Add 2-way interactions (optional):",
        choices = setNames(pair_values, pair_labels)
      )
    })
    
    # --- Formula preview ---
    output$formula_preview <- renderUI({
      req(input$dep)
      if (is.null(input$fixed) || length(input$fixed) == 0) return(NULL)
      
      rhs <- input$fixed
      if (!is.null(input$covar) && length(input$covar) > 0) {
        rhs <- c(rhs, input$covar)
      }
      if (!is.null(input$interactions) && length(input$interactions) > 0) {
        rhs <- c(rhs, input$interactions)
      }
      
      formula_text <- paste(input$dep, "~", paste(rhs, collapse = " + "))
      
      wellPanel(
        strong("Model formula preview:"),
        code(formula_text)
      )
    })
    
    model <- eventReactive(input$run, {
      req(data(), input$dep, input$fixed)
      
      rhs <- input$fixed
      if (!is.null(input$covar) && length(input$covar) > 0) {
        rhs <- c(rhs, input$covar)
      }
      if (!is.null(input$interactions) && length(input$interactions) > 0) {
        rhs <- c(rhs, input$interactions)
      }
      
      formula <- as.formula(paste(input$dep, "~", paste(rhs, collapse = " + ")))
      lm(formula, data = data())
    })
    
    output$full_summary <- renderPrint({
      req(model())
      
      # --- ANOVA (Type III) ---
      cat("ANOVA (Type III)\n================\n")
      anova_out <- capture.output(car::Anova(model(), type = 3))
      # Remove redundant first line
      if (length(anova_out) > 0 && grepl("^Anova Table", anova_out[1])) {
        anova_out <- anova_out[-1]
      }
      cat(paste(anova_out, collapse = "\n"))
      
      # --- Model summary ---
      cat("\n\nModel summary\n================\n")
      summary_out <- capture.output(summary(model()))
      # Remove 'Call:' section and following formula line
      call_idx <- grep("^Call:", summary_out)
      if (length(call_idx) > 0) {
        summary_out <- summary_out[-c(call_idx, call_idx + 1)]
      }
      cat(paste(summary_out, collapse = "\n"))
    })
    
    
    output$resid_plot <- renderPlot({
      req(model())
      plot(model(), which = 1)
    })
    
    output$qq_plot <- renderPlot({
      req(model())
      plot(model(), which = 2)
    })
    
    output$download_model <- downloadHandler(
      filename = function() paste0("lm_summary_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        write_lm_docx(model(), file)
      }
    )
    
    return(model)
  })
}
