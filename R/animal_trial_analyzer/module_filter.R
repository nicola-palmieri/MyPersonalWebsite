# ===============================================================
# 🧪 Animal Trial Analyzer — Filter Module
# ===============================================================
filter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 2 — Filter Records"),
      p("Pick the columns to focus on and adjust the filters to refine the dataset for analysis."),
      hr(),
      uiOutput(ns("column_selector")),
      hr(),
      uiOutput(ns("filter_widgets")),
      hr(),
      div(
        class = "d-flex justify-content-between gap-2",
        actionButton(ns("back_upload"), "← Back"),
        actionButton(ns("go_analysis"), "Continue →", class = "btn-primary")
      )
    ),
    mainPanel(
      width = 8,
      h4("Filtered Data Preview"),
      DT::DTOutput(ns("filtered_preview"))
    )
  )
}

filter_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(uploaded_data())
      uploaded_data()
    })
    
    # --- 1. Column selector
    output$column_selector <- renderUI({
      req(df())
      selectInput(
        ns("columns"),
        "Select columns to filter:",
        choices = names(df()),
        multiple = TRUE,
        selectize = TRUE
      )
    })
    
    # --- 2. Dynamic widgets
    output$filter_widgets <- renderUI({
      req(df())
      cols <- input$columns
      isolate(updateSelectInput(session, "columns", selected = cols))
      if (length(cols) == 0) return(NULL)
      
      widgets <- lapply(cols, function(col) {
        col_data <- df()[[col]]
        
        if (is.numeric(col_data)) {
          rng <- range(col_data, na.rm = TRUE)
          fluidRow(
            column(
              width = 6,
              numericInput(
                ns(paste0("min_", col)),
                label = paste(col, "(min)"),
                value = rng[1],
                min = rng[1],
                max = rng[2],
                step = diff(rng) / 100
              )
            ),
            column(
              width = 6,
              numericInput(
                ns(paste0("max_", col)),
                label = paste(col, "(max)"),
                value = rng[2],
                min = rng[1],
                max = rng[2],
                step = diff(rng) / 100
              )
            )
          )
        } else if (is.logical(col_data)) {
          checkboxGroupInput(
            ns(paste0("filter_", col)),
            label = col,
            choices = c(TRUE, FALSE),
            selected = c(TRUE, FALSE),
            inline = TRUE
          )
        } else {
          choices <- sort(unique(as.character(col_data)))
          selectInput(
            ns(paste0("filter_", col)),
            label = col,
            choices = choices,
            multiple = TRUE,
            selected = choices
          )
        }
      })
      do.call(tagList, widgets)
    })
    
    # --- 3. Reactive filtering (auto-updates)
    filtered_df <- reactive({
      req(df())
      data <- df()
      cols <- input$columns
      if (is.null(cols) || length(cols) == 0) return(data)
      
      for (col in cols) {
        col_data <- data[[col]]
        
        if (is.numeric(col_data)) {
          min_val <- input[[paste0("min_", col)]]
          max_val <- input[[paste0("max_", col)]]
          # numeric inputs should always exist; but if they don't, yield 0 rows
          if (is.null(min_val) || is.null(max_val)) {
            data <- data[0, , drop = FALSE]; break
          }
          data <- data[data[[col]] >= min_val & data[[col]] <= max_val, , drop = FALSE]
          
        } else {
          sel <- input[[paste0("filter_", col)]]
          # 👇 KEY CHANGE: empty selection => zero rows
          if (is.null(sel) || length(sel) == 0) {
            data <- data[0, , drop = FALSE]; break
          }
          data <- data[data[[col]] %in% sel, , drop = FALSE]
        }
      }
      
      data
    })
    
    
    # --- 4. Preview
    output$filtered_preview <- DT::renderDT({
      req(filtered_df())
      DT::datatable(filtered_df(),
                options = list(scrollX = TRUE, pageLength = 5))
    })
    
    # Return
    return(filtered_df)
  })
}
