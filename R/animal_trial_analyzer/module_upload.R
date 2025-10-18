# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long + flat wide support)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload Data"),
      p("Upload your Excel file, choose the worksheet, and follow the guidance before proceeding."),
      hr(),
      radioButtons(
        ns("layout_type"),
        label = "Data layout:",
        choices = c(
          "Long format (one row per measurement)" = "long",
          "GraphPad-Style (two header rows)"     = "graphpad"
        ),
        selected = "long"
      ),
      uiOutput(ns("layout_example")),
      hr(),
      fileInput(
        ns("file"),
        "Upload Excel file (.xlsx / .xls / .xlsm)",
        accept = c(".xlsx", ".xls", ".xlsm")
      ),
      uiOutput(ns("sheet_selector")),
      hr(),
      div(
        class = "d-flex justify-content-end",
        actionButton(ns("go_filter"), "Continue →", class = "btn-primary")
      )
    ),
    mainPanel(
      width = 8,
      h4("Data Preview"),
      verbatimTextOutput(ns("validation_msg")),
      hr(),
      DTOutput(ns("preview"))
    )
  )
}

upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactiveVal(NULL)
    
    # ---- Example layouts ----
    output$layout_example <- renderUI({
      req(input$layout_type)
      
      long_path <- "data/toy_animal_trial_data_long.xlsx"
      graphpad_path <- "data/toy_animal_trial_data_graphpad.xlsx"
      
      if (!file.exists(long_path) || !file.exists(graphpad_path))
        return(p("❌ Example files not found in /data folder."))
      
      if (input$layout_type == "long") {
        toy <- readxl::read_excel(long_path, n_max = 5)
        caption_txt <- paste(
          "Long format — one row per animal × replicate.",
          "Each replicate is a separate row; responses (FAMACHA, BCS, EPG) each have their own column."
        )
      } else {
        toy <- readxl::read_excel(graphpad_path, n_max = 5)
        caption_txt <- paste(
          "GraphPad-style format — two header rows.",
          "First row: response names. Second row: replicate numbers (1, 2, 3...)."
        )
      }
      
      DT::datatable(
        toy,
        caption = htmltools::tags$caption(htmltools::tags$b(caption_txt)),
        elementId = ns("example_dt"),
        options = list(dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    # ---- File upload + sheet selection ----
    observeEvent(input$file, {
      req(input$file)
      fpath <- input$file$datapath
      ext <- tolower(tools::file_ext(input$file$name))
      
      if (!ext %in% c("xlsx", "xls", "xlsm")) {
        output$validation_msg <- renderText("❌ Invalid file type. Please upload .xlsx/.xls/.xlsm.")
        return()
      }
      
      sheets <- tryCatch(readxl::excel_sheets(fpath), error = function(e) NULL)
      if (is.null(sheets) || length(sheets) == 0) {
        output$validation_msg <- renderText("❌ No readable sheets found in the workbook.")
        return()
      }
      
      output$validation_msg <- renderText(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI({
        selectInput(ns("sheet"), "Select sheet:", choices = sheets)
      })
    }, ignoreInit = TRUE)
    
    # ---- Load selected sheet ----
    observeEvent(list(input$sheet, input$file$datapath, input$layout_type), {
      req(input$file, input$sheet)
      
      tmp <- tryCatch(
        readxl::read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) e
      )
      if (inherits(tmp, "error")) {
        output$validation_msg <- renderText(paste("❌ Error loading sheet:", conditionMessage(tmp)))
        return()
      }
      
      if (input$layout_type == "graphpad") {
        # 🧩 Handle two header rows (GraphPad)
        hdr <- readxl::read_excel(input$file$datapath, sheet = input$sheet, n_max = 2, col_names = FALSE)
        
        # fill blanks in first header row
        hdr[1, ] <- zoo::na.locf(hdr[1, ], na.rm = FALSE)
        
        new_names <- mapply(function(a, b) paste0(a, "_", b),
                            hdr[1, ], hdr[2, ], SIMPLIFY = TRUE)
        
        tmp <- readxl::read_excel(input$file$datapath, sheet = input$sheet, skip = 2, col_names = new_names)
        tmp <- convert_flat_wide_to_long(tmp)
        output$validation_msg <- renderText("✅ GraphPad-style format recognized and reshaped.")
      } else {
        output$validation_msg <- renderText("✅ Long format loaded successfully.")
      }
      
      tmp <- janitor::clean_names(tmp)
      df(tmp)
      
      output$preview <- renderDT(tmp, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    return(df)
  })
}
