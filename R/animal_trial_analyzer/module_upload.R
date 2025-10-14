# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long-format only)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("1. Upload Data (long format only)"),
    fileInput(ns("file"), "Upload Excel file (.xlsx / .xls / .xlsm)", accept = c(".xlsx", ".xls", ".xlsm")),
    uiOutput(ns("sheet_selector")),     # <- will always render something
    br(),
    verbatimTextOutput(ns("validation_msg")),  # format checks + debug text
    DTOutput(ns("preview"))
  )
}

upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactiveVal(NULL)
    
    # Default UI while waiting
    output$sheet_selector <- renderUI({
      p("📄 Waiting for Excel upload…")
    })
    output$validation_msg <- renderText({ "" })
    
    # ---- STEP 1: after file upload, list sheets (NEVER silent) ----
    observeEvent(input$file, {
      cat(">>> Uploaded file:", input$file$datapath, "\n")
      
      req(input$file)
      # small delay to ensure tempfile is fully written
      Sys.sleep(0.2)
      
      fname <- input$file$name
      fpath <- input$file$datapath
      ext   <- tolower(file_ext(fname))
      
      # Show immediate debug info
      output$validation_msg <- renderText({
        paste0(
          "File: ", fname, " | ext: .", ext, "\n",
          "Temp path: ", fpath
        )
      })
      
      if (!file.exists(fpath)) {
        output$sheet_selector <- renderUI(
          p("❌ Temporary upload not found on disk. Try again.")
        )
        return()
      }
      
      if (!ext %in% c("xlsx","xls","xlsm")) {
        output$sheet_selector <- renderUI(
          p("❌ Not an Excel file (.xlsx/.xls/.xlsm).")
        )
        return()
      }
      
      sheets <- tryCatch(excel_sheets(fpath), error = function(e) e)
      
      if (inherits(sheets, "error")) {
        output$sheet_selector <- renderUI(
          div(
            p("❌ Could not read sheets from file."),
            tags$pre(conditionMessage(sheets))
          )
        )
        return()
      }
      
      if (length(sheets) == 0) {
        output$sheet_selector <- renderUI(p("❌ No sheets found in workbook."))
        return()
      }
      
      # Render the dropdown (always)
      output$sheet_selector <- renderUI({
        selectInput(ns("sheet"), "Select sheet:", choices = sheets, selected = sheets[1])
      })
    }, ignoreInit = TRUE)
    
    # ---- STEP 2: load selected sheet ----
    observeEvent(input$sheet, {
      req(input$file, input$sheet)
      
      tmp <- tryCatch(
        read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) e
      )
      
      if (inherits(tmp, "error")) {
        df(NULL)
        output$validation_msg <- renderText(
          paste("❌ Error loading sheet:", conditionMessage(tmp))
        )
        output$preview <- renderDT(NULL)
        return()
      }
      
      # ---- STEP 3: validate long format ----
      msg <- validate_long_format(tmp)
      output$validation_msg <- renderText(msg)
      df(tmp)
      
      output$preview <- renderDT(
        head(tmp, 10),
        options = list(scrollX = TRUE, pageLength = 5)
      )
    }, ignoreInit = TRUE)
    
    return(df)
  })
}

# ------------------------------
# Validation helper
# ------------------------------
validate_long_format <- function(data) {
  if (!is.data.frame(data))            return("❌ Not a valid table.")
  if (ncol(data) < 3)                  return("⚠️ Expected ≥3 columns (ID, factor, value).")
  if (any(names(data) == "" | is.na(names(data))))
    return("⚠️ Some columns are unnamed — please fix in Excel.")
  
  num_cols <- names(data)[sapply(data, is.numeric)]
  chr_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  
  if (length(num_cols) == 0)           return("⚠️ No numeric columns detected.")
  if (length(chr_cols) < 1)            return("⚠️ No categorical columns detected.")
  
  "✅ Data appears to be in long format."
}
