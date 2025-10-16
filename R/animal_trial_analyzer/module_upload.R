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
    preview_df <- reactiveVal(NULL)
    validation_msg <- reactiveVal("")

    sheet_state <- reactiveValues(
      choices = NULL,
      error = NULL,
      file_info = ""
    )

    output$validation_msg <- renderText({ validation_msg() })

    output$sheet_selector <- renderUI({
      if (!is.null(sheet_state$error)) {
        return(div(class = "text-danger", sheet_state$error))
      }

      if (is.null(sheet_state$choices)) {
        return(p("📄 Upload an Excel file to choose a worksheet."))
      }

      current_choices <- sheet_state$choices
      selectInput(
        ns("sheet"),
        "Select sheet:",
        choices = current_choices,
        selected = isolate({
          if (!is.null(input$sheet) && input$sheet %in% current_choices) {
            input$sheet
          } else {
            current_choices[1]
          }
        })
      )
    })

    output$preview <- DT::renderDT({
      data <- preview_df()
      if (is.null(data)) {
        return(NULL)
      }

      DT::datatable(
        data,
        options = list(scrollX = TRUE, pageLength = 5)
      )
    })

    # ---- STEP 1: after file upload, list sheets (NEVER silent) ----
    observeEvent(input$file, {
      req(input$file)

      fname <- input$file$name
      fpath <- input$file$datapath
      ext   <- tolower(file_ext(fname))

      # Ensure the tempfile is fully written before accessing it
      Sys.sleep(0.2)

      sheet_state$choices <- NULL
      sheet_state$error <- NULL
      preview_df(NULL)
      df(NULL)

      sheet_state$file_info <- paste0(
        "File: ", fname, " | ext: .", ext, "\n",
        "Temp path: ", fpath
      )
      validation_msg(sheet_state$file_info)

      if (!file.exists(fpath)) {
        sheet_state$error <- "❌ Temporary upload not found on disk. Try again."
        validation_msg(paste(sheet_state$file_info, sheet_state$error, sep = "\n"))
        return()
      }

      if (!ext %in% c("xlsx", "xls", "xlsm")) {
        sheet_state$error <- "❌ Not an Excel file (.xlsx/.xls/.xlsm)."
        validation_msg(paste(sheet_state$file_info, sheet_state$error, sep = "\n"))
        return()
      }

      sheets <- tryCatch(excel_sheets(fpath), error = function(e) e)

      if (inherits(sheets, "error")) {
        sheet_state$error <- paste0("❌ Could not read sheets: ", conditionMessage(sheets))
        validation_msg(paste(sheet_state$file_info, sheet_state$error, sep = "\n"))
        return()
      }

      if (length(sheets) == 0) {
        sheet_state$error <- "❌ No sheets found in workbook."
        validation_msg(paste(sheet_state$file_info, sheet_state$error, sep = "\n"))
        return()
      }

      sheet_state$choices <- sheets
    }, ignoreInit = TRUE)

    # ---- STEP 2: load selected sheet ----
    observeEvent({
      list(
        input$sheet,
        input$file,
        sheet_state$choices
      )
    }, {
      req(input$file)

      choices <- sheet_state$choices
      if (is.null(choices) || length(choices) == 0) {
        return()
      }

      sheet <- input$sheet
      if (is.null(sheet) || !sheet %in% choices) {
        sheet <- choices[1]
      }

      tmp <- tryCatch(
        read_excel(input$file$datapath, sheet = sheet),
        error = function(e) e
      )

      if (inherits(tmp, "error")) {
        df(NULL)
        preview_df(NULL)
        validation_msg(paste(sheet_state$file_info, paste("❌ Error loading sheet:", conditionMessage(tmp)), sep = "\n"))
        return()
      }

      tmp <- janitor::clean_names(tmp)
      tmp <- as.data.frame(tmp)

      msg <- validate_long_format(tmp)
      validation_msg(paste(sheet_state$file_info, msg, sep = "\n"))

      df(tmp)
      preview_df(tmp)
    }, ignoreNULL = TRUE)

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
