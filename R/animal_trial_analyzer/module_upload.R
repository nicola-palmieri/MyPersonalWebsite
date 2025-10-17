# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long + flat wide support)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("1. Upload Data (long or flat-wide format)"),
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
      
    output$validation_msg <- renderText({ "" })
    
    # ---- STEP 1: after file upload, list sheets (NEVER silent) ----
    observeEvent(input$file, {
      
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
    observeEvent(list(input$sheet, input$file$datapath), {
      req(input$file, input$sheet)

      tmp <- tryCatch(
        read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) e
      )
      
      # Clean column names immediately after successful load
      if (!inherits(tmp, "error")) {
        tmp <- janitor::clean_names(tmp)
      }
      
      
      if (inherits(tmp, "error")) {
        df(NULL)
        output$validation_msg <- renderText(
          paste("❌ Error loading sheet:", conditionMessage(tmp))
        )
        output$preview <- renderDT(NULL)
        return()
      }
      
      # ---- STEP 3: validate / convert to long format ----
      validation_msg <- validate_long_format(tmp)

      if (!identical(validation_msg, "✅ Data appears to be in long format.")) {
        converted <- tryCatch(convert_flat_wide_to_long(tmp), error = function(e) e)

        if (!inherits(converted, "error")) {
          converted_validation <- validate_long_format(converted)

          if (identical(converted_validation, "✅ Data appears to be in long format.")) {
            tmp <- converted
            validation_msg <- paste(
              "ℹ️ Detected flat wide layout — converted to long format automatically.",
              converted_validation,
              sep = "\n"
            )
          } else {
            validation_msg <- paste(
              "⚠️ Converted from flat wide layout but data still needs attention:",
              converted_validation,
              sep = "\n"
            )
            tmp <- converted
          }
        } else {
          validation_msg <- paste(
            validation_msg,
            paste("ℹ️ Attempted flat wide conversion but failed:", conditionMessage(converted)),
            sep = "\n\n"
          )
        }
      }

      output$validation_msg <- renderText(validation_msg)
      df(tmp)
      
      output$preview <- renderDT(
        tmp,
        options = list(scrollX = TRUE, pageLength = 5)
      )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
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

convert_flat_wide_to_long <- function(data) {
  all_cols <- names(data)
  
  # Detect <prefix>_<number> pattern
  measure_cols <- grep("^[A-Za-z]\\w*_\\d+$", all_cols, value = TRUE)
  if (length(measure_cols) == 0)
    stop("No '<response>_<rep>' columns detected.")
  
  id_cols <- setdiff(all_cols, measure_cols)
  
  # Pivot: .value = response name, Replicate = numeric suffix
  long_df <- tidyr::pivot_longer(
    data,
    cols = dplyr::all_of(measure_cols),
    names_to   = c(".value", "Replicate"),
    names_pattern = "^(.+?)_(\\d+)$"
  )
  
  long_df <- dplyr::mutate(long_df, Replicate = as.integer(Replicate))
  
  # Reorder columns: IDs → Replicate → responses
  id_cols_in_data <- intersect(id_cols, names(long_df))
  response_cols   <- setdiff(names(long_df), c(id_cols_in_data, "Replicate"))
  
  long_df <- dplyr::relocate(long_df,
                             dplyr::all_of(id_cols_in_data),
                             Replicate,
                             dplyr::all_of(response_cols))
  
  long_df
}
