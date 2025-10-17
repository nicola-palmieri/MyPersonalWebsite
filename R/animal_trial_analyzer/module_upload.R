# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long + flat wide support)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload Data"),
      p("Upload your Excel file in long format, choose the worksheet, and follow the guidance before proceeding."),
      hr(),
      radioButtons(
        ns("layout_type"),
        label = "Data layout:",
        choices = c(
          "Long format (one row per measurement)" = "long",
          "Flat Wide (replicates as suffixes)"   = "flat"
        ),
        selected = "long"
      ),
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
    observeEvent(list(input$sheet, input$file$datapath, input$layout_type), {
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
      
      tmp <- janitor::clean_names(tmp)
      
      # ---- EXPLICIT CONVERSION ----
      if (input$layout_type == "flat") {
        converted <- tryCatch(convert_flat_wide_to_long(tmp), error = function(e) e)
        if (inherits(converted, "error")) {
          output$validation_msg <- renderText(
            paste("❌ Flat-wide conversion failed:", conditionMessage(converted))
          )
          df(NULL)
          return()
        } else {
          tmp <- converted
          output$validation_msg <- renderText("ℹ️ Flat-wide format converted successfully.")
        }
      } else {
        output$validation_msg <- renderText("✅ Long format loaded successfully.")
      }
      
      # optional: normalize column names
      names(tmp) <- tolower(names(tmp))
      names(tmp) <- dplyr::recode(
        names(tmp),
        animal_id = "AnimalID",
        treatment = "Treatment",
        farm = "Farm",
        replicate = "Replicate"
      )
      
      df(tmp)
      
      output$preview <- renderDT(
        tmp,
        options = list(scrollX = TRUE, pageLength = 5)
      )
    })
    
    
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
