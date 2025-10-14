#' Sankey diagram builder module
#'
#' Provides UI and server logic for uploading tabular metadata and generating a
#' Sankey diagram. The module supports multiple tabular formats and offers an
#' interactive data preview using DT.

library(DT)
library(readxl)
library(data.table)

# --- Demo dataset ---
demo_sankey_data <- tibble::tribble(
  ~Species,  ~Condition,  ~Outcome,
  "Chicken", "Vaccinated", "Recovered",
  "Chicken", "Vaccinated", "Recovered",
  "Chicken", "Control",    "Sick",
  "Turkey",  "Vaccinated", "Recovered",
  "Turkey",  "Control",    "Recovered",
  "Turkey",  "Control",    "Sick",
  "Duck",    "Vaccinated", "Recovered",
  "Duck",    "Control",    "Recovered",
  "Duck",    "Control",    "Sick"
)

# --- UI ---
sankey_app_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      tags$div(
        h5("Create a Sankey flow diagram directly from a simple table — no coding required.")
      ),
      tags$hr(),
      h4("Aim"),
      p("Turn plain tabular data into clear, interactive flow diagrams that visualize how items move or are distributed across categories."),
      h4("Why it’s useful"),
      p("Most tools for creating Sankey plots require programming knowledge or complex setup. This app makes it effortless — you only need a spreadsheet with categorical columns."),
      h4("How to use"),
      tags$ol(
        tags$li("Review the demo dataset and its Sankey plot (loaded automatically)."),
        tags$li("Upload your own table (formats: CSV, TSV, TXT, or XLSX)."),
        tags$li("Select which columns should define the flow (in order)."),
        tags$li("The Sankey updates automatically — no coding, no scripts.")
      ),
      tags$hr(),
      fileInput(ns("file"), "Upload your table"),
      uiOutput(ns("col_select")),
      numericInput(ns("width"), "Plot width (inches)", 7, min = 3, max = 20),
      numericInput(ns("height"), "Plot height (inches)", 5, min = 3, max = 20),
      selectInput(ns("format"), "Download format", choices = c("png", "pdf")),
      downloadButton(ns("download_plot"), "Download Sankey Plot")
    ),
    mainPanel(
      h4("Preview of Your Data"),
      DTOutput(ns("preview")),
      hr(),
      h4("Sankey Plot"),
      p("Each arrow shows how many items flow between categories. The width of a link corresponds to its value."),
      networkD3::sankeyNetworkOutput(ns("sankey"), height = "600px")
    )
  )
}

# --- SERVER ---
sankey_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data (demo by default)
    data <- reactive({
      if (is.null(input$file)) {
        return(demo_sankey_data)
      }
      
      ext <- tolower(tools::file_ext(input$file$name))
      df <- tryCatch({
        switch(
          ext,
          csv  = readr::read_csv(input$file$datapath, show_col_types = FALSE),
          tsv  = readr::read_tsv(input$file$datapath, show_col_types = FALSE),
          txt  = readr::read_delim(input$file$datapath, delim = NULL, show_col_types = FALSE),
          xlsx = readxl::read_excel(input$file$datapath),
          validate(need(FALSE, paste0("Unsupported file type: .", ext)))
        )
      }, error = function(e) {
        validate(need(FALSE, "Unable to read the uploaded file. Please check the format."))
      })
      
      df <- tibble::as_tibble(df)
      validate(need(ncol(df) >= 2, "Upload a table with at least two categorical columns."))
      df
    })
    
    # Data preview
    output$preview <- renderDT({
      req(data())
      datatable(
        data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Column selector
    output$col_select <- renderUI({
      req(data())
      cols <- names(data())
      
      # If using demo dataset → preselect first 3 columns
      # If user uploaded a file → no preselection
      default_selection <- if (is.null(input$file)) {
        cols[seq_len(min(3, length(cols)))]
      } else {
        NULL
      }
      
      selectizeInput(
        session$ns("cols"),
        "Select columns for Sankey (in order)",
        choices = cols,
        selected = default_selection,
        multiple = TRUE
      )
    })
    
    
    # Build edges dynamically (auto-updating, no button)
    edges <- reactive({
      req(data(), input$cols)
      req(length(input$cols) >= 2)
      df <- data()[, input$cols, drop = FALSE]
      
      edges_list <- vector("list", length(input$cols) - 1)
      for (i in seq_len(length(input$cols) - 1)) {
        col1 <- input$cols[i]
        col2 <- input$cols[i + 1]
        tmp <- df |>
          dplyr::count(.data[[col1]], .data[[col2]], name = "value") |>
          dplyr::rename(source = 1, target = 2) |>
          dplyr::mutate(dplyr::across(c(source, target), as.character))
        edges_list[[i]] <- tmp
      }
      dplyr::bind_rows(edges_list)
    })
    
    # Build nodes
    nodes <- reactive({
      req(edges())
      tibble::tibble(name = unique(c(edges()$source, edges()$target)))
    })
    
    # Create Sankey object
    sankey_obj <- reactive({
      req(edges(), nodes())
      links <- edges() |>
        dplyr::mutate(
          source = match(source, nodes()$name) - 1,
          target = match(target, nodes()$name) - 1
        )
      
      networkD3::sankeyNetwork(
        Links = links,
        Nodes = nodes(),
        Source = "source",
        Target = "target",
        Value  = "value",
        NodeID = "name",
        fontSize = 12,
        nodeWidth = 30
      )
    })
    
    # Render Sankey plot
    output$sankey <- networkD3::renderSankeyNetwork({
      sankey_obj()
    })
    
    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("sankey_plot.", input$format)
      },
      content = function(file) {
        tmp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(sankey_obj(), tmp_html, selfcontained = TRUE)
        webshot2::webshot(
          url = tmp_html,
          file = file,
          vwidth = input$width * 96,
          vheight = input$height * 96,
          zoom = 300 / 96
        )
      }
    )
  })
}
