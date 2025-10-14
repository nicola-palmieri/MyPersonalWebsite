#' Sankey diagram builder module
#'
#' Provides UI and server logic for uploading tabular metadata and generating a
#' Sankey diagram. The module supports multiple tabular formats and offers an
#' interactive data preview using DT.
library(DT)
library(readxl)
library(data.table)

# Provide a ready-to-use demo dataset so users see an example Sankey as soon as
# the app loads. The data mimics a simple experimental workflow with three
# categorical stages.
demo_sankey_data <- tibble::tribble(
  ~Species,        ~Condition,       ~Outcome,
  "Chicken",       "Vaccinated",     "Recovered",
  "Chicken",       "Vaccinated",     "Recovered",
  "Chicken",       "Control",        "Sick",
  "Turkey",        "Vaccinated",     "Recovered",
  "Turkey",        "Control",        "Recovered",
  "Turkey",        "Control",        "Sick",
  "Duck",          "Vaccinated",     "Recovered",
  "Duck",          "Control",        "Recovered",
  "Duck",          "Control",        "Recovered",
  "Duck",          "Control",        "Sick"
)

sankey_app_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      tags$div(
        class = "mb-3",
        h4("What this app does"),
        p(
          strong("Aim."),
          " Turn plain tables into interactive Sankey flow diagrams with just a few clicks."
        ),
        p(
          strong("Why it matters."),
          " Researchers and analysts often need to visualise how samples move through experimental stages, yet most Sankey tools require coding knowledge. This interface makes it effortless to build these plots directly from tidy spreadsheets."
        ),
        p(
          strong("How to use it."),
          " Review the demo data that loads automatically, then upload your own table (CSV, TSV, TXT, XLSX) and select the categorical columns in the order you want the flow to appear."
        ),
        tags$ol(
          tags$li("Prepare a table with at least two categorical columns (e.g., Species → Condition → Outcome)."),
          tags$li("Upload the file or keep the demo data to experiment."),
          tags$li("Adjust the column order, preview the table, and download the Sankey as PNG or PDF."),
          tags$li("Need a fresh start? Simply upload a new dataset—the demo will always be available on reload.")
        )
      ),
      fileInput(ns("file"), "Upload metadata file"),
      uiOutput(ns("col_select")),
      numericInput(ns("width"), "Plot width (inches)", 7, min = 3, max = 20),
      numericInput(ns("height"), "Plot height (inches)", 5, min = 3, max = 20),
      selectInput(ns("format"), "Download format", choices = c("png", "pdf")),
      actionButton(ns("go"), "Generate Sankey"),
      downloadButton(ns("download_plot"), "Download Sankey Plot")
    ),
    mainPanel(
      tags$div(
        class = "mb-4",
        h3("Explore the demo Sankey"),
        p(
          "The app opens with a toy dataset showing animal species moving through a vaccination trial and their outcomes.",
          " Use it as a template to understand the required structure before replacing it with your own data."
        ),
        p(
          "Once you upload a file, the preview and plot update instantly."
        )
      ),
      h4("Data Preview"),
      DTOutput(ns("preview")),
      hr(),
      networkD3::sankeyNetworkOutput(ns("sankey"), height = "600px")
    )
  )
}

sankey_app_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      if (is.null(input$file)) {
        return(demo_sankey_data)
      }

      ext <- tolower(tools::file_ext(input$file$name))

      df <- tryCatch({
        switch(
          ext,
          csv = readr::read_csv(input$file$datapath, show_col_types = FALSE),
          tsv = readr::read_tsv(input$file$datapath, show_col_types = FALSE),
          txt = readr::read_delim(
            input$file$datapath,
            delim = NULL,
            show_col_types = FALSE
          ),
          xlsx = readxl::read_excel(input$file$datapath),
          validate(need(FALSE, paste0("Unsupported file type: .", ext)))
        )
      }, error = function(e) {
        validate(need(FALSE, "Unable to read the uploaded file. Please check the format and try again."))
      })

      df <- tibble::as_tibble(df)
      validate(need(ncol(df) >= 2, "Upload a table with at least two categorical columns — e.g., sample metadata or factors."))
      df
    })

    output$preview <- DT::renderDataTable({
      req(data())
      DT::datatable(
        data(),
        options = list(
          pageLength = 25,
          lengthChange = FALSE,
          scrollY = "400px",
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    output$col_select <- renderUI({
      selectizeInput(
        session$ns("cols"),
        "Select columns for Sankey (in order)",
        choices = NULL,
        multiple = TRUE
      )
    })

    observeEvent(data(), {
      cols <- names(data())
      default_selection <- cols[seq_len(min(3, length(cols)))]
      updateSelectizeInput(
        session,
        "cols",
        choices = cols,
        selected = default_selection,
        server = TRUE
      )
    }, ignoreNULL = FALSE)

    edges <- eventReactive({
      input$go
      data()
      input$cols
    }, {
      req(data(), input$cols)
      req(length(input$cols) >= 2)
      df <- data()[, input$cols, drop = FALSE]

      edges_list <- vector("list", length(input$cols) - 1)

      for (i in seq_len(length(input$cols) - 1)) {
        col1 <- input$cols[i]
        col2 <- input$cols[i + 1]
        
        tmp <- df |>
          dplyr::count(
            .data[[col1]],
            .data[[col2]],
            name = "value"
          ) |>
          dplyr::rename(
            source = 1,
            target = 2
          ) |>
          dplyr::mutate(
            dplyr::across(c(source, target), as.character)
          )
        
        edges_list[[i]] <- tmp
      }

      dplyr::bind_rows(edges_list)
    }, ignoreNULL = FALSE)


    nodes <- reactive({
      req(edges())
      tibble::tibble(name = unique(c(edges()$source, edges()$target)))
    })

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
        Value = "value",
        NodeID = "name",
        fontSize = 12,
        nodeWidth = 30
      )
    })

    output$sankey <- networkD3::renderSankeyNetwork({
      sankey_obj()
    })

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("sankey_plot.", input$format)
      },
      content = function(file) {
        tmp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(sankey_obj(), tmp_html, selfcontained = TRUE)
        if (input$format == "png") {
          webshot2::webshot(
            url = tmp_html,
            file = file,
            vwidth = input$width * 96,
            vheight = input$height * 96,
            zoom = 300 / 96
          )
        } else if (input$format == "pdf") {
          webshot2::webshot(
            url = tmp_html,
            file = file,
            vwidth = input$width * 96,
            vheight = input$height * 96,
            zoom = 300 / 96
          )
        }
      }
    )
  })
}
