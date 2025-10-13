library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("🧬 Dr. Nicola Palmieri — Bioinformatics Tools & Apps"),
  
  # --- About section
  fluidRow(
    column(
      3,
      tags$img(src = "my_photo.jpeg", width = "100%", style = "border-radius: 50%; box-shadow: 0 0 8px #aaa;")
    ),
    column(
      9,
      h3("About Me"),
      p("I am a researcher working on bioinformatics, microbial genomics and brain organoids. 
         I build bioinformatics and visualization tools to make data exploration easier for colleagues."),
      tags$ul(
        tags$li("🔬 Fields: Bioinformatics, Genomics, Neural Data Analysis"),
        tags$li("💡 Interests: Evolution, Organoids, R/Python pipelines"),
        tags$li("📧 Contact: ", tags$a(href="mailto:palmierinico@gmail.com", "palmierinico@gmail.com")),
        tags$li("🌍 GitHub: ", tags$a(href="https://github.com/yourname", "github.com/yourname"))
      )
    )
  ),
  hr(),
  
  # --- Apps section
  h3("🧰 My Shiny Apps"),
  fluidRow(
    column(
      4,
      card(
        card_header("Sankey Plot Builder"),
        card_body(
          p("Build interactive Sankey diagrams from metadata tables."),
          tags$a(href="https://yourname.shinyapps.io/sankey_plot_builder", 
                 target="_blank", class="btn btn-primary", "Open App")
        )
      )
    ),
    column(
      4,
      card(
        card_header("Volcano Plot Tool"),
        card_body(
          p("Visualize differential expression results interactively."),
          tags$a(href="https://yourname.shinyapps.io/volcano_plot_tool", 
                 target="_blank", class="btn btn-primary", "Open App")
        )
      )
    ),
    column(
      4,
      card(
        card_header("OrganoViz Explorer"),
        card_body(
          p("Interactive visualization of calcium imaging data from brain organoids."),
          tags$a(href="https://yourname.shinyapps.io/organoviz_explorer", 
                 target="_blank", class="btn btn-primary", "Open App")
        )
      )
    )
  ),
  hr(),
  
  # --- Footer
  tags$footer(
    align = "center",
    style = "margin-top: 40px; color: #777;",
    "© 2025 Your Name — Built with R Shiny"
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
