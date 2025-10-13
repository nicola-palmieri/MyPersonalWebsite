library(shiny)
library(bslib)

source("sankey_module.R")

ui <- navbarPage(
  title = "🧬 Dr. Nicola Palmieri — Bioinformatics Tools & Apps",
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  tabPanel(
    title = "Home",
    fluidRow(
      column(
        3,
        tags$img(
          src = "my_photo.jpeg",
          width = "100%",
          style = "border-radius: 50%; box-shadow: 0 0 8px #aaa;"
        )
      ),
      column(
        9,
        h3("About Me"),
        p(
          "I am a researcher working on bioinformatics, microbial genomics and brain organoids.",
          "I build bioinformatics and visualization tools to make data exploration easier for colleagues."
        ),
        tags$ul(
          tags$li("🔬 Fields: Bioinformatics, Genomics, Neural Data Analysis"),
          tags$li("💡 Interests: Evolution, Organoids, R/Python pipelines"),
          tags$li("📧 Contact: ", tags$a(href = "mailto:palmierinico@gmail.com", "palmierinico@gmail.com")),
          tags$li("🌍 GitHub: ", tags$a(href = "https://github.com/nicola-palmieri", "github.com/nicola-palmieri"))
        )
      )
    ),
    hr(),
    tags$footer(
      align = "center",
      style = "margin-top: 40px; color: #777;",
      "© 2025 Your Name — Built with R Shiny"
    )
  ),
  navbarMenu(
    title = "Apps",
    tabPanel(
      title = "Sankey Plot Builder",
      fluidPage(
        h3("Sankey Plot Builder"),
        sankey_app_ui("sankey")
      )
    )
  )
)

server <- function(input, output, session) {
  sankey_app_server("sankey")
}

shinyApp(ui, server)
