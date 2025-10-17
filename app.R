library(shiny)
library(bslib)
library(ggplot2)

options(shiny.autoreload = TRUE)

source("sankey_module.R")
source("animal_trial_analyzer_module.R")

ui <- navbarPage(
  title = "🧬 Dr. Nicola Palmieri",
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  selected = "Animal Trial Analyzer", # TEMPORARILY SELECT THIS APP
  tabPanel(
    title = "Home",
    fluidPage(
      tags$head(
        tags$style(HTML("
        /* Container width and hero tuning */
        .container-fluid { max-width: 1600px; margin: auto; }
        .hero {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9f5ff 100%);
          border-radius: 16px; padding: 20px 24px; /* smaller than before */
        }
        /* Photo cap */
        .profile-pic {
          width: 100%; max-width: 240px; height: auto;
          border-radius: 50%;
          box-shadow: 0 10px 24px rgba(0,0,0,0.12);
        }
        /* Make left column sticky so contacts remain visible on scroll (desktop) */
        @media (min-width: 992px) {
          .sticky-col { position: sticky; top: 84px; }
        }
        /* Cards equal height helpers */
        .equal-card .card-body { display: flex; flex-direction: column; }
        .equal-card ul { margin-bottom: 0; }
        /* Tighten vertical rhythm */
        h1 { margin-top: 0.2rem; margin-bottom: 0.6rem; }
        h3 { margin-top: 0; }
        .section { margin-top: 18px; }
      "))
      ),
      
      # --- HERO: 3 columns (photo+contacts | name+bio | quick links) ---
      tags$section(
        class = "hero",
        fluidRow(
          column(
            width = 3, class = "sticky-col",
            div(class = "d-flex flex-column align-items-center",
                tags$img(src = "my_photo.jpeg", class = "profile-pic")
            ),
            bslib::card(
              class = "mt-3",
              bslib::card_body(
                h5("Contact"),
                tags$ul(class = "list-unstyled mb-2",
                        tags$li(tags$strong("Location:"), " Vienna, Austria"),
                        tags$li(tags$strong("Phone:"), " +43 676 4342779"),
                        tags$li(tags$strong("Email:"), " ",
                                tags$a(href = "mailto:nicola.palmieri@vetmeduni.ac.at",
                                       "nicola.palmieri@vetmeduni.ac.at"))
                )
              )
            )
          ),
          column(
            width = 6,
            h1("Dr. Nicola Palmieri"),
            h4(class = "text-muted",
               "Bioinformatician · Genomics Researcher · Data Scientist"),
            p(
              "I am a Vienna-based researcher exploring genome evolution, microbial pathogenicity, and the genomics of host–pathogen interactions.",
              "My work bridges bioinformatics, molecular biology, and neuroinformatics to uncover actionable insights from complex biological data."
            )
          ),
          column(
            width = 3,
            bslib::card(
              bslib::card_body(
                h5("Links"),
                tags$ul(class = "list-unstyled mb-2",
                        tags$li(tags$strong("ORCID: "), tags$a(href="https://orcid.org/0000-0002-6164-1843/",
                                                               "0000-0002-6164-1843")),
                        tags$li(tags$strong("GitHub: "), tags$a(href="https://github.com/nicola-palmieri",
                                                                "github.com/nicola-palmieri")),
                        tags$li(tags$strong("LinkedIn: "), tags$a(href="https://www.linkedin.com/in/nicola-palmieri-05ab4b144",
                                                                  "linkedin.com/in/nicola-palmieri-05ab4b144"))
                )
              )
            ),
            bslib::card(
              class = "mt-3",
              bslib::card_body(
                h5("Languages"),
                div("Italian · English · German · Spanish · French")
              )
            )
          )
        )
      ),
      
      # --- THREE COLUMNS: Research / Roles / Expertise (equal height cards) ---
      tags$section(
        class = "section",
        bslib::layout_columns(col_widths = c(4,4,4),
                              bslib::card(class = "equal-card",
                                          bslib::card_header("Research Focus"),
                                          bslib::card_body(
                                            tags$ul(
                                              tags$li("Genomics and phylogenomics of pathogenic microorganisms"),
                                              tags$li("Antibiotic resistance and virulence evolution"),
                                              tags$li("Host–pathogen interactions across species"),
                                              tags$li("Neuroinformatics and neural signal analysis")
                                            )
                                          )
                              ),
                              bslib::card(class = "equal-card",
                                          bslib::card_header("Current Roles"),
                                          bslib::card_body(
                                            tags$ul(
                                              tags$li(tags$strong("Data Analyst"), ", a:head Bio, Vienna Biocenter (2021 – Present)"),
                                              tags$li(tags$strong("Bioinformatician"), ", University Clinic for Poultry and Fish Medicine, Vetmeduni Vienna (2017 – Present)"),
                                              tags$li(tags$strong("Postdoctoral Researcher"), ", Institute of Parasitology, Vetmeduni Vienna (2014 – 2018)")
                                            )
                                          )
                              ),
                              bslib::card(class = "equal-card",
                                          bslib::card_header("Core Expertise"),
                                          bslib::card_body(
                                            tags$ul(
                                              tags$li("Genome sequencing, annotation, and comparative genomics"),
                                              tags$li("RNA-Seq, Nanopore, and NGS data analysis"),
                                              tags$li("Neuroinformatics pipelines for calcium imaging and MEA data"),
                                              tags$li("Statistical modeling, machine learning, and data visualization")
                                            )
                                          )
                              )
        )
      ),
      
      # --- PUBLICATIONS in an accordion (collapsible) ---
      tags$section(
        class = "section",
        h3("Selected Publications"),
        bslib::accordion(
          id = "pubs",
          bslib::accordion_panel(
            "Show publications",
            tags$ul(class = "list-unstyled",
                    tags$li(
                      tags$strong("2023 · "),
                      "The Genetic Network Underlying the Evolution of Pathogenicity in Avian Escherichia coli, Frontiers in Veterinary Science. ",
                      tags$a(href="https://www.frontiersin.org/articles/10.3389/fvets.2023.1195585","Read")
                    ),
                    tags$li(
                      tags$strong("2023 · "),
                      "Gene Expression of Peripheral Blood Mononuclear Cells and CD8+ T Cells from Gilts after PRRSV Infection, Frontiers in Immunology. ",
                      tags$a(href="https://www.frontiersin.org/articles/10.3389/fimmu.2023.1159970","Read")
                    ),
                    tags$li(
                      tags$strong("2022 · "),
                      "A Novel Chaphamaparvovirus Is the Etiological Agent of Hepatitis Outbreaks in Pheasants, Transboundary and Emerging Diseases. ",
                      tags$a(href="https://doi.org/10.1111/tbed.14545","Read")
                    ),
                    tags$li(
                      tags$strong("2021 · "),
                      "Complete Genomes of the Eukaryotic Poultry Parasite Histomonas meleagridis, BMC Genomics. ",
                      tags$a(href="https://doi.org/10.1186/s12864-021-08059-2","Read")
                    )
            )
          )
        )
      ),
      
      # --- TEACHING & SOFTWARE as two cards ---
      tags$section(
        class = "section",
        bslib::layout_columns(col_widths = c(6,6),
                              bslib::card(
                                bslib::card_header("Teaching"),
                                bslib::card_body(
                                  p("Doctoral Course Instructor in R and Python Programming, University of Veterinary Medicine Vienna (2009 – 2012)."),
                                  p("Designed and delivered programming curricula for doctoral researchers with a focus on reproducible analytical workflows.")
                                )
                              ),
                              bslib::card(
                                bslib::card_header("Software Development"),
                                bslib::card_body(
                                  p("Proprietary R packages and Shiny applications developer at a:head Bio."),
                                  p("Builds interactive analytics tools, dashboards, and pipelines that accelerate decision-making for translational research teams.")
                                )
                              )
        )
      ),
      
      hr(),
      tags$footer(
        align = "center",
        style = "margin-top: 20px; color: #777;",
        "© 2025 Dr. Nicola Palmieri — Built with R Shiny"
      )
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
    ),
    tabPanel(
      title = "Animal Trial Analyzer",
      fluidPage(
        animal_trial_app_ui("animal_trial")
      )
    )
  )
)

server <- function(input, output, session) {
  sankey_app_server("sankey")
  animal_trial_app_server("animal_trial")
}

shinyApp(ui, server)
