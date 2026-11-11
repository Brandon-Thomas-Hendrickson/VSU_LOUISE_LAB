library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.extras)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Bird Observations. Lake Louise and Valdosta State University"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Diversity Analysis", tabName = "diversity", icon = icon("chart-bar")),
      menuItem("iNaturalist Comparison", tabName = "inaturalist", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #079ef6ff;
        }
      ")),
      # Remove D3.js references
      tags$script(HTML("
        // Simple document ready logging
        $(document).ready(function() {
          console.log('BirdLab App loaded successfully');
        });
      "))
    ),
    
    tabItems(
      tabItem(tabName = "intro",
        fluidRow(
          box(
            title = "Bird Observations Analysis Portal", status = "primary", solidHeader = TRUE,
            width = 12,
            h3("Analyze Bird Biodiversity at Lake Louise, Georgia, and Valdosta State University, Georgia."),
            br(),
            div(
              style = "text-align: center; margin: 30px 0;",
              img(
                src = "martin_pic.jpeg",
                style = "max-width: 100%; max-height: 500px; border-radius: 12px; box-shadow: 0 8px 25px rgba(0,0,0,0.15);",
                alt = "Lake Louise"
              ),
              br(), br(),
              h4("Lake Louise, Georgia", style = "color: #2c3e50; margin: 15px 0 5px 0;"),
                p(
                tags$i("Ardea herodias"),
                " amongst Bald Cyprus Trees (",
                tags$i("Taxodium distichum"),
                ")", 
                style = "color: #7f8c8d; font-style: italic; margin: 0;"
                )
            ),
            br(),
            h4("To-Do List for Lab 2: Bird Observation:"),
            tags$ol(
              tags$li(strong("Upload Data:"), " Import your bird survey data with sites as rows and species as columns"),
              tags$li(strong("Diversity Analysis:"), " Calculate Shannon diversity, Simpson's index, and alpha diversity"),
              tags$li(strong("iNaturalist Comparison:"), " Compare your data with recent bird observations from iNaturalist"),
              tags$li(strong("Sampling Curve:"), " Visualize species discovery over time"),
              tags$li(strong("Next Steps:"), " Assignment guidelines for students")
            ),
            br(),
            p(strong("About Lake Louise and Valdosta State University:"), "Lake Louise Field Station, a Valdosta State University research and teaching site in Lowndes County, is centered on a small blackwater lake and surrounded by wetlands and forest. Its mix of open water, wetland edges, and wooded habitat offers opportunities to study birds that use these environments, including waterbirds, wading birds, raptors, and woodland species. The station’s boardwalk and field facilities support student research, classes, and outreach.

Valdosta State University’s main campus sits within the developed city of Valdosta in South Georgia’s Coastal Plain. Campus buildings, roads, parking areas, lawns, and landscaped trees create a more urban habitat than the natural areas at Lake Louise. Birds commonly associated with developed settings—such as pigeons, European Starlings, House Sparrows, and Northern Mockingbirds—may be encountered alongside species that use campus trees and green spaces, including cardinals, Carolina Wrens, and Blue Jays.

Together, the campus and Lake Louise provide contrasting settings for observing how bird communities vary with habitat. Students can compare birds using an urban campus with those found around a lake, wetlands, and forest, connecting field observations to questions about habitat use and wildlife in human-modified landscapes."),
br(),
            h4("Overview of Biodiversity Measures:"),
            div(style = "margin-left: 20px;",
              h5("Shannon Diversity Index (H')"),
              p("Measures both species richness and evenness. Richness is the number of different species present, while evenness considers how evenly individuals are distributed among those species. Shannon's diversity is based upon the concept of information entropy, which quantifies the uncertainty in predicting the species identity of an individual randomly sampled from the community. The paper can be found at Shannon, C.E. (1948) A Mathematical Theory of Communication. It is very interesting because greater diversity as evenness and richness leads to greater unpredictability. Imagine a sack of marbles. If you had one sack with 99 black marbles, and 1 red marble, you'd have 2 species but a much more predictbale outcome than if you had 50 black marbles and 50 red marbles. The predictability is dependent upon evenness, or how information is distributed. Just extrapolate, and you'd have less predictive power if there were three species with 33 balls each."),
              div(style = "font-family: monospace; margin: 10px 0; padding: 8px; border-left: 3px solid #11181cff;",
                "H' = -Σ (p", tags$sub("i"), " × log(p", tags$sub("i"), "))"
              ),
              p(style = "font-size: 14px; color: #666;", "where p", tags$sub("i"), " is the proportion of individuals belonging to species i."),
              
              br(),
              h5("Simpson's Index (D)"),
              p("Emphasizes dominant species in the community. It is calculated as the probability that two individuals randomly selected from a sample will belong to the same species. The paper can be found at Simpson, E.H. (1949) Measurement of Diversity. It is interesting because it highlights the role of common species in maintaining overall diversity. If you have a community dominated by one species, the probability of picking that species twice is high, leading to a low diversity score. Similar conceptual framework as Shannon's Index."),
              div(style = "font-family: monospace; margin: 10px 0; padding: 8px; border-left: 3px solid #0c0807ff;",
                "D = Σ (n", tags$sub("i"), " × (n", tags$sub("i"), " - 1)) / (N × (N - 1))"
              ),
              p(style = "font-size: 14px; color: #666;", "where n", tags$sub("i"), " is the number of individuals of species i and N is the total number of individuals in the sample."),
              
              br(),
              h5("Alpha Diversity (α)"),
              p("Simple species richness (number of species per site). It is calculated as the total number of unique species present in a sample. The purpose of Alpha Diversity provides a basic measure of biodiversity within a specific area or ecosystem.")
            )
          )
        )
      ),
      
      # Upload Data Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(
            title = "Data Upload", status = "primary", solidHeader = TRUE,
            width = 12,
            h4("Upload your bird survey data"),
            p("Your data should be formatted with:"),
            tags$ul(
              tags$li("Rows representing different sites"),
              tags$li("Columns representing different species"),
              tags$li("Values as positive integers (0 to infinity) representing abundance counts"),
              tags$li("Compatible with the vegan package for ecological analysis")
            ),
            br(),
            fileInput("file1", "Choose CSV File",
                     accept = c(".csv")),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                        selected = ","),
            br(),
            h4("Data Preview:"),
            DT::dataTableOutput("data_preview")
          )
        )
      ),
      
      tabItem(tabName = "diversity",
        fluidRow(
          box(
            title = "Diversity Indices", status = "success", solidHeader = TRUE,
            width = 12,
            conditionalPanel(
              condition = "output.data_uploaded",
              h4("Calculated Diversity Indices:"),
              DT::dataTableOutput("diversity_table"),
              br(),
              fluidRow(
                column(4,
                  h4("Visualization Options:"),
                  selectInput("plot_type", "Choose Plot Type:",
                    choices = list(
                      "Bar Chart - Diversity Indices" = "bar_diversity",
                      "Box Plot - Species Abundance" = "boxplot_abundance",
                      "Scatter Plot - Richness vs Abundance" = "scatter_rich_abund",
                      "Heatmap - Species by Site" = "heatmap_species",
                      "Species Accumulation Curve" = "species_accumulation",
                      "Rank Abundance Curve" = "rank_abundance"
                    ),
                    selected = "bar_diversity"
                  ),
                  br(),
                  h5("Code:"),
                  verbatimTextOutput("plot_code")
                ),
                column(8,
                  h4("Diversity Visualization:"),
                  plotlyOutput("diversity_plot")
                )
              )
            ),
            conditionalPanel(
              condition = "!output.data_uploaded",
              div(
                style = "text-align: center; padding: 40px;",
                icon("upload", style = "font-size: 48px; color: #ccc;"),
                br(), br(),
                h4("Please upload data first", style = "color: #999;")
              )
            )
          )
        )
      ),
      
      # iNaturalist Tab
      tabItem(tabName = "inaturalist",
        fluidRow(
          box(
            title = "iNaturalist Data Comparison", status = "info", solidHeader = TRUE,
            width = 12,
            h4("Find Bird Data"),
            
            # Help text for API limitations
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h5("Tips for Successful Data Retrieval:", style = "color: #101411ff; margin-bottom: 10px;"),
              tags$ul(
                tags$li("Start with a small rectangular area (e.g., a few miles across)"),
                tags$li("Urban areas and popular birding spots may have more data: Try Valdosta State University to Start."),
                tags$li("Research-grade observations only are included for reliability.")
              ),
            ),
            
            # Map-based search only
            fluidRow(
              column(9,
                h5("Draw a rectangle on the map to define your search area:"),
                leafletOutput("search_area_map", height = "400px")
              ),
              column(3,
                br(),
                h6("Search covers the past year (365 days)", 
                   style = "color: #666; margin-bottom: 20px;"),
                actionButton("fetch_data_map", "Fetch Data", class = "btn-info"),
                br(), br(),
                br(), br(),
                div(style = "font-size: 12px; color: #0c0a0aff;",
                  h6("How to use:", style = "font-weight: bold;"),
                  p("1. Click the rectangle tool (⬜) in the map toolbar"),
                  p("2. Draw a rectangle over your area of interest"),
                  p("3. Click 'Fetch Data' to search within that area"),
                  p("4. Use the trash icon to clear your selection")
                ),
                br(),
                div(style = "background-color: #f9f8f4ff; padding: 10px; border-radius: 3px; font-size: 11px;",
                  strong("Troubleshooting:"),
                  br(),
                  "• No data found? → Try larger area or different region",
                  br(),
                  "• Slow loading? → The app is likely processing a large request"
                )
              )
            ),
            br(),
            conditionalPanel(
              condition = "output.inaturalist_fetched",
              fluidRow(
                column(4,
                  h4("iNaturalist Results:"),
                  verbatimTextOutput("inaturalist_summary"),
                  br(),
                  downloadButton("download_inaturalist", "Download Data as CSV", 
                               class = "btn-success", style = "margin-bottom: 15px;"),
                  br(),
                  h4("Visualization Options:"),
                  selectInput("inaturalist_plot_type", "Choose Plot Type:",
                    choices = list(
                      "Bar Chart - Species Abundance (Clickable)" = "bar_species",
                      "Comparison - Your Data vs iNaturalist (Boxplot)" = "comparison_diversity",
                      "Interactive Map - Species Locations" = "interactive_map",
                      "Monthly Diversity Trends" = "monthly_diversity"
                    ),
                    selected = "bar_species"
                  ),
                  conditionalPanel(
                    condition = "input.inaturalist_plot_type == 'bar_species'",
                    numericInput("num_species_display", "Number of Species to Display:", 
                                value = 15, min = 5, max = 50, step = 5),
                    div(style = "font-size: 12px; color: #666; margin-bottom: 15px;",
                        "Choose how many top species to show in the bar chart")
                  ),
                  br(),
                  h5("Code:"),
                  verbatimTextOutput("inaturalist_code")
                ),
                column(8,
                  h4("iNaturalist Visualization:"),
                  conditionalPanel(
                    condition = "input.inaturalist_plot_type != 'interactive_map'",
                    plotlyOutput("inaturalist_plot"),
                    
                    # Species photo display area for clickable bar chart
                    conditionalPanel(
                      condition = "input.inaturalist_plot_type == 'bar_species'",
                      br(),
                      div(
                        style = "border-top: 2px solid #ecf0f1; padding-top: 15px;",
                        h5("Click on a species bar above to see its photo and details"),
                        uiOutput("species_photo_display")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.inaturalist_plot_type == 'interactive_map'",
                    leafletOutput("inaturalist_map", height = "400px"),
                    br(),
                    h5("Species Selection Table:"),
                    p("Click on species (or multiple) to show only those observations on the map."),
                    DT::dataTableOutput("species_table")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "!output.inaturalist_fetched",
              div(
                style = "text-align: center; padding: 40px;",
                icon("globe", style = "font-size: 48px; color: #ccc;"),
                br(), br(),
                h4("Fetch iNaturalist data to see visualizations", style = "color: #999;")
              )
            )
          )
        )
      )
    )
  )
)
