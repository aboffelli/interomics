library(shiny)
library(plotly)

ui <- fluidPage(
  # Application title in the top of the page
  fluidRow(column(
    6, offset = 3,
    wellPanel(
      h1(strong("Interomics"), 
         align = "center", 
         style = "color:firebrick; font-size: 100px"),
      
      h2("An interactive application for metagenomic data"),
      align = 'center')
    )),

  tabsetPanel(
    id = "tabswitch",
    tabPanel("File upload",
      value = "upload",
      sidebarLayout(
        sidebarPanel(
          # OTU table input box
          fileInput(
            "otu",
            "Choose a OTU counts table file",
            multiple = FALSE,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # Taxonomic table input box
          fileInput(
            "taxa",
            "Choose a Taxonomic table file",
            multiple = FALSE,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # Sample table input box
          fileInput(
            "sample",
            "Choose a Sample information table file",
            multiple = FALSE,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # check box for header
          checkboxInput("header", "Header", TRUE),
          
          # radio buttons for separator
          radioButtons(
            "sep",
            "Separator",
            choices = c(
              Tab = "\t",
              Comma = ",",
              Semicolon = ";"
            ),
            selected = "\t"
          ),
          
          # slider for number of rows to display
          sliderInput(
            "rownum",
            "Number of rows to display",
            value = 10,
            min = 1,
            max = 50
          ),
          # Checkbox that loads the example data
          checkboxInput("example", "Use an example dataset", FALSE),
        ),
        
        # Tables display
        mainPanel(fluidRow(
          column(
            12,
            p(strong("OTU table")),
            div(style = "height:200px; overflow-y:scroll",
                # Display the tables
                tableOutput("otu_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Taxa table")),
            div(style = "height:200px; overflow-y:scroll",
                tableOutput("taxa_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Sample table")),
            div(style = "height:200px; overflow-y:scroll",
                tableOutput("sample_table"))
          )
        ))
      )
    ),
    
    # Abundance tab containing Heatmap and possibly a taxonomic tree or barplot.
    tabPanel("Abundance",
      value = "taxa",
      # Division between heatmap and tax tree
      navlistPanel(
        widths = c(2, 10),
        
        tabPanel("Heatmap",
                 # Variable selection box
                 fluidRow(
                   column(2,
                          varSelectInput("sample_var", 
                                         "Select the sample label", 
                                         data=FALSE),
                          helpText("Label that appears under each sample in the heatmap."))
                 ),
                 # Plot display
                 wellPanel(
                   plotlyOutput("heat_plot",
                                height = "750px")
                 )),
        
        tabPanel("Taxonomic tree") 
      )),
    
    # Graphics tab containing Biplot and Alpha Diversity plots
    tabPanel("Graphics",
      value = "graph",
      # Division between Biplot and Alpha
      navlistPanel(
        widths = c(2, 10),
        
        tabPanel("Biplot",
                 # Variable selection boxes
                 fluidRow(
                   column(2,
                          varSelectInput("fill_var", 
                                         "Select the color variable", 
                                         data=FALSE)),
                   column(2,
                          varSelectInput('shape_var', 
                                         "Select the shape variable", 
                                         data=FALSE))
                 ),
                 helpText("The color and shape of the points will be based on the variables selected"),
                 # Plot display
                 wellPanel(plotlyOutput("biplot",
                                        height = "750px"))),
        tabPanel("Alpha Diversity",
                 # Variables selection
                 fluidRow(
                   column(2,
                          varSelectInput("alpha_x_var", 
                                         "Select the x variable", 
                                         data=FALSE)),
                   column(2,
                          varSelectInput("alpha_col_var",
                                         "Select the color variable",
                                         data=FALSE)),
                   column(2,
                          selectInput("alpha_measure_var", 
                                      "Select the measures used",
                                      choices=c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher"),
                                      multiple=TRUE))
                 ),
                 # Plot display
                 wellPanel(plotlyOutput("alpha",
                                        height = "750px")))
      )
    )
  ),
  # print a line
  hr(),
  # link to GitHub
  p(strong("Visit "),
    strong(
      a(href = "https://github.com/aboffelli/interomics",
        "Interomics GitHub")
    ),
    strong("for more information."))
)
