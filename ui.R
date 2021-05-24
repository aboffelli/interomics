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
  
  # Main tabs of the page 
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
          
          # radio buttons for separator
          radioButtons("sep",
                       "Separator",
                       choices = c(
                         Tab = "\t",
                         Comma = ",",
                         Semicolon = ";"),
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
          br(),
          
          p(strong("Subset the data")),
          br(),
          radioButtons("subset_remove",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          
          div(style="display: inline-block; width: 32%",
              selectInput("subset_type",
                          "Select the table",
                          choices=c("", "Taxa", "Sample"),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_level",
                          "Select the level", 
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_choice",
                          "Select the target group", 
                          choices=NULL,
                          width="100%")),
          checkboxInput("use_subset", "Use subsetted data", FALSE)
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
            ))
          ))
      ),
    
###############################################################################
    # Abundance tab containing Heatmap and possibly a taxonomic tree or barplot.
    tabPanel("Abundance",
      value = "taxa",
      # Division between heatmap and tax tree
      navlistPanel(
        id="tax_tabs",
        widths = c(2, 10),
        
        tabPanel("Heatmap",
                 # Variable selection box
                 fluidRow(
                   column(3,
                          varSelectInput("sample_var", 
                                         "Select the sample label", 
                                         data=FALSE),
                          )
                   ),
                 helpText("The selection is required to load the plot and defines the label under each sample"),
                 # Heatmap display
                 wellPanel(
                   plotlyOutput("heat_plot",
                                height = "750px")
                 )),
        
        tabPanel("Taxonomic tree",
                 fluidRow(
                   column(12,
                   sliderInput("abundance_filter", 
                               "Select the minimum abundance to display", 
                               min=0, 
                               max=1000, 
                               value=0, 
                               width='100%')),
                   column(2,
                          selectInput("taxa_filter_level", 
                                      "Select the level to filter", 
                                      choices=NULL)),
                   column(2,
                          selectInput("taxa_filter_selection", 
                                      "Select the target group to filter", 
                                      choices=NULL))
                   
                   ),
                 actionButton("make_tree", "Create tree"),
                 # TODO: help message
                 helpText("Help message"),
                 wellPanel(
                   plotOutput("tax_tree",
                              height="1000px")
                   ))
        )),
    
###############################################################################
    # Graphics tab containing Biplot and Alpha Diversity plots
    tabPanel("Function",
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
                 helpText("Both, color and shape variables, are required to load the plot."),
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
                                      choices=c("Observed", "Chao1", "ACE",
                                                "Shannon", "Simpson",
                                                "InvSimpson", "Fisher"),
                                      multiple=TRUE))
                 ),
                   helpText("The X variable and the measure are required. More then one measure can be selected at the same time."),
                 
                 # Plot display
                 wellPanel(plotlyOutput("alpha",
                                        height = "750px")))
      )
    )
  ),
  # print a line
  column(12,
         hr(),
  # link to GitHub
  p(strong("Visit "),
    strong(
      a(href = "https://github.com/aboffelli/interomics",
        "Interomics GitHub")
    ),
    strong("for more information."))
  ))
