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
            accept = c("text/csv", ".csv", ".tab", ".tsv", 
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
          checkboxInput("example", "Load an example dataset", FALSE),
          br(),
          
          hr(),
          p(strong("Subset the data")),
          helpText("Use this option to filter the data. You can either isolate or remove the chosen groups."),
          helpText("All plots will be affected, except the Taxonomic Tree."),
          checkboxInput("use_subset", "Use subsetted data", FALSE),
          br(),
          
          # Subset 1
          radioButtons("subset_remove1",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          
          div(style="display: inline-block; width: 32%",
              selectInput("subset_type1",
                          "Select the table",
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_level1",
                          "Select the level", 
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_choice1",
                          "Select the target group", 
                          choices=NULL,
                          width="100%")),
          
          # Subset 2
          radioButtons("subset_remove2",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_type2",
                          "Select the table",
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_level2",
                          "Select the level", 
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_choice2",
                          "Select the target group", 
                          choices=NULL,
                          width="100%")),
          
          # Subset 3
          radioButtons("subset_remove3",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_type3",
                          "Select the table",
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_level3",
                          "Select the level", 
                          choices=NULL,
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectInput("subset_choice3",
                          "Select the target group", 
                          choices=NULL,
                          width="100%"))
          
          ),
        
        # Tables display
        mainPanel(fluidRow(
          column(
            12,
            p(strong("OTU table")),
            div(style = "height:350px; overflow-y:scroll",
                # Display the tables
                tableOutput("otu_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Taxa table")),
            div(style = "height:350px; overflow-y:scroll",
                tableOutput("taxa_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Sample table")),
            div(style = "height:350px; overflow-y:scroll",
                tableOutput("sample_table"))
            ))
          ))
      ),
    
###############################################################################
    # Abundance tab containing Heatmap and taxonomic tree.
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
                 helpText("The selection is required to load the plot and defines the label under each sample."),
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
                               max=100, 
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
                 downloadButton("download_tree"),
                 helpText("You can choose any level to filter, however the filtering is optional. To create the tree, click in the button above. The image may take some time to be created."),
                 wellPanel(
                   plotOutput("tax_tree",
                              height="1500px")
                   ))
        )),
    
###############################################################################
    # Function tab containing Biplot and Alpha Diversity plots
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
                          varSelectInput("alpha_shape_var",
                                         "Select the shape variable",
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
