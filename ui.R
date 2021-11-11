## -----------------------------------------------------------------------------
##
## Script name: global.R
##
## Author: Arthur Boffelli Castro
##
## Date created: 2021-05-05
##
## GitHub: https://github.com/aboffelli/interomics
##
## Description:
##  Script responsible for creating the web template and all the features 
##    displayed on the screen.
##  
## -----------------------------------------------------------------------------
## 
## Notes:
##  This script is one of three scripts used to run Interomics (global.R, ui.R
##    and server.R). The three scripts must be in the same directory to run the 
##    program.
##    
## ----------------------------------------------------------------------------- 

# Starts a web page.
ui <- fluidPage(
  # Name in the web browser tab.
  title="Interomics",
  
  # Application title on the top of the page
  fluidRow(
    wellPanel(
      h1(strong("Interomics"), 
         align = "center", 
         style = "color:firebrick; font-size: 100px"),
      
      h2("An interactive application for metagenomic data"),
      align = 'center')
    ),
  
  # Creates three separated tabs: File Upload, Abundance, and Diversity.
  tabsetPanel(
    id = "tabswitch",
    
## -----------------------------------------------------------------------------
## File Upload tab
## Initial tab containing the buttons to upload the tables and the options for 
##  subsetting. The tables are displayed in the right side of the page,
    
    # Start the tab.
    tabPanel("File Upload",
      value = "upload",
      sidebarLayout(
        sidebarPanel(
          # Create the radio buttons for separator
          radioButtons("sep",
                       "Separator",
                       choices = c(
                         Tab = "\t",
                         Comma = ",",
                         Semicolon = ";"),
                       selected = "\t"
          ),
          # Add three input boxes for the files.
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
          
          # Add checkbox that loads the example data
          checkboxInput("example", "Load an example dataset", FALSE),
          br(),
          
          ## Subset area -------------------------------------------------------
          hr(),
          # Add the title and a help message.
          p(strong("Subset the data")),
          helpText("Use this option to filter the data. You can either isolate or remove the chosen groups."),
          helpText("All plots will be affected, except the Taxonomic Tree."),
          # Add a check box that will activate the subset.
          checkboxInput("use_subset", "Use subset", FALSE),
          br(),
          
          ## Subset boxes. Three separate boxes to allow more combinations.
          
          # First box for subset (this code will be repeated for the three 
          #   options).
          # Radio buttons to chose if the selection will be either selected or
          #   removed from the table.
          radioButtons("subset_remove1",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          # The boxes containing the possible selections (Table, 
          #   level, and group).
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_type1",
                          "Select the table",
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_level1",
                          "Select the level", 
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_choice1",
                          "Select the target group", 
                          choices=character(0),
                          multiple=TRUE,
                          width="100%")),
          
          # Second box for subset
          radioButtons("subset_remove2",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_type2",
                          "Select the table",
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_level2",
                          "Select the level", 
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_choice2",
                          "Select the target group", 
                          choices=character(0),
                          multiple=TRUE,
                          width="100%")),
          
          # Third box for subset
          radioButtons("subset_remove3",
                       label=NULL,
                       choices=c("Select",
                                 "Remove")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_type3",
                          "Select the table",
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_level3",
                          "Select the level", 
                          choices=character(0),
                          width="100%")),
          div(style="display: inline-block; width: 32%",
              selectizeInput("subset_choice3",
                          "Select the target group", 
                          choices=character(0),
                          multiple=TRUE,
                          width="100%")),
          
          # Add a button to download the tables after the subset.
          downloadButton("download_subset", "Download subsetted tables")
          ),
        
        
        ## Tables display ------------------------------------------------------
        # Add the area where the three tables will be displayed.
        mainPanel(fluidRow(
          # OTU counts table.
          column(
            12,
            p(strong("OTU table")),
            div(style = "height:500px; overflow-y:scroll",
                # Area to display the table
                DT::dataTableOutput("otu_table"))
          ),
          # Taxonomy table.
          column(
            12,
            hr(),
            p(strong("Taxa table")),
            div(style = "height:500px; overflow-y:scroll",
                # Area to display the table
                DT::dataTableOutput("taxa_table"))
          ),
          # Sample information table
          column(
            12,
            hr(),
            p(strong("Sample table")),
            div(style = "height:500px; overflow-y:scroll",
                # Area to display the table
                DT::dataTableOutput("sample_table"))
            ))
          ))
      ),
    
## -----------------------------------------------------------------------------
## Abundance tab contains the Heatmap and the Taxonomy tree.
 
    # Create the tab.
    tabPanel("Abundance",
      value = "taxa",
      # Create a division between heatmap and tax tree
      navlistPanel(
        id="tax_tabs",
        widths = c(2, 10),
        
        ## Heatmap -------------------------------------------------------------
        tabPanel("Heatmap",
                 # Create the selection box for the sample lable.
                 fluidRow(
                   column(3,
                          varSelectizeInput("sample_var", 
                                         "Select the sample label", 
                                         data=FALSE),
                          )
                   ),
                 # Add a help message.
                 helpText("The selection is required to load the plot and defines the label under each sample."),
                 # Add the download button.
                 downloadButton("download_heatmap"),
                 # Area to display the heatmap.
                 wellPanel(
                   plotlyOutput("heat_plot",
                                height = "750px")
                 )),
        
        ## Taxonomy tree -------------------------------------------------------
        tabPanel("Taxonomic tree",
                 # Create the slider to choose the minimum abundance
                 fluidRow(
                   column(12,
                   # Add the slider set to zero.
                   sliderInput("abundance_filter", 
                               "Select the minimum abundance to display", 
                               min=0, 
                               max=100, 
                               value=0, 
                               width='100%')),
                   # Add the two boxes to filter the table. One for the 
                   #  taxonomic level and one for the targeted group.
                   column(2,
                          varSelectizeInput("taxa_filter_level", 
                                      "Select the level to filter", 
                                      data=character(0))),
                   column(2,
                          varSelectizeInput("taxa_filter_selection", 
                                      "Select the target group to filter", 
                                      data=character(0)))
                   
                   ),
                 # Add the button to create the tree, since the tree creation
                 #  takes some time, it will only run after clicking this 
                 #  button.
                 actionButton("make_tree", "Create tree"),
                 # Add the download button.
                 downloadButton("download_tree"),
                 # Add a help message.
                 helpText("You can choose any level to filter, however the filtering is optional. 
                          To create the tree, click in the button above. The image may take some time to be created."),
                 wellPanel(
                   # Area to display the tree.
                   plotOutput("tax_tree",
                              height="1500px")
                   ))
        )),
    
## -----------------------------------------------------------------------------
## Diversity tab.

    # Function tab containing Biplot and Alpha Diversity plots
    tabPanel("Diversity",
      value = "graph",
      # Division between Biplot and Alpha
      navlistPanel(
        widths = c(2, 10),
        
        tabPanel("Alpha-Diversity",
                 # Variables selection
                 fluidRow(
                   column(2,
                          varSelectizeInput("alpha_x_var", 
                                            "Select the x variable", 
                                            data=character(0))),
                   column(2,
                          varSelectizeInput("alpha_col_var",
                                            "Select the color variable",
                                            data=character(0))),
                   column(2,
                          varSelectizeInput("alpha_shape_var",
                                            "Select the shape variable",
                                            data=character(0))),
                   column(2,
                          selectizeInput("alpha_measure_var", 
                                         "Select the measures used",
                                         choices=c("Observed", "Chao1", "ACE",
                                                   "Shannon", "Simpson",
                                                   "InvSimpson", "Fisher"),
                                         multiple=TRUE)),
                   column(4,
                          sliderInput("alpha_slider",
                                      "Trim the data",
                                      min=0,
                                      max=30,
                                      value=0,
                                      width='100%'))
                 ),
                 helpText("The X variable and the measure are required. More then one measure can be selected at the same time.
                            OTUs with the abundance equal to or below the number chosen in the slider will be removed."),
                 downloadButton("download_alpha"),
                 
                 # Plot display
                 wellPanel(plotlyOutput("alpha",
                                        height = "750px"))),
        
        tabPanel("Beta-Diversity",
                 # Variable selection boxes
                 fluidRow(
                   column(2,
                          selectizeInput("type_var", 
                                         "Select the type", 
                                         choices=character(0))),
                   column(2,
                          varSelectizeInput("fill_var", 
                                         "Select the color variable", 
                                         data=character(0))),
                   column(2,
                          varSelectizeInput('shape_var', 
                                         "Select the shape variable", 
                                         data=character(0)))
                 ),
                 helpText("All variables, type, color and shape, are required to load the plot."),
                 downloadButton("download_beta"),
                 # Plot display
                 wellPanel(plotlyOutput("beta",
                                        height = "750px")))
      )
    )
  ),

  # print a line
  fluidRow(wellPanel(
  # link to GitHub
  p(strong("Visit "),
    strong(
      a(href = "https://github.com/aboffelli/interomics",
        "Interomics GitHub")
    ),
    strong("for more information."))
  ))
)
