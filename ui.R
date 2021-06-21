
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
          # radio buttons for separator
          radioButtons("sep",
                       "Separator",
                       choices = c(
                         Tab = "\t",
                         Comma = ",",
                         Semicolon = ";"),
                       selected = "\t"
          ),
          
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
          
          # Subset 2
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
          
          # Subset 3
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
          
          downloadButton("download_subset", "Download subsetted tables")
          ),
        
        
        # Tables display
        mainPanel(fluidRow(
          column(
            12,
            p(strong("OTU table")),
            div(style = "height:500px; overflow-y:scroll",
                # Display the tables
                DT::dataTableOutput("otu_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Taxa table")),
            div(style = "height:500px; overflow-y:scroll",
                DT::dataTableOutput("taxa_table"))
          ),
          column(
            12,
            hr(),
            p(strong("Sample table")),
            div(style = "height:500px; overflow-y:scroll",
                DT::dataTableOutput("sample_table"))
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
                          varSelectizeInput("sample_var", 
                                         "Select the sample label", 
                                         data=FALSE),
                          )
                   ),
                 helpText("The selection is required to load the plot and defines the label under each sample."),
                 downloadButton("download_heatmap"),
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
                          varSelectizeInput("taxa_filter_level", 
                                      "Select the level to filter", 
                                      data=character(0))),
                   column(2,
                          varSelectizeInput("taxa_filter_selection", 
                                      "Select the target group to filter", 
                                      data=character(0)))
                   
                   ),
                 actionButton("make_tree", "Create tree"),
                 downloadButton("download_tree"),
                 helpText("You can choose any level to filter, however the filtering is optional. 
                          To create the tree, click in the button above. The image may take some time to be created."),
                 wellPanel(
                   plotOutput("tax_tree",
                              height="1500px")
                   ))
        )),
    
###############################################################################
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
