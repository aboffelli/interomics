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
    id = "tabset",
    tabPanel("File upload",
      value = "upload",
      sidebarLayout(
        sidebarPanel(
          # OTU table input box
          fileInput(
            "otu",
            "Choose OTU table file",
            multiple = F,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # Taxonomic table input box
          fileInput(
            "taxa",
            "Choose Taxonomic table file",
            multiple = F,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # Sample table input box
          fileInput(
            "sample",
            "Choose Sample table file",
            multiple = F,
            accept = c("text/csv", ".csv", 
                       "text/comma-separated-values,text/plain")
          ),
          
          # check box for header
          checkboxInput("header", "Header", T),
          
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
          
          # display the whole table
          checkboxInput("all", "Display all", F),
          
          checkboxInput("example", "Use an example dataset", F),
          
          # Button to Abundance tab.
          actionButton("abundance", "Abundance"),
          
          # Button to Graphics tab.
          actionButton("plots", "Graphics")
        ),
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
    
    tabPanel("Abundance",
      # Code to display the taxonomic tree.
      # Abundance filter slider if possible.
      value = "taxa",
      # Variable selection box
      fluidRow(
        column(2,
               varSelectInput("sample_var", 
                              "Select the sample label", data=F),
               helpText("Label that appears under each sample in the heatmap."))
        ),
      navlistPanel(
        widths = c(2, 10),
        tabPanel("Heatmap",
                 wellPanel(
                   plotlyOutput("heat_plot",
                                height = "750px")
                 )),
        
        tabPanel("Taxonomic tree") 
      )),
    
    tabPanel("Graphics",
      # Code to display the graphics.
      value = "graph",
      # Variable selection boxes
      fluidRow(
        column(2,
               varSelectInput("fill_var", 
                              "Select the color variable", data=F)),
        column(2,
               varSelectInput('shape_var', 
                              "Select the shape variable", data=F))
        ),
      helpText("The color and shape of the points will be based on the variables selected"),
      # Create a tab panel with different types of plots
      navlistPanel(
        widths = c(2, 10),
        tabPanel("Biplot",
                 wellPanel(plotlyOutput("biplot",
                                        height = "750px"))),
        tabPanel("Alpha Diversity",
                 wellPanel())
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
