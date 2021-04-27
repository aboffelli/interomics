library(shiny)
library(metacoder)

ui <- fluidPage(
  # Application title in the top of the page
  fluidRow(column(
    6, offset = 3,
    wellPanel(
      h1(strong("Interomics"), align = "center", style = "color:firebrick; font-size: 100px"),
      h2("An interactive application for metagenomic data"),
      align = 'center'
    )
  )),
  
  
  tabsetPanel(
    id = "tabset",
    tabPanel(
      "File upload",
      value = "upload",
      sidebarLayout(
        sidebarPanel(
          # Input box
          fileInput(
            "file",
            "Choose File",
            multiple = F,
            accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")
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
          
          # Button to Taxonomy tab.
          actionButton("taxon", "Taxonomic Tree"),
          # Button to Annotation tab.
          actionButton("plots", "Annotation"),
        ),
        mainPanel(fluidRow(column(
          12,
          div(style = "height:450px; overflow-y:scroll",
              # Display the table
              tableOutput("contents"))
        )))
      )
    ),
    
    tabPanel("Taxonomy",
             # Code to display the taxonomic tree.
             # Abundance filter slider if possible.
             value = "taxa",
             plotOutput('taxa_tree', 
                        height=1000)
             ),
    
    tabPanel("Functional Anotation",
             # Code to display the graphics.
             # Zoom and click
             value = "funct")
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

server <- function(input, output) {
  # Upload tab
  
  # Slidebar will react to change
  headnum <- eventReactive(input$rownum, {
    input$rownum
  })
  # uploaded table display
  output$contents <- renderTable({
    req(input$file)
    
    df <- read.table(input$file$datapath,
                     header = input$header,
                     sep = input$sep)
    # display the table based on the slider
    if (!input$all) {
      head(df, n = headnum())
    }
    else
      df
    
  })
  
  # Button Taxonomic tree change to Taxonomy tab
  observeEvent(input$taxon, {
    req(input$file)
    updateTabsetPanel(session = getDefaultReactiveDomain(),
                      "tabset",
                      selected = "taxa")
  })
  
  # Button Annotation change to Plots tab
  observeEvent(input$plots, {
    req(input$file)
    updateTabsetPanel(session = getDefaultReactiveDomain(),
                      "tabset",
                      selected = "funct")
  })
  
  # Taxonomy tab
  # Build the tax tree
  output$taxa_tree <- renderPlot({
    # only works when clicking in "Taxonomic tree" Button
    req(input$taxon)
    
    df <- read.table(input$file$datapath,
                     header = input$header,
                     sep = input$sep)
    
    # Only works if column with taxa is called V1 and separated by semicolon (TODO fix this)
    taxa_data <- extract_tax_data(df$V1,
                                  key = c("class"),
                                  regex="(.*)",
                                  class_sep = ";")
    # Create the plot
    heat_tree(taxa_data, node_label=taxon_names, node_size=n_obs, node_color=n_obs)
  })
  
}

shinyApp(ui = ui, server = server)