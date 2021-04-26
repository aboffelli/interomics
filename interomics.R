library(shiny)
library(metacoder)

ui <- fluidPage(
  fluidRow(column(
    6, offset = 3,
    wellPanel(
      h1(strong("Interomics"), align = "center", style = "color:firebrick; font-size: 100px"),
      h2("An interactive application for metagenomic data"),
      align = 'center'
    )
  )),
  
  tabsetPanel(
    tabPanel(
      "Input upload",
      value = "upload",
      sidebarLayout(
        sidebarPanel(
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
          sliderInput(
            "rownum",
            "Number of rows to display",
            value = 10,
            min = 1,
            max = 50
          )
        ),
        mainPanel(fluidRow(
          column(
            12,
            div(style = "height:410px; overflow-y:scroll",
                # Display the table
                tableOutput("contents"))
          ),
          # Button(s) to continue with the selected table.
          column(3, style = "height:25px",
                 actionButton("continue", "Continue"))
        ))
      )
    ),
    
    tabPanel("Taxonomy"
             # Code to display the taxonomic tree.
             # Abundance filter slider if possible.
             
             
             ),
             
             tabPanel("Functional Anotation"
                      # Code to display the graphics.
                      # Zoom and click
                      )
             ),
             
             hr(),
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
      output$contents <- renderTable({
        req(input$file)
        
        df <- read.table(input$file$datapath,
                         header = input$header,
                         sep = input$sep)
        # display the table based on the slider
        head(df, n = headnum())
      })
      
    }
    
    shinyApp(ui = ui, server = server)