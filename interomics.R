library(shiny)
library(metacoder)

ui <- fluidPage(
    fluidRow(
        column(6, offset=3,
            wellPanel(
                h1(strong("Interomics"), align="center", style="color:firebrick; font-size: 100px"),
                h2("An interactive application for metagenomic data"), align='center'
                )
            )
        ),
    
    tabsetPanel(
        tabPanel("Input upload"
            # Code to upload the data
        ),
        
        tabPanel("Taxonomy",
            # Code to display the taxonomic tree.
            # Abundance filter slider
        ),
        
        tabPanel("Functional Anotation"
            # Code to display the graphics.
            # Zoom and click
        )
    ),
    hr(),
    p(strong("Visit "),
      strong(
          a(href="https://github.com/aboffelli/interomics", 
            "Interomics GitHub")
        ),
        strong("for more information.")
    )
)

server <- function(input, output) {
    # All the function codes
}

shinyApp(ui=ui, server=server)