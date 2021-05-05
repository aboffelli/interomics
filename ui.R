library(shiny)
library(plotly)

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
    
    
    tabsetPanel(id = "tabset",
                tabPanel(
                    "File upload",
                    value = "upload",
                    sidebarLayout(
                        sidebarPanel(
                            # OTU table input box
                            fileInput(
                                "otu",
                                "Choose OTU table file",
                                multiple = F,
                                accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")
                            ),
                            # Taxonomic table input box
                            fileInput(
                                "taxa",
                                "Choose Taxonomic table file",
                                multiple = F,
                                accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")
                            ),
                            # Sample table input box
                            fileInput(
                                "sample",
                                "Choose Sample table file",
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
                    )),
                tabPanel(
                    "Taxonomy",
                    # Code to display the taxonomic tree.
                    # Abundance filter slider if possible.
                    value = "taxa",
                    column(3, wellPanel(
                        varSelectInput(
                            "tree_col",
                            "Select the column containing the taxonomic information: ",
                            F
                        ),
                        helpText("Requires Taxonomic table upload in the File Upload tab.")
                        # Include after (maybe)
                        # selectInput("tree_type", "Select the output type used: ", choices=c("Kaiju", "EggNOG"))
                    )),
                    plotOutput('taxa_tree',
                               height = 1300)
                    ),
    
                tabPanel(
                    "Graphics",
                    # Code to display the graphics.
                    value = "graph",
                    # Create a tab panel with different types of plots
                    navlistPanel(
                        widths = c(2, 10),
                        tabPanel("Biplot",
                                 wellPanel(plotlyOutput("biplot",
                                                        height = "750px"))),
                        tabPanel("Other stuff",
                                 wellPanel())
                    )
                )),
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
