library(shiny)
library(metacoder)
library(phyloseq)
library(ggplot2)

# Set a new theme for ggplot
theme_set(theme_bw())

# Function to create a biplot object using phyloseq
create_biplot <- function(taxa, otu, sample) {
    OTU <- otu_table(otu, taxa_are_rows=T)
    TAXA <- tax_table(taxa)
    SAMPLE <- sample_data(sample)
    phylo <- phyloseq(OTU, TAXA, SAMPLE)
    phylo.ord <- ordinate(phylo, "NMDS", "bray")
    biplot <- plot_ordination(phylo, phylo.ord, type="split", color="Family", shape="Ecotype", title="Biplot")
    return(biplot)
}

# Function to generate the taxonomic tree.
taxa_tree <- function(taxa) {
    
}




server <- function(input, output, session) {
    # Upload tab
    taxa_df <- reactive({
        as.matrix(read.table(input$taxa$datapath,
                             header = input$header,
                             sep = input$sep, row.names=row.names(otu_df()))[,2:10])
        
    })
    
    otu_df <- reactive({
        as.matrix(read.table(input$otu$datapath,
                             sep = input$sep,
                             row.names=1,
                             header=input$header,
                             check.names=F)
        )
    })
    
    
    sample_df <- reactive({
        read.table(input$sample$datapath,
                   header = input$header,
                   sep = input$sep, row.names=1)
    })
    
    # Slidebar will react to change
    headnum <- eventReactive(input$rownum, {
        input$rownum
    })
    
    output$taxa_table <- renderTable({
        req(input$taxa)
        # display the table based on the slider
        if (!input$all) {
            head(taxa_df(), n = headnum())
        }
        else
            taxa_df()
    }, rownames=T)
    
    output$otu_table <- renderTable({
        req(input$otu)
        # display the table based on the slider
        if (!input$all) {
            head(otu_df(), n = headnum())
        }
        else
            otu_df()
    }, rownames=T)
    
    output$sample_table <- renderTable({
        req(input$sample)
        # display the table based on the slider
        if (!input$all) {
            head(sample_df(), n = headnum())
        }
        else
            sample_df()
    }, rownames=T)
        
    
    
    # Button Taxonomic tree change to Taxonomy tab
    observeEvent(input$taxon, {
        req(input$taxa)
        updateTabsetPanel(session,
                          "tabset",
                          selected = "taxa")
        # Update the variable options
        updateVarSelectInput(session, "tree_col", data=taxa_df(), selected=F)
    })
    
    # Button Annotation change to Plots tab
    observeEvent(input$plots, {
        req(input$taxa)
        req(input$otu)
        req(input$sample)
        updateTabsetPanel(session,
                          "tabset",
                          selected = "graph")
    })
    
    # Taxonomy tab
    # Build the tax tree
    output$taxa_tree <- renderPlot({
        # only works when clicking in "Taxonomic tree" Button
        req(input$tree_col)
        
    })
    
    # Graphics tab
    # Biplot outoput
    output$biplot <- renderPlotly({
        # only works when clicking in "Graphics" button
        req(input$plots)
        biplot <- create_biplot(taxa=taxa_df(), 
                                otu=otu_df(),
                                sample=sample_df())
        ggplotly(biplot)
    })
}