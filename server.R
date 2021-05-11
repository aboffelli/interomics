library(shiny)
library(metacoder)
library(phyloseq)
library(ggplot2)

# Set a new theme for ggplot
theme_set(theme_bw())

# Function to create a biplot object using phyloseq
create_phylo <- function(taxa, otu, sample) {
    OTU <- otu_table(otu, taxa_are_rows=T)
    TAXA <- tax_table(taxa)
    SAMPLE <- sample_data(sample)
    phylo <- phyloseq(OTU, TAXA, SAMPLE)
    return(phylo)
}

create_biplot <- function(phylo_object, fill, shape) {
    phylo.ord <- ordinate(phylo_object, "NMDS", "bray")
    biplot <- plot_ordination(phylo_object, phylo.ord, type="split", color=fill, shape=shape)
    return(biplot)
}

create_heatmap <- function(phylo_object) {
    heat_plot <- plot_heatmap(phylo_object)
    return(heat_plot)
}

# Function to generate the taxonomic tree.
create_taxa_tree <- function(taxa) {
    
}



server <- function(input, output, session) {
    # Upload tab
    taxa_df <- reactive({
        as.matrix(read.table(input$taxa$datapath,
                             header = input$header,
                             sep = input$sep, row.names=1)[,1:9])
        
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
    observeEvent(input$abundance, {
        req(input$taxa)
        req(input$otu)
        req(input$sample)
        updateTabsetPanel(session,
                          "tabset",
                          selected = "taxa")
        # Update the variable options
        updateVarSelectInput(session, "sample_var", data=sample_df(), selected=F)
    })
    
    # Button Annotation change to Plots tab
    observeEvent(input$plots, {
        req(input$taxa)
        req(input$otu)
        req(input$sample)
        updateTabsetPanel(session,
                          "tabset",
                          selected = "graph")
        x <- matrix(ncol=sum(ncol(taxa_df()),ncol(sample_df())), nrow=0)
        colnames(x) <- c(colnames(taxa_df()), colnames(sample_df()))
        updateVarSelectInput(session,
                             "fill_var", 
                             data = x, selected=F)
        updateVarSelectInput(session,
                             "shape_var", 
                             data = x, selected=F)
    })
    
    # Taxonomy tab
    # Build the tax tree
    output$heat_plot <- renderPlotly({
        # only works when clicking in "Taxonomic tree" Button
        req(input$sample_var)
        
        chosen_var <- toString(input$sample_var)
        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        heat_plot <- plot_heatmap(phylo, sample.label=chosen_var)
        ggplotly(heat_plot)
        
    })
    
    # Graphics tab
    # Biplot output
    
    output$biplot <- renderPlotly({
        # only works when clicking in "Graphics" button
        req(input$fill_var)
        req(input$shape_var)
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))
        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        biplot <- create_biplot(phylo, 
                                fill=chosen_var[1], 
                                shape=chosen_var[2]) +
            scale_shape(solid=F)
        
        ggplotly(biplot, tooltip=c(chosen_var[1], 
                                   chosen_var[2], 
                                   "NMDS1", 
                                   "NMDS2"))
    })
}