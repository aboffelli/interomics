library(shiny)
library(metacoder)
library(phyloseq)
library(ggplot2)

# Set a new theme for ggplot
theme_set(theme_bw())

# Function to create a biplot object using phyloseq
create_phylo <- function(taxa, otu, sample) {
    OTU <- otu_table(otu, taxa_are_rows=TRUE)
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
    observeEvent(input$example, {
        data("GlobalPatterns")
    })
    
    # Upload tab
    taxa_df <- reactive({
        if(!input$example) {
            # Create the table based on the file
            as.matrix(read.table(input$taxa$datapath,
                                 header = input$header,
                                 sep = input$sep, 
                                 na.strings="",
                                 row.names=1)[,1:9])
        }
        # Use example data
        else GlobalPatterns@tax_table 
    })
    
    otu_df <- reactive({
        if(!input$example) {
            # Create the table based on the file
            as.matrix(read.table(input$otu$datapath,
                                 sep = input$sep,
                                 row.names=1,
                                 header=input$header,
                                 check.names=FALSE))
        }
        # Use example data
        else GlobalPatterns@otu_table
    })
    
    sample_df <- reactive({
        if(!input$example) {
            # Create the table based on the file
            read.table(input$sample$datapath,
                       header = input$header,
                       sep = input$sep, 
                       row.names=1)
        }
        # Use example data
        else GlobalPatterns@sam_data
    })
    
    # Slidebar will react to change
    headnum <- eventReactive(input$rownum, {
        input$rownum
    })
    
    output$taxa_table <- renderTable({
        if(!input$example) {
            req(input$taxa)
            # display the file table
            head(taxa_df(), n = headnum())
            }
        else    # Display the example table
            head(taxa_df(), n = headnum())
        },
        rownames=TRUE)
            
    
    output$otu_table <- renderTable({
        if(!input$example) {
            req(input$otu)
            # display the file table
            head(otu_df(), n = headnum())
            }
        else    # Display the example table
            head(otu_df(), n=headnum())
        }, 
        rownames=TRUE)
    
    output$sample_table <- renderTable({
        if(!input$example) {
            req(input$sample)
            # display the file table
            head(sample_df(), n = headnum())
            }
        else    # Display the example table
            head(as.matrix(sample_df()), n=headnum())
    }, 
    rownames=TRUE)
    
    # Button Abundance change to Abundance tab
    observeEvent(input$tabswitch, {
        req(input$taxa, input$otu, input$sample)
        
        # Update the variable options
        updateVarSelectInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=FALSE)
        x <- matrix(ncol=sum(ncol(taxa_df()),ncol(sample_df())), nrow=0)
        colnames(x) <- c(colnames(taxa_df()), colnames(sample_df()))
        updateVarSelectInput(session,
                             "fill_var", 
                             data = x, selected=FALSE)
        updateVarSelectInput(session,
                             "shape_var", 
                             data = x, selected=FALSE)
        updateVarSelectInput(session,
                             "alpha_x_var",
                             data=sample_df(), selected=FALSE)
        updateVarSelectInput(session,
                             "alpha_col_var",
                             data=sample_df(), selected=FALSE)
    })
    
    # When changing tabs update all variable boxes
    observeEvent(input$tabswitch, {
        req(input$example)
        updateVarSelectInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=FALSE)
        x <- matrix(ncol=sum(ncol(taxa_df()),ncol(sample_df())), nrow=0)
        colnames(x) <- c(colnames(taxa_df()), colnames(sample_df()))
        updateVarSelectInput(session,
                             "fill_var", 
                             data = x, selected=FALSE)
        updateVarSelectInput(session,
                             "shape_var", 
                             data = x, selected=FALSE)
        updateVarSelectInput(session,
                             "alpha_x_var",
                             data=sample_df(), selected=FALSE)
        updateVarSelectInput(session,
                             "alpha_col_var",
                             data=sample_df(), selected=FALSE)
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
        
        if(input$example) {
            archaea <- subset_taxa(phylo, Kingdom=="Archaea")
            
            heat_plot <- plot_heatmap(archaea, sample.label=chosen_var, low="#66CCFF", high="#000033")
        }
        else {
            heat_plot <- plot_heatmap(phylo, sample.label=chosen_var, low="#66CCFF", high="#000033")
        }
        ggplotly(heat_plot)   
    })
    
    # Graphics tab
    
    # Biplot output
    output$biplot <- renderPlotly({
        # only works when clicking in "Graphics" button
        req(input$fill_var, input$shape_var)
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))

        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        biplot <- create_biplot(phylo, 
                                fill=chosen_var[1], 
                                shape=chosen_var[2]) +
            scale_shape(solid=FALSE)
        
        ggplotly(biplot, tooltip=c(chosen_var[1], 
                                   chosen_var[2], 
                                   "NMDS1", 
                                   "NMDS2"))
    })
    
    # Alpha-diversity output
    output$alpha <- renderPlotly({
        req(input$alpha_measure_var)
        
        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        x <- NULL
        if(!is.null(input$alpha_x_var)) x <- toString(input$alpha_x_var)
        col <- NULL
        if(!is.null(input$alpha_col_var)) col <- toString(input$alpha_col_var)
        
        alpha_div <- plot_richness(phylo,
                                   x=x,
                                   color=col,
                                   measures=input$alpha_measure_var)
        ggplotly(alpha_div)
    })
}