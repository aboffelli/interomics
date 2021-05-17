library(shiny)
library(metacoder)
library(phyloseq)
library(ggplot2)

# Set a new theme for ggplot
theme_set(theme_bw())

# Function to create a phylo object using phyloseq
create_phylo <- function(taxa, otu, sample) {
    OTU <- otu_table(otu, taxa_are_rows=TRUE)
    TAXA <- tax_table(taxa)
    SAMPLE <- sample_data(sample)
    phylo <- phyloseq(OTU, TAXA, SAMPLE)
    return(phylo)
}

# Function to create a phylo object using phyloseq
create_biplot <- function(phylo_object, fill, shape) {
    phylo.ord <- ordinate(phylo_object, "NMDS", "bray")
    biplot <- plot_ordination(phylo_object, phylo.ord, 
                              type="split", color=fill, 
                              shape=shape)
    return(biplot)
}

# Function to create a heatmap
create_heatmap <- function(phylo_object) {
    heat_plot <- plot_heatmap(phylo_object)
    return(heat_plot)
}


# Function to generate the taxonomic tree.
create_taxmap <- function(taxa, otu, sample) {
    # Create a taxonomy column
    taxa_cols <- matrix(nrow=nrow(df), ncol=1)
    for(i in 1:nrow(df)) {
        tax_line <- toString(df[i,])
        tax_line <- gsub(", NA", "", tax_line)
        tax_line <- gsub(", ", ";", tax_line)
        
        taxa_cols[i,1] <- tax_line
    }
    colnames(taxa_cols) <- "Taxonomy"
    
    taxonomic_df <- cbind(taxa, taxa_cols)
    
    taxmap <- parse_tax_data(taxonomic_df, 
                              class_cols="Taxonomy", 
                              class_sep=";")
    names(taxa$data) <- "otu_counts"
    return(taxmap)
}

# Functions to the subset option
# Change the selected column to a default name
subset_col_on <- function(phylo, tax_col) {
    positions <- colnames(phylo@tax_table@.Data)
    colnames(phylo@tax_table@.Data)[which(positions==tax_col)] <- "sel_col"
    return(phylo)
}

# Change the column name back to the original
subset_col_off <- function(phylo, tax_col) {
    positions <- colnames(phylo@tax_table@.Data)
    colnames(phylo@tax_table@.Data)[which(positions=="sel_col")] <- tax_col
    return(phylo)
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
                                 header = TRUE,
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
                                 header=TRUE,
                                 check.names=FALSE))
        }
        # Use example data
        else GlobalPatterns@otu_table
    })
    
    sample_df <- reactive({
        if(!input$example) {
            # Create the table based on the file
            df <- read.table(input$sample$datapath,
                       header = TRUE,
                       sep = input$sep)
            row.names(df) <- df[,1]
            df
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
    
    # Update variable boxes when change tab (Upload files)
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
    
    # Update variable boxes when change tab (Example data)
    observeEvent(input$tabswitch, {
        req(input$example)
        # Heatmap
        updateVarSelectInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=FALSE)
        
        # Biplot
        # Create a df with all sample and taxa columns
        x <- matrix(ncol=sum(ncol(taxa_df()),ncol(sample_df())), nrow=0)
        colnames(x) <- c(colnames(taxa_df()), colnames(sample_df()))
        updateVarSelectInput(session,
                             "fill_var", 
                             data = x, selected=FALSE)
        updateVarSelectInput(session,
                             "shape_var", 
                             data = x, selected=FALSE)
        # Alpha diversity
        updateVarSelectInput(session,
                             "alpha_x_var",
                             data=sample_df(), selected=FALSE)
        updateVarSelectInput(session,
                             "alpha_col_var",
                             data=sample_df(), selected=FALSE)
    })
    
    # Abundance tab
    
    # Heatmap output
    output$heat_plot <- renderPlotly({
        # only works after selecting the sample label
        req(input$sample_var)
        
        chosen_var <- toString(input$sample_var)
        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        colnames(phylo@tax_table@.Data)[1] <- "Domain"
        
        if(input$example) {
            archaea <- subset_taxa(phylo, Domain=="Archaea")
            
            heat_plot <- plot_heatmap(archaea, sample.label=chosen_var, 
                                      low="#66CCFF", high="#000033")
        }
        else {
            heat_plot <- plot_heatmap(phylo, sample.label=chosen_var, 
                                      low="#66CCFF", high="#000033")
        }
        ggplotly(heat_plot
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
                 )   
    })
    
    # Graphics tab
    
    # Biplot output
    output$biplot <- renderPlotly({
        # only works after selecting fill and shape
        req(input$fill_var, input$shape_var)
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))

        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        biplot <- create_biplot(phylo, 
                                fill=chosen_var[1], 
                                shape=chosen_var[2]) +
            scale_shape(solid=FALSE)
        
        ggplotly(biplot
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
                 tooltip=c(chosen_var[1],
                           chosen_var[2], 
                           "NMDS1", 
                           "NMDS2"))
    })
    
    # Alpha-diversity output
    output$alpha <- renderPlotly({
        # Only works after selecting measure and x.
        req(input$alpha_measure_var, input$alpha_x_var)
        
        phylo <- create_phylo(taxa=taxa_df(), 
                              otu=otu_df(),
                              sample=sample_df())
        
        Alpha <- prune_taxa(taxa_sums(phylo) > 10, phylo) 
        
        x <- toString(input$alpha_x_var)
        
        # Color default will be black, unless selected by the user.
        col <- NULL
        if(!is.null(input$alpha_col_var)) col <- toString(input$alpha_col_var)
        
        alpha_div <- plot_richness(Alpha,
                                   x=x,
                                   color=col,
                                   measures=input$alpha_measure_var)
        ggplotly(alpha_div 
                 + theme(plot.margin = unit(c(1, 1, 1, 1.5), "cm")),
                 tooltip=c(x, col, "value")
                 )
    })
}
