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

# Function to generate the taxonomic tree.
create_taxmap <- function(taxa, otu) {
    # Create a taxonomy column
    taxa_col <- as.data.frame(matrix(nrow=nrow(taxa), ncol=1))
    for(i in 1:nrow(taxa)) {
        tax_line <- toString(taxa[i,])
        tax_line <- gsub(", NA", "", tax_line)
        tax_line <- gsub(", ", ";", tax_line)
        
        taxa_col[i,1] <- tax_line
    }

    colnames(taxa_col) <- "Taxonomy"
    
    # Join the new column in the OTU table
    taxonomic_df <- cbind(otu, taxa_col)
    
    # Create the tax maps
    taxmap <- parse_tax_data(taxonomic_df,
                             class_cols="Taxonomy", 
                             class_sep=";")
    names(taxmap$data) <- "otu_counts"
    return(taxmap)
}

# Function to subset the phylo object
subset_func <- function(phylo, level, choice, taxa, remove) {
    # Taxa table
    if(taxa) {
        # Change the column name
        positions <- colnames(phylo@tax_table@.Data)
        colnames(phylo@tax_table@.Data)[which(positions == level)] <- "sel_col"
        # Change the target name
        phylo@tax_table@.Data[phylo@tax_table@.Data == choice] <- "choice"
        
        # Subset the object
        if(!remove) phylo <- subset_taxa(phylo, sel_col== "choice")
        else phylo <- subset_taxa(phylo, sel_col!= "choice")
        
        # Change the column back
        positions <- colnames(phylo@tax_table@.Data)
        colnames(phylo@tax_table@.Data)[which(positions=="sel_col")] <- level
        # Change the target back
        phylo@tax_table@.Data[phylo@tax_table@.Data == "choice"] <- choice
    }
    
    # Sample table
    else {
        # Change the column name
        positions <- colnames(phylo@sam_data)
        colnames(phylo@sam_data)[which(positions==level)] <- "sel_col" 
        # Change the target name
        change <- as.character(phylo@sam_data@.Data[[which(positions==level)]])
        change[change==choice] <- "choice"
        phylo@sam_data@.Data[[which(positions==level)]] <- factor(change)
        
        if(!remove) phylo <- subset_samples(phylo, sel_col=="choice")
        else phylo <- subset_samples(phylo, sel_col!="choice")
        
        # Change the target back
        change <- as.character(phylo@sam_data@.Data[[which(positions==level)]])
        change[change=="choice"] <- choice
        phylo@sam_data@.Data[[which(positions==level)]] <- factor(change)
        # Change the column back
        positions <- colnames(phylo@sam_data)
        colnames(phylo@sam_data)[which(positions=="sel_col")] <- level
    }
        
    return(phylo)
}




server <- function(input, output, session) {
    observeEvent(input$example, {
        data("GlobalPatterns")
    })
    
    # Upload tab
    taxa_df <- reactive({
        if(!input$example) {
            req(input$taxa)
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
            req(input$otu)
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
            req(input$sample)
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
    
    phylo <- reactive({
        req(taxa_df(), otu_df(), sample_df())
        
        phylo <- create_phylo(taxa=taxa_df(),
                              otu=otu_df(),
                              sample=sample_df())
        
        # TODO: subset more than one target
        if(input$use_subset) {
            # TODO: FIX - subset only works if there is no NA in the column
            type <- toString(input$subset_type)
            level <- toString(input$subset_level)
            choice <- toString(input$subset_choice)
            if (type == "Taxa") taxa <- TRUE
            else taxa <- FALSE
            if (input$subset_remove=="Select") remove <- FALSE
            else remove <- TRUE
            phylo <- subset_func(phylo=phylo, 
                                 level=level, 
                                 choice=choice, 
                                 taxa=taxa,
                                 remove=remove)
            
        }
        phylo
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
    
    # TODO: FIX - Observe event is subset_type, if this changes before data input the variables do not update
    
    # Subset variables level
    observeEvent(input$subset_type, {
        req(taxa_df(), otu_df(), sample_df())
        # Taxa table
        if(toString(input$subset_type)=="Taxa") {
            updateSelectInput(session, "subset_level", 
                              choices=colnames(taxa_df()),
                              selected=FALSE)
        }
        # Sample table
        else {
            updateSelectInput(session, "subset_level", 
                              choices=colnames(sample_df()),
                              selected=FALSE)
        }
    })
    
    # Subset variable choice
    observeEvent(input$subset_level, {
        req(input$subset_level)
        level <- toString(input$subset_level)
        # Taxa table
        if(toString(input$subset_type)=="Taxa")
            updateSelectInput(session,
                              "subset_choice",
                              choices=unique(taxa_df()[,level]),
                              selected=FALSE)
        # Sample table
        else {
            updateSelectInput(session,
                              "subset_choice",
                              choices=unique(sample_df()[,level]),
                              selected=FALSE)
            }
    })
    
    # Update variable boxes when change tab
    observeEvent(input$tabswitch, {
        req(taxa_df(), otu_df(), sample_df())
        
        # Heatmap
        updateVarSelectInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=FALSE)
        #Tax tree
        updateSelectInput(session, 
                          "taxa_filter_level", 
                          choices=colnames(taxa_df()), selected=FALSE)
        
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
    
###############################################################################
    # Abundance tab
    
    # Heatmap output
    output$heat_plot <- renderPlotly({
        # only works after selecting the sample label
        req(input$sample_var)
        
        chosen_var <- toString(input$sample_var)
        phylo <- phylo()

        heat_plot <- plot_heatmap(phylo, sample.label=chosen_var, 
                                      low="#66CCFF", high="#000033")
        # Display plot
        ggplotly(heat_plot
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
                 )   
    })
    
    # Taxonomy tree tab
    # Taxa filter
    observeEvent(input$taxa_filter_level, {
        req(input$taxa_filter_level)
        level <- toString(input$taxa_filter_level)
        
        # Update the filter target
        updateSelectInput(session,
                          "taxa_filter_selection",
                          choices=unique(taxa_df()[,level]),
                          selected=FALSE)
    })
    
    # Tree display
    output$tax_tree <- renderPlot({
        req(input$make_tree)
        filter_num <- input$abundance_filter
        
        taxmap <- create_taxmap(taxa=taxa_df(), 
                                otu=otu_df())
        # Check if the filter is selected
        if(!is.null(input$taxa_filter_selection)){
            taxmap <- taxa::filter_taxa(taxmap,
                                  taxon_names==toString(
                                      input$taxa_filter_selection),
                                  subtaxa=TRUE)
        }
        
        reads_filter <- rowSums(
            taxmap$data$otu_counts[,row.names(sample_df())]) < filter_num
        
        taxmap <- filter_obs(taxmap, "otu_counts",
                             !reads_filter, drop_taxa=TRUE)
        
        # Display the tree
        heat_tree(taxmap,
                  node_label = taxon_names,
                  node_size = n_obs,
                  node_color=n_obs)
    })
    
###############################################################################
    # Graphics tab
    
    # Biplot output
    output$biplot <- renderPlotly({
        # only works after selecting fill and shape
        req(input$fill_var, input$shape_var)
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))

        phylo <- phylo()
        
        
        biplot <- create_biplot(phylo, 
                                fill=chosen_var[1], 
                                shape=chosen_var[2]) +
            scale_shape(solid=FALSE)
        
        # Display plot
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
        # Display plot
        ggplotly(alpha_div 
                 + theme(plot.margin = unit(c(1, 1, 1, 1.5), "cm")),
                 tooltip=c(x, col, "value")
                 )
    })
}
