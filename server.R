## -----------------------------------------------------------------------------
##
## Script name: global.R
##
## Author: Arthur Boffelli Castro
##
## Date created: 2021-05-05
##
## GitHub: https://github.com/aboffelli/interomics
##
## Description:
##  Script containing all the code necessary to run the program and generate 
##      the features that will be displayed in the web interface.
##  
## -----------------------------------------------------------------------------
## 
## Notes:
##  This script is one of three scripts used to run Interomics (global.R, ui.R
##    and server.R), the three scripts must be in the same directory to run the 
##    program. 
## -----------------------------------------------------------------------------

# Create the running function for shiny.
server <- function(input, output, session) {
    # Check if the example check box is selected. If it is selected, the data
    #   GlobalPatterns from phyloseq is loaded.
    observeEvent(input$example, {
        data("GlobalPatterns")
    })
    
    ## Upload tables -----------------------------------------------------------
    # Load the tables that are provided on the upload button.
    taxa_df <- reactive({
        # Only load the tables if the example check box is not selected.
        if(!input$example) {
            # The respective file must be uploaded.
            req(input$taxa)
            # Create the table based on the file
            as.matrix(read.table(input$taxa$datapath,
                                 header = TRUE,
                                 sep = input$sep, 
                                 na.strings="",
                                 row.names=1))
        }
        # If the example check box is selected, use the example data.
        else tax_table(GlobalPatterns) 
    })
    
    otu_df <- reactive({
        # Only load the tables if the example check box is not selected.
        if(!input$example) {
            # The respective file must be uploaded.
            req(input$otu)
            # Create the table based on the file
            as.matrix(read.table(input$otu$datapath,
                                 sep = input$sep,
                                 row.names=1,
                                 header=TRUE,
                                 check.names=FALSE))
        }
        # If the example check box is selected, use the example data.
        else otu_table(GlobalPatterns)
    })
    
    sample_df <- reactive({
        # Only load the tables if the example check box is not selected.
        if(!input$example) {
            # The respective file must be uploaded.
            req(input$sample)
            # Create the table based on the file
            df <- read.table(input$sample$datapath,
                       header = TRUE,
                       sep = input$sep)
            row.names(df) <- df[,1]
            df
        }
        # If the example check box is selected, use the example data.
        else sample_data(GlobalPatterns)
    })
    
    # Display the three tables in the interface
    # DT creates the interactive tables.
    output$taxa_table <- DT::renderDataTable({
        # The phylo object is necessary to display the tables, which means that
        # the tables will only be displayed if all three files are uploaded.
        req(phylo())
        tax_table(phylo())
        }, rownames=TRUE)
            
    output$otu_table <- DT::renderDataTable({
        req(phylo())
        otu_table(phylo())
        }, rownames=TRUE)
    
    output$sample_table <- DT::renderDataTable({
        req(phylo())
        sample_data(phylo())
        }, rownames=TRUE)
    
    # Activate the download tables button, saving the three tables tab delimited
    # compressed in a tar file.
    output$download_subset <- downloadHandler(
        filename="interomics_tables.tar",
        content=function(filename) {
            # write the tables in files.
            write.table(tax_table(phylo()), file="taxa_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(otu_table(phylo()), file="otu_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(sample_data(phylo()), file="sample_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            # Compress the three files.
            tar(filename, files=c("taxa_table.txt", 
                                      "otu_table.txt", 
                                      "sample_table.txt"))
            # Remove the files not compressed.
            file.remove(c("taxa_table.txt", 
                          "otu_table.txt", 
                          "sample_table.txt"))
        })

    
    ## Subset the data ---------------------------------------------------------
    ## All the necessary code for subset the tables.
    
    # Populate the choices of the type of table for subset if the "Use subset" 
    # check box is selected.
    observeEvent(input$use_subset, {
        # Only run if the check box is selected.
        req(input$use_subset)
        # Clear all boxes every time the check box is selected to reset the 
        # subset. The vectors used here are created in the global.R script. 
        for (var in c(subset_types, subset_levels, subset_choices)) {
            updateSelectizeInput(session, var,
                              choices=character(0),
                              selected=character(0))
        }
        # Activate the first box (table) for the tree subset box set. Only 
        # "Taxa" and "Sample".
        for(var in subset_types) {
            updateSelectizeInput(session, var,
                              choices=c("Taxa", "Sample"),
                              selected=character(0))
        }
    })
    
    # Observe if any of the first boxes is changed, this activates the second 
    # box.
    observeEvent(c(input$subset_type1, 
                   input$subset_type2, 
                   input$subset_type3), {
           # Only run in the phylo object exists.
           req(phylo())
           
           # Activate the second box based on the selection of the first box. 
           for(i in 1:3) {
               # Check if the the level box is empty before updating.
               x <- parse(text=paste0("input$",subset_levels[i]))
               if(eval(x)=="") {
                   # Retrieve the choice from the first box.
                   x <- parse(text=paste0("input$",subset_types[i]))
                   # If the choice is "Taxa", populate the choices from the 
                   # second box with column names from the taxa table.
                   if(toString(eval(x))=="Taxa") {
                       updateSelectizeInput(session, subset_levels[i], 
                                      choices=colnames(tax_table(phylo())),
                                      selected=character(0))
                       }
                    # If the choice is "Sample", populate the choices from the 
                   # second box with column names from the sample table.
                    else if(toString(eval(x))=="Sample") {
                        updateSelectizeInput(session, subset_levels[i], 
                                          choices=colnames(
                                              sample_data(phylo())),
                                          selected=character(0))
                    }
               }}
    })
    
    # Observe if the second box is selected, and activate the third box.
    observeEvent(c(input$subset_level1,
                   input$subset_level2,
                   input$subset_level3,
                   phylo()), {
        # Only run if the phylo object exists.
        req(phylo())
        
        # Activate the third box, based on the choice of the second box.              
        for(i in 1:3) {
            # Check if the the choice box is empty before updating.
            x <- parse(text=paste0("input$",subset_choices[i]))
            if(toString(eval(x))=='') {
                # Retrieve the choice from the second box.
                x <- parse(text=paste0("input$",subset_levels[i]))
                level <- toString(eval(x))
                # If the selection is one of the columns from the taxa table, 
                # populate the choices of the third box with all unique values 
                # from that specific column.
                if(level %in% colnames(tax_table(phylo()))) {
                    updateSelectizeInput(session,
                                      subset_choices[i],
                                      choices=unique(
                                          tax_table(phylo())[,level]),
                                      selected=character(0))
                }
                # If the selection is one of the columns from the sample table, 
                # populate the choices of the third box with all unique values 
                # from that specific column.
                else if (level %in% colnames(sample_data(phylo()))) {
                    updateSelectizeInput(session,
                                      subset_choices[i],
                                      choices=unique(as.matrix(
                                          sample_data(phylo())[,level])),
                                      selected=character(0))
                }
                }}
    })
    
    ## Phylo object ------------------------------------------------------------
    # As soon as all the tree tables are uploaded, the phylo object is created.
    phylo <- reactive({
        req(taxa_df(), otu_df(), sample_df())
        
        # First, a phylo object without subset is created.
        phylo <- create_phylo(taxa=taxa_df(),
                              otu=otu_df(),
                              sample=sample_df())
        
        # With a phylo object created, check if the subset check box is 
        # activated.
        if(input$use_subset==TRUE) {
            # If yes, check all three box sets.
            for(i in 1:3){
                # Check if the third box is not empty.
                x <- eval(parse(text=paste0("input$",subset_choices[i])))
                if(toString(x)!=""){
                    # If third box is not empty, get the information from the 
                    # three boxes and the value on the radio button with either
                    # select or remove, and save it all in a vector.
                    subset_data <- c(
                        type=isolate(eval(
                            parse(text=paste0("input$",
                                              subset_types[i])))), 
                        level=isolate(eval(
                                parse(text=paste0("input$",
                                                  subset_levels[i])))),
                        choice=isolate(eval(
                            parse(text=paste0("input$",
                                              subset_choices[i])))),
                        remove=eval(
                            parse(text=paste0("input$", 
                                              subset_removes[i]))))
                    
                    # Check if the selection of the radio button is select or 
                    # remove, and assign True or False to remove.
                    if (subset_data["remove"] == "Select") remove <- FALSE
                    else remove <- TRUE
                    
                    # Check if the selection is in the Taxa or Sample table.
                    if (subset_data["type"] == "Taxa") {
                        # Check if there are more than 1 selection.
                        if (length(x) == 1) {
                            # If only one choice were made, apply the subset 
                            # function in the phylo object.
                            phylo <- taxa_subset(
                                phylo,
                                level=subset_data["level"],
                                choice=subset_data["choice"],
                                remove=remove)
                        }
                        
                        # More than one choice.
                        else {
                            # Check if we want to remove or isolate the 
                            # choices.
                            if(!remove) {
                                # If it is not being removed a new phylo object 
                                # will be created for each choice and merged 
                                # together in the end.
                                phylos <- list()
                                # Create the necessary number of phylo objects, 
                                # and store them in a list.
                                for(i in 1:length(x)) {
                                    merge_phylo <- taxa_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove)
                                    phylos[[i]] <- merge_phylo
                                    }
                                
                                # Assign the first phylo on the list to a 
                                # variable.
                                phylo <- phylos[[1]]
                                # From the second on, merge into the first.
                                for (i in 2:length(phylos))
                                    phylo <- merge_phyloseq(phylo, phylos[[i]])
                            }
                            else{
                                # If it is being removed, only remove them one
                                # after the other in a loop.
                                for(i in 1:length(x)){
                                    phylo <- taxa_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove)
                                }}
                            }}
                    # Sample subset    
                    else {
                        # Check if there are more than 1 selection.
                        if (length(x) == 1) {
                            # If only one choice were made, apply the subset 
                            # function in the phylo object.
                            phylo <- sample_subset(
                                phylo,
                                level=subset_data["level"],
                                choice=subset_data["choice"],
                                remove=remove)
                        }
                        # More than one choice.
                        else {
                            # Check if we want to remove or isolate the 
                            # choices.
                            if(!remove) {
                                # If it is not being removed a new phylo object 
                                # will be created for each choice and merged 
                                # together in the end.
                                phylos <- list()
                                # Create the necessary number of phylo objects, 
                                # and store them in a list.
                                for(i in 1:length(x)) {
                                    merge_phylo <- sample_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove)
                                    phylos[[i]] <- merge_phylo
                                }
                                # Assign the first phylo on the list to a 
                                # variable.
                                phylo <- phylos[[1]]
                                # From the second on, merge into the first.
                                for (i in 2:length(phylos))
                                    phylo <- merge_phyloseq(phylo, phylos[[i]])
                            }
                            else{
                                # If it is being removed, only remove them one
                                # after the other in a loop.
                                for(i in 1:length(x)){
                                    phylo <- sample_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove) 
                                }
                            }}

                }}}}
        # Return the final phylo object.
        phylo
    })
    
    ## Update the variables ----------------------------------------------------
    ## Update the variable boxes in the plots every time the main tab changes 
    ##  according to the phylo object.
    
    observeEvent(input$tabswitch, {
        # Only runs if the phylo object exists.
        req(phylo())
        # First clear all the boxes, to reset the plots.
        for (var in c("sample_var", "taxa_filter_level",
                      "taxa_filter_selection", "fill_var", 
                      "shape_var", "alpha_x_var",
                      "alpha_col_var", "alpha_shape_var")) {
            updateVarSelectizeInput(session, var,
                                    data=character(0),
                                    selected=character(0))
        }
        # Clear the alpha measure by itself, since the options in it will not 
        # change, only the selection is erased.
        updateSelectizeInput(session, "alpha_measure_var", 
                             selected=character(0))
        
        # Update the Heatmap variables.
        updateVarSelectizeInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=character(0))
        #Update the Tax tree variables.
        updateSelectizeInput(session, 
                          "taxa_filter_level", 
                          choices=colnames(taxa_df()), selected=character(0))
        
        # Update the Beta diversity variables.
        updateSelectizeInput(session, "type_var", 
                             choices=c("taxa", "samples",
                                       "biplot", "split"),
                             selected=character(0))
                             
        
        # Update the Alpha diversity variables.
        for (var in c("alpha_x_var", "alpha_col_var", "alpha_shape_var")) {
        updateVarSelectizeInput(session,
                             var,
                             data=sample_df(), selected=character(0))
        }
        
    })
    

    ## Abundance tab -----------------------------------------------------------
    ## Heatmap
    
    # Create the heatmap object.
    heat_plot <- reactive({
        # Only runs after selecting the sample label.
        req(input$sample_var)
        
        # Retrieve the selection of the variable.
        chosen_var <- toString(input$sample_var)
        phylo <- phylo()
        
        # Create the heatmap object inverting the default colors, so the darker
        # color is more abundant and lighter color is less abundant.
        heat_plot <- plot_heatmap(phylo, sample.label=chosen_var, 
                                  low="#66CCFF", high="#000033", 
                                  na.value="white")
    })
    
    # Display the heatmap in the page
    output$heat_plot <- renderPlotly({
        # Only runs if the heatmap object exists.
        req(heat_plot())
        
        # Display plot in the screen. Set the hovering information to sample 
        # name, OTU number and the normalized abundance.
        ggplotly(heat_plot()
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")), tooltip=c(
                     "Sample", "OTU", "Abundance"))
    })
    
    # Activate the download button to download the heatmap.
    output$download_heatmap <- downloadHandler(
        filename="heatmap.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(heat_plot())
            dev.off()
        })
    
    ## Taxonomic tree
    
    # Check the filter box was modified.
    observeEvent(input$taxa_filter_level, {
        # Only runs if the first box is not empty. 
        req(input$taxa_filter_level)
        level <- toString(input$taxa_filter_level)
        if(level!=""){
            # Update the second box according to the selection on the first box.
            updateSelectizeInput(session,
                              "taxa_filter_selection",
                              choices=unique(taxa_df()[,level]),
                              selected=character(0))
            }
    })
    
    # Create the tree object
    tax_tree <- eventReactive(input$make_tree, {
        # eventReactive isolate the selection boxes, so the tree will only be 
        # created after clicking the button.
         
        # Retrieve the number in the filter slider.
        filter_num <- input$abundance_filter
        
        # Create the taxmap using the metacoder package.
        taxmap <- create_taxmap(taxa=taxa_df(), 
                                otu=otu_df())
        # Check if the filter is selected and filter the taxmap using only the
        # levels below the selected.
        if(!is.null(input$taxa_filter_selection)){
            taxmap <- taxa::filter_taxa(taxmap,
                                        taxon_names==toString(
                                            input$taxa_filter_selection),
                                        subtaxa=TRUE)
        }
        
        # Keep only the organisms with a minimum abundance set by the slider.
        taxmap <- taxa::filter_taxa(taxmap, n_obs>=filter_num)
        
        # Create the tree object.
        heat_tree(taxmap,
                  node_label = taxon_names,
                  node_size = n_obs,
                  node_color=n_obs,
                  node_size_range = c(0.005, 0.05),
                  node_label_size_range = c(0.008, 0.04))
    })
    
    # Display the tree in the screen.
    output$tax_tree <- renderPlot({
        # Only runs if the tree object exists.
        req(tax_tree())
        
        tax_tree()
        
    })
    
    # Activate the download button to download the taxonomic tree.
    output$download_tree <- downloadHandler(
        filename="taxa_tree.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(tax_tree())
            dev.off()
        })
    
    
    ## Diversity tab -----------------------------------------------------------
    ## Alpha-diversity 
    
    # Create the Alpha diversity object
    alpha_div <- reactive({
        # Only runs after selecting measure and x variable.
        req(input$alpha_measure_var, input$alpha_x_var)
        
        phylo <- phylo()
        
        # Retrieve the slider number for trimming the data.
        trim_value <- input$alpha_slider
        # Keep only the organisms with a minimum abundance set by the slider.
        Alpha <- prune_taxa(taxa_sums(phylo) > trim_value, phylo) 
        
        # Retrieve the x variable selected.
        x <- toString(input$alpha_x_var)
        
        # Set the color by default as null, which causes the default color of 
        # the function (black). 
        col <- NULL
        if(!is.null(input$alpha_col_var)) {
            # If the user selected a color variable, reassign it to col.
            col <- toString(input$alpha_col_var)
        }
        
        # Set the default shape as null, which causes the default shape of the
        # function (19 - circle).
        shape <- NULL
        if(!is.null(input$alpha_shape_var)) {
            # If the user selected a shape variable, reassign it to shape.
            shape <- toString(input$alpha_shape_var)
        }
        
        # Create the alpha plot object.
        alpha_div <- plot_richness(Alpha,
                                   x=x,
                                   color=col,
                                   shape=shape,
                                   measures=input$alpha_measure_var)
    })
    
    # Display the alpha-diversity plot in the screen.
    output$alpha <- renderPlotly({
        # Only runs if the alpha plot object exists.
        req(alpha_div())
        
        # Reassign all the selections again for the hovering information on 
        # the plot.
        x <- toString(input$alpha_x_var)
        
        col <- NULL
        if(!is.null(input$alpha_col_var)) {
            col <- toString(input$alpha_col_var)
        }

        shape <- NULL
        if(!is.null(input$alpha_shape_var)) {
            shape <- toString(input$alpha_shape_var)
        }
        
        # Display plot in the screen.
        ggplotly(alpha_div() 
                 + theme(plot.margin = unit(c(1, 1, 1, 1.5), "cm")),
                 tooltip=c(x, col, shape, "value")
                 )
    })
    
    # Activate the Alpha-diversity download button to download the plot.
    output$download_alpha <- downloadHandler(
        filename="alpha_diversity.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(alpha_div())
            dev.off()
        })
    
    ## Beta-diversity
    
    # Beta selection boxes update
    observeEvent(input$type_var, {
        req(input$type_var)
        type <- input$type_var
        if(type %in% c("biplot", "split")) {
            x <- matrix(ncol=sum(ncol(taxa_df()),ncol(sample_df())), nrow=0)
            colnames(x) <- c(colnames(taxa_df()), colnames(sample_df()))
            for (var in c("fill_var", "shape_var")) {
                updateVarSelectizeInput(session,
                                        var, 
                                        data = x, selected=character(0))
            }}
        else if(type == "taxa") {
            for(var in c("fill_var", "shape_var")) {
                print(var)
                updateVarSelectizeInput(session,
                                        var, 
                                        data = taxa_df(), 
                                        selected=character(0))  
            }}
        else {
            for (var in c("fill_var", "shape_var")) {
                updateVarSelectizeInput(session,
                                        var, 
                                        data = sample_df(), 
                                        selected=character(0))
        }}
            
    })
    
    # Beta-diversity object
    beta_div <- reactive({
        # only works after selecting fill and shape
        req(input$fill_var, input$shape_var)
        chosen_var <- c(toString(input$type_var), toString(input$fill_var), toString(input$shape_var))
        
        phylo <- phylo()
        
        
        Beta <- create_beta(phylo,
                            type=chosen_var[1],
                            fill=chosen_var[2], 
                            shape=chosen_var[3]) +
            scale_shape(solid=FALSE)
    })
    # Display beta-diversity
    output$beta <- renderPlotly({
        req(beta_div())
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))
        ggplotly(beta_div()
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
                 tooltip=c(chosen_var[1],
                           chosen_var[2], 
                           "NMDS1", 
                           "NMDS2"))
    })
    
    # Beta-diversity download
    output$download_beta <- downloadHandler(
        filename="biplot.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(beta_div())
            dev.off()
        })
}
