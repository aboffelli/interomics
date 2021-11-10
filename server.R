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
##  TODO
##  
## -----------------------------------------------------------------------------
## 
## Notes:
##  This script is one of three scripts used to run Interomics (global.R, ui.R
##    and server.R), the three scripts must be in the same directory to run the 
##    program. 
## -----------------------------------------------------------------------------


server <- function(input, output, session) {
    observeEvent(input$example, {
        data("GlobalPatterns")
    })
    
    # Upload tables
    taxa_df <- reactive({
        if(!input$example) {
            req(input$taxa)
            # Create the table based on the file
            as.matrix(read.table(input$taxa$datapath,
                                 header = TRUE,
                                 sep = input$sep, 
                                 na.strings="",
                                 row.names=1))
        }
        # Use example data
        else tax_table(GlobalPatterns) 
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
        else otu_table(GlobalPatterns)
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
        else sample_data(GlobalPatterns)
    })
    # Display tables
    output$taxa_table <- DT::renderDataTable({
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
    
    #Download tables
    output$download_subset <- downloadHandler(
        filename="interomics_tables.tar",
        content=function(filename) {
            
            write.table(tax_table(phylo()), file="taxa_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(otu_table(phylo()), file="otu_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(sample_data(phylo()), file="sample_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            
            tar(filename, files=c("taxa_table.txt", 
                                      "otu_table.txt", 
                                      "sample_table.txt"))
            file.remove(c("taxa_table.txt", 
                          "otu_table.txt", 
                          "sample_table.txt"))
        })

################################################################################
    # Subset
    
    # Subset variables type
    observeEvent(input$use_subset, {
        req(input$use_subset)
        # Clear all boxes
        for (var in c(subset_types, subset_levels, subset_choices)) {
            updateSelectizeInput(session, var,
                              choices=character(0),
                              selected=character(0))
        }
        for(var in subset_types) {
            updateSelectizeInput(session, var,
                              choices=c("Taxa", "Sample"),
                              selected=character(0))
        }
    })
    
    # Subset variables level
    observeEvent(c(input$subset_type1, 
                   input$subset_type2, 
                   input$subset_type3), {
           req(phylo())
            
           for(i in 1:3) {
               # Check if the the level box is empty before updating
               # TODO: box do not update if not empty and you change type
               x <- parse(text=paste0("input$",subset_levels[i]))
               if(eval(x)=="") {
                   x <- parse(text=paste0("input$",subset_types[i]))
                   # Taxa table
                   if(toString(eval(x))=="Taxa") {
                       updateSelectizeInput(session, subset_levels[i], 
                                      choices=colnames(tax_table(phylo())),
                                      selected=character(0))
                       }
                    # Sample table
                    else if(toString(eval(x))=="Sample") {
                        updateSelectizeInput(session, subset_levels[i], 
                                          choices=colnames(
                                              sample_data(phylo())),
                                          selected=character(0))
                    }
               }}
    })
    
    # Subset variable choice
    observeEvent(c(input$subset_level1,
                   input$subset_level2,
                   input$subset_level3,
                   phylo()), {
        req(phylo())
                       
        for(i in 1:3) {
            # Check if the the choice box is empty before updating
            x <- parse(text=paste0("input$",subset_choices[i]))
            if(toString(eval(x))=='') {
                x <- parse(text=paste0("input$",subset_levels[i]))
                level <- toString(eval(x))
                # Taxa table
                if(level %in% colnames(tax_table(phylo()))) {
                    updateSelectizeInput(session,
                                      subset_choices[i],
                                      choices=unique(
                                          tax_table(phylo())[,level]),
                                      selected=character(0))
                }
                # Sample table
                else if (level %in% colnames(sample_data(phylo()))) {
                    updateSelectizeInput(session,
                                      subset_choices[i],
                                      choices=unique(as.matrix(
                                          sample_data(phylo())[,level])),
                                      selected=character(0))
                }
                }}
    })
    
################################################################################
    # Create the phylo object
    phylo <- reactive({
        req(taxa_df(), otu_df(), sample_df())
        
        # Create phylo object without subset
        phylo <- create_phylo(taxa=taxa_df(),
                              otu=otu_df(),
                              sample=sample_df())
        
        # Check subset
        if(input$use_subset==TRUE) {

            for(i in 1:3){
                x <- eval(parse(text=paste0("input$",subset_choices[i])))
                # If choice box is not empty
                if(toString(x)!=""){
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
                    
                    if (subset_data["remove"] == "Select") remove <- FALSE
                    else remove <- TRUE
                    
                    # Check if it is a Taxa or Sample subset
                    if (subset_data["type"] == "Taxa") {
                        # Check if there are more than 1 selection
                        if (length(x) == 1) {
                            phylo <- taxa_subset(
                                phylo,
                                level=subset_data["level"],
                                choice=subset_data["choice"],
                                remove=remove)
                        }
                        # if choice > 1 create more phylo objects and merge them
                        else {
                            # Select option
                            if(!remove) {
                                phylos <- list()
                                for(i in 1:length(x)) {
                                    merge_phylo <- taxa_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove)
                                    phylos[[i]] <- merge_phylo
                                    }
                                
                                phylo <- phylos[[1]]
                                for (i in 2:length(phylos))
                                    phylo <- merge_phyloseq(phylo, phylos[[i]])
                            }
                            else{
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
                        # Check if there are more than 1 selection
                        if (length(x) == 1) {
                            phylo <- sample_subset(
                                phylo,
                                level=subset_data["level"],
                                choice=subset_data["choice"],
                                remove=remove)
                        }
                        # if choice > 1 create more phylo objects and merge them
                        else {
                            if(!remove) {
                                phylos <- list()
                                for(i in 1:length(x)) {
                                    merge_phylo <- sample_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove)
                                    phylos[[i]] <- merge_phylo
                                }
                                phylo <- phylos[[1]]
                                for (i in 2:length(phylos))
                                    phylo <- merge_phyloseq(phylo, phylos[[i]])
                            }
                            else{
                                for(i in 1:length(x)){
                                    phylo <- sample_subset(
                                        phylo,
                                        level=subset_data["level"],
                                        choice=subset_data[paste0("choice",i)],
                                        remove=remove) 
                                }
                            }}

                }}}}
        phylo
    })
    
################################################################################
    # Update variable boxes when change tab
    observeEvent(input$tabswitch, {
        req(phylo())
        # Clear all boxes
        for (var in c("sample_var", "taxa_filter_level",
                      "taxa_filter_selection", "fill_var", 
                      "shape_var", "alpha_x_var",
                      "alpha_col_var", "alpha_shape_var")) {
            updateVarSelectizeInput(session, var,
                                    data=character(0),
                                    selected=character(0))
        }
        updateSelectizeInput(session, "alpha_measure_var", 
                             selected=character(0))
        
        # Heatmap
        updateVarSelectizeInput(session, 
                             "sample_var", 
                             data=sample_df(), selected=character(0))
        #Tax tree
        updateSelectizeInput(session, 
                          "taxa_filter_level", 
                          choices=colnames(taxa_df()), selected=character(0))
        
        # Beta diversity
        updateSelectizeInput(session, "type_var", 
                             choices=c("taxa", "samples",
                                       "biplot", "split"),
                             selected=character(0))
                             
        
        # Alpha diversity
        for (var in c("alpha_x_var", "alpha_col_var", "alpha_shape_var")) {
        updateVarSelectizeInput(session,
                             var,
                             data=sample_df(), selected=character(0))
        }
        
    })
    
################################################################################
    # Abundance tab
    
    # Heatmap object
    heat_plot <- reactive({
        # only works after selecting the sample label
        req(input$sample_var)
        
        chosen_var <- toString(input$sample_var)
        phylo <- phylo()
        
        heat_plot <- plot_heatmap(phylo, sample.label=chosen_var, 
                                  low="#66CCFF", high="#000033", 
                                  na.value="white")
    })
    
    # Heatmap display
    output$heat_plot <- renderPlotly({
        req(heat_plot())
        
        # Display plot
        ggplotly(heat_plot()
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")), tooltip=c(
                     "Sample", "OTU", "Abundance"))
    })
    
    # Heatmap download
    output$download_heatmap <- downloadHandler(
        filename="heatmap.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(heat_plot())
            dev.off()
        })
    
    # Taxonomy tree tab
    # Taxa filter
    observeEvent(input$taxa_filter_level, {
        req(input$taxa_filter_level)
        level <- toString(input$taxa_filter_level)
        if(level!=""){
        # Update the filter target
            updateSelectizeInput(session,
                              "taxa_filter_selection",
                              choices=unique(taxa_df()[,level]),
                              selected=character(0))
            }
    })
    # Create the tree object
    # eventReactive isolate the selection boxes, so the tree will be created only after clicking the button.
    tax_tree <- eventReactive(input$make_tree, {
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
        
        taxmap <- taxa::filter_taxa(taxmap, n_obs>=filter_num)
        
        # Display the tree
        heat_tree(taxmap,
                  node_label = taxon_names,
                  node_size = n_obs,
                  node_color=n_obs,
                  node_size_range = c(0.005, 0.05),
                  node_label_size_range = c(0.008, 0.04))
    })
    
    # Tree display
    output$tax_tree <- renderPlot({
        req(tax_tree())
        
        tax_tree()
        
    })
    
    # Download taxa tree in pdf
    output$download_tree <- downloadHandler(
        filename="taxa_tree.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(tax_tree())
            dev.off()
        })
    
    
################################################################################
    # Diversity tab
    
    # Alpha-diversity object
    alpha_div <- reactive({
        # Only works after selecting measure and x.
        req(input$alpha_measure_var, input$alpha_x_var)
        
        phylo <- phylo()
        
        trim_value <- input$alpha_slider 
        Alpha <- prune_taxa(taxa_sums(phylo) > trim_value, phylo) 
        
        x <- toString(input$alpha_x_var)
        
        # Color default will be black, unless selected by the user.
        col <- NULL
        if(!is.null(input$alpha_col_var)) {
            col <- toString(input$alpha_col_var)
        }
        # Shape defaul will be 19 (circle)
        shape <- NULL
        if(!is.null(input$alpha_shape_var)) {
            shape <- toString(input$alpha_shape_var)
        }
        alpha_div <- plot_richness(Alpha,
                                   x=x,
                                   color=col,
                                   shape=shape,
                                   measures=input$alpha_measure_var)
    })
    
    # Display alpha-diversity
    output$alpha <- renderPlotly({
        req(alpha_div())
        x <- toString(input$alpha_x_var)
        
        # Color default will be black, unless selected by the user.
        col <- NULL
        if(!is.null(input$alpha_col_var)) {
            col <- toString(input$alpha_col_var)
        }
        # Shape defaul will be 19 (circle)
        shape <- NULL
        if(!is.null(input$alpha_shape_var)) {
            shape <- toString(input$alpha_shape_var)
        }
        
        # Display plot
        ggplotly(alpha_div() 
                 + theme(plot.margin = unit(c(1, 1, 1, 1.5), "cm")),
                 tooltip=c(x, col, shape, "value")
                 )
    })
    
    # Alpha-diversity download
    output$download_alpha <- downloadHandler(
        filename="alpha_diversity.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(alpha_div())
            dev.off()
        })
    
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
