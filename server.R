
server <- function(input, output, session) {
    observeEvent(input$example, {
        data("GlobalPatterns")
    })
    
    # Upload tab
    # TODO: the taxa table must have only the taxa information, so there will be no indexing of the table.
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

    output$taxa_table <- DT::renderDataTable({
        req(phylo())
        phylo()@tax_table
        },
        rownames=TRUE)
            
    
    output$otu_table <- DT::renderDataTable({
        req(phylo())
        phylo()@otu_table
    }, rownames=TRUE)
    
    output$sample_table <- DT::renderDataTable({
        req(phylo())
        phylo()@sam_data
    }, rownames=TRUE)
    
    #Download tables
    output$download_subset <- downloadHandler(
        filename="interomics_tables.tar",
        content=function(filename) {
            
            write.table(phylo()@tax_table, file="taxa_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(phylo()@otu_table, file="otu_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            write.table(phylo()@sam_data, file="sample_table.txt",
                        sep="\t", quote=FALSE, na="", col.names=NA)
            
            tar(filename, files=c("taxa_table.txt", 
                                      "otu_table.txt", 
                                      "sample_table.txt"))
            file.remove(c("taxa_table.txt", 
                          "otu_table.txt", 
                          "sample_table.txt"))
        })
    
    # Subset variables
    observe({
        req(taxa_df(), otu_df(), sample_df())
        for(var in subset_types) {
            updateSelectInput(session, var,
                              choices=c("Taxa", "Sample"),
                              selected=FALSE)
        }
    })
    # Subset variables level1
    observeEvent(input$subset_type1, {
        req(input$subset_type1)
        # Taxa table
        if(toString(input$subset_type1)=="Taxa") {
            updateSelectInput(session, "subset_level1", 
                              choices=colnames(taxa_df()),
                              selected=FALSE)
        }
        # Sample table
        else {
            updateSelectInput(session, "subset_level1", 
                              choices=colnames(sample_df()),
                              selected=FALSE)
        }
    })
    
    # Subset variable choice1
    observeEvent(input$subset_level1, {
        req(input$subset_level1)
        level <- toString(input$subset_level1)
        # Taxa table
        if(toString(input$subset_type1)=="Taxa")
            updateSelectInput(session,
                              "subset_choice1",
                              choices=unique(taxa_df()[,level]),
                              selected=FALSE)
        # Sample table
        else {
            updateSelectInput(session,
                              "subset_choice1",
                              choices=unique(sample_df()[,level]),
                              selected=FALSE)
        }
    })
    
    # Subset variables level2
    observeEvent(input$subset_type2, {
        req(input$subset_type2)
        # Taxa table
        if(toString(input$subset_type2)=="Taxa") {
            updateSelectInput(session, "subset_level2", 
                              choices=colnames(taxa_df()),
                              selected=FALSE)
        }
        # Sample table
        else {
            updateSelectInput(session, "subset_level2", 
                              choices=colnames(sample_df()),
                              selected=FALSE)
        }
    })
    
    # Subset variable choice2
    observeEvent(input$subset_level2, {
        req(input$subset_level2)
        level <- toString(input$subset_level2)
        # Taxa table
        if(toString(input$subset_type2)=="Taxa")
            updateSelectInput(session,
                              "subset_choice2",
                              choices=unique(taxa_df()[,level]),
                              selected=FALSE)
        # Sample table
        else {
            updateSelectInput(session,
                              "subset_choice2",
                              choices=unique(sample_df()[,level]),
                              selected=FALSE)
        }
    })
    
    # Subset variables level3
    observeEvent(input$subset_type3, {
        req(input$subset_type3)
        # Taxa table
        if(toString(input$subset_type3)=="Taxa") {
            updateSelectInput(session, "subset_level3", 
                              choices=colnames(taxa_df()),
                              selected=FALSE)
        }
        # Sample table
        else {
            updateSelectInput(session, "subset_level3", 
                              choices=colnames(sample_df()),
                              selected=FALSE)
        }
    })
    
    # Subset variable choice3
    observeEvent(input$subset_level3, {
        req(input$subset_level3)
        level <- toString(input$subset_level3)
        # Taxa table
        if(toString(input$subset_type3)=="Taxa")
            updateSelectInput(session,
                              "subset_choice3",
                              choices=unique(taxa_df()[,level]),
                              selected=FALSE)
        # Sample table
        else {
            updateSelectInput(session,
                              "subset_choice3",
                              choices=unique(sample_df()[,level]),
                              selected=FALSE)
        }
    })
    
    phylo <- reactive({
        req(taxa_df(), otu_df(), sample_df())
        
        phylo <- create_phylo(taxa=taxa_df(),
                              otu=otu_df(),
                              sample=sample_df())
        
        if(input$use_subset) {
            # TODO: FIX - subset only works if there is no NA in the column
            subset_data <- list(c(type=input$subset_type1, 
                                  level=input$subset_level1,
                                  choice=input$subset_choice1,
                                  remove=input$subset_remove1),
                                c(type=input$subset_type2, 
                                  level=input$subset_level2,
                                  choice=input$subset_choice2,
                                  remove=input$subset_remove2),
                                c(type=input$subset_type3, 
                                  level=input$subset_level3,
                                  choice=input$subset_choice3,
                                  remove=input$subset_remove3))
            
            for(i in 1:3){
                if(subset_data[[i]]["choice"]!=""){
                    if (subset_data[[i]]["type"] == "Taxa") taxa <- TRUE
                    else taxa <- FALSE
                    if (subset_data[[i]]["remove"] == "Select") remove <- FALSE
                    else remove <- TRUE
                    
                    phylo <- subset_func(phylo=phylo, 
                                         level=subset_data[[i]]["level"], 
                                         choice=subset_data[[i]]["choice"], 
                                         taxa=taxa,
                                         remove=remove)
                    
                }}}
        phylo
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
        for (var in c("fill_var", "shape_var")) {
            updateVarSelectInput(session,
                                 var, 
                                 data = x, selected=FALSE)
        }
        
        # Alpha diversity
        for (var in c("alpha_x_var", "alpha_col_var", "alpha_shape_var")) {
        updateVarSelectInput(session,
                             var,
                             data=sample_df(), selected=FALSE)
        }
        
    })
    
###############################################################################
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
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
                 )   
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
        
        # Update the filter target
        updateSelectInput(session,
                          "taxa_filter_selection",
                          choices=unique(taxa_df()[,level]),
                          selected=FALSE)
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
    
    
###############################################################################
    # Graphics tab
    
    # Biplot object
    biplot <- reactive({
        # only works after selecting fill and shape
        req(input$fill_var, input$shape_var)
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))
        
        phylo <- phylo()
        
        
        biplot <- create_biplot(phylo, 
                                fill=chosen_var[1], 
                                shape=chosen_var[2]) +
            scale_shape(solid=FALSE)
    })
    # Display biplot
    output$biplot <- renderPlotly({
        req(biplot())
        chosen_var <- c(toString(input$fill_var), toString(input$shape_var))
        ggplotly(biplot()
                 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
                 tooltip=c(chosen_var[1],
                           chosen_var[2], 
                           "NMDS1", 
                           "NMDS2"))
    })
    
    # Biplot download
    output$download_biplot <- downloadHandler(
        filename="biplot.pdf",
        content=function(file){
            pdf(file, width=12, height=10)
            print(biplot())
            dev.off()
        })
    
    
    # Alpha-diversity object
    alpha_div <- reactive({
        # Only works after selecting measure and x.
        req(input$alpha_measure_var, input$alpha_x_var)
        
        phylo <- phylo()
        
        Alpha <- prune_taxa(taxa_sums(phylo) > 10, phylo) 
        
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
}
