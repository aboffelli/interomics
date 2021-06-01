library(shiny)
library(plotly)
library(metacoder)
library(phyloseq)
library(ggplot2)
library(DT)

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

subset_types <- c("subset_type1", "subset_type2", "subset_type3")
