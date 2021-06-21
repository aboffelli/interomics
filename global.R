library(shiny)
library(plotly)
library(metacoder)
library(phyloseq)
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
create_beta <- function(phylo_object, type, fill, shape) {
    phylo.ord <- ordinate(phylo_object, "NMDS", "bray")
    Beta <- plot_ordination(phylo_object, phylo.ord, 
                              type=type, color=fill, 
                              shape=shape)
    return(Beta)
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

# Functions to subset the phylo object
taxa_subset <- function(phylo, level, choice, remove) {
    if(!remove) x <- parse(text=paste0(level,"==","'",choice,"'"))
    else x <- parse(text=paste0(level,"!=","'",choice,"'"))
    oldTax <- data.frame(tax_table(phylo))
    newTax <- subset(oldTax, eval(parse(text=x)))
    tax_table(phylo) <- tax_table(as.matrix(newTax))
    return(phylo)
}

sample_subset <- function(phylo, level, choice, remove) {
    if(!remove) x <- parse(text=paste0(level,"==","'",choice,"'"))
    else x <- parse(text=paste0(level,"!=","'",choice,"'"))
    oldSam <- data.frame(sample_data(phylo))
    newSam <- subset(oldSam, eval(parse(text=x)))
    sample_data(phylo) <- sample_data(data.frame(newSam))
    return(phylo)
}
    

subset_types <- c("subset_type1", "subset_type2", "subset_type3")
subset_levels <- c("subset_level1", "subset_level2", "subset_level3")
subset_choices <- c("subset_choice1", "subset_choice2", "subset_choice3")
subset_removes <- c("subset_remove1", "subset_remove2", "subset_remove3")