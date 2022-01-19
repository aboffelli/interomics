## -----------------------------------------------------------------------------
##
## Script name: global.R
##
## Author: Arthur Boffelli Castro
##
## Date created: 2021-05-25
##
## GitHub: https://github.com/aboffelli/interomics
##
## Description:
##  Script containing all the packages and functions used by Interomics.
##  
## -----------------------------------------------------------------------------
## 
## Notes:
##  This script is one of three scripts used to run Interomics (global.R, ui.R 
##    and server.R), the three scripts must be in the same directory to run the 
##    program.
##  
## -----------------------------------------------------------------------------

# Load all packages needed.
library(shiny)
library(plotly)
library(metacoder)
library(phyloseq)
library(DT)

# Set a new theme for ggplot, this changes the theme of all plots created.
theme_set(theme_bw())


# Function to create a phylo object using phyloseq.
create_phylo <- function(taxa, otu, sample) {
    ## -------------------------------------------------------------------------
    ## This function creates a phylo object that will be used in all phyloseq
    ##  functions to create the plots. The arguments contain three tables with:
    ##  taxonomic classification, OTU counts, and sample information, 
    ##  respectively.
    ## -------------------------------------------------------------------------
    
    OTU <- otu_table(otu, taxa_are_rows=TRUE)
    TAXA <- tax_table(taxa)
    SAMPLE <- sample_data(sample)
    phylo <- phyloseq(OTU, TAXA, SAMPLE)
    return(phylo)
}


# Function to create a beta-diversity plot.
create_beta <- function(phylo_object, type, fill, shape) {
    ## -------------------------------------------------------------------------
    ## This function creates an ordination for the Beta-diversity plot, using 
    ##  NMDS and bray for the calculation. The ordination is then used to build 
    ##  the Beta-diversity plot. The arguments contain the phyloseq object 
    ##  created previously, type of plot, color, and shape.
    ## ------------------------------------------------------------------------- 
    phylo.ord <- ordinate(phylo_object, "NMDS", "bray")
    Beta <- plot_ordination(phylo_object, phylo.ord, 
                              type=type, color=fill, 
                              shape=shape)
    return(Beta)
}


# Function to generate the taxonomic tree.
create_taxmap <- function(taxa, otu) {
    ## -------------------------------------------------------------------------
    ## This function creates the taxonomic tree object using the metacoder 
    ##  package. The arguments contain the taxonomic classification table and 
    ##  the OTU counts.
    ## ------------------------------------------------------------------------- 
    
    # Create a new column contain all the taxonomic levels for each OTU 
    # separated by ";". This is the format required for metacoder.
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
    
    # Create the tax map using the new OTU table.
    taxmap <- parse_tax_data(taxonomic_df,
                             class_cols="Taxonomy", 
                             class_sep=";")
    names(taxmap$data) <- "otu_counts"
    return(taxmap)
}


# Functions to subset the phylo object
taxa_subset <- function(phylo, level, choice, remove) {
    ## -------------------------------------------------------------------------
    ## Function that removes or isolates specific organisms from the from the 
    ##  taxonomic table. The arguments contain the phyloseq object, the 
    ##  taxonomic level that contains the selected organism, and a boolean to
    ##  either remove or isolate the organism.
    ## -------------------------------------------------------------------------
    
    # If the boolean is False, the operator is be equals to, since we are 
    # selecting the organism
    if(!remove) x <- parse(text=paste0(level,"==","'",choice,"'"))
    # If the boolean is True, the operator is not equals to, since we are 
    # removing the organism.
    else x <- parse(text=paste0(level,"!=","'",choice,"'"))
    # Use the previous variable to subset the old table and create a new one.
    oldTax <- data.frame(tax_table(phylo))
    newTax <- subset(oldTax, eval(parse(text=x)))
    tax_table(phylo) <- tax_table(as.matrix(newTax))
    return(phylo)
}


sample_subset <- function(phylo, level, choice, remove) {
    ## -------------------------------------------------------------------------
    ## Function that removes or isolates specific organisms from the from the 
    ##  sample table. The arguments contain the phyloseq object, the 
    ##  sample information level that contains the selected organism, and a 
    ##  boolean to either remove or isolate the organism.
    ## -------------------------------------------------------------------------
    
    # If the boolean is False, the operator is be equals to, since we are 
    # selecting the organism
    if(!remove) x <- parse(text=paste0(level,"==","'",choice,"'"))
    # If the boolean is True, the operator is not equals to, since we are 
    # removing the organism.
    else x <- parse(text=paste0(level,"!=","'",choice,"'"))
    # Use the previous variable to subset the old table and create a new one.
    oldSam <- data.frame(sample_data(phylo))
    newSam <- subset(oldSam, eval(parse(text=x)))
    sample_data(phylo) <- sample_data(data.frame(newSam))
    return(phylo)
}
    
# Vectors used for loops in the Subset section of server.R script.
subset_types <- c("subset_type1", "subset_type2", "subset_type3")
subset_levels <- c("subset_level1", "subset_level2", "subset_level3")
subset_choices <- c("subset_choice1", "subset_choice2", "subset_choice3")
subset_removes <- c("subset_remove1", "subset_remove2", "subset_remove3")