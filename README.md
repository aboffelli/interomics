# README - Interomics

## About the program

***Interomics*** is a application created to facilitate the visualization of metagenomic data. It is an user-friendly environment, where the user can easily: subset the data that is being calculated; select the variables that will be used to generate the plot, changing it instantly; visualize interactive plots – heatmap, biplot, alpha-diversity – where you can obtain information by hovering the mouse over the plot, zoom in or out, isolate or hide groups in the plot based on specific characteristics; generate a taxonomic tree.   

## Installation

The program is written in R (4.0.3), and uses the following packages:

- shiny 1.6.0
- metacoder 0.3.4
- phyloseq 1.34.0
- plotly 4.9.3
- ggplot2 3.3.3
- BiocManager 1.30.12

### Installing packages

#### Conda

Using Conda you can create a new environment with all necessary packages with the following code on the terminal.

```bash
# Creates a conda env and downloads all the necessary packages
conda create -n interomics -c bioconda -c conda-forge r-base=4.0.3 r-dt bioconductor-phyloseq r-plotly r-ggplot2 r-metacoder r-shiny
# Activate the environment
conda activate interomics
# Start the R console in the terminal
R
```

#### R console/Rstudio

To install the necessary package open the R console/Rstudio and use the lines below.

```R
# Run each line separately.
packages <- c("shiny", "metacoder", "ggplot2", "plotly", "BiocManager", "DT")
for(package in packages) install.packages(package)
BiocManager::install("phyloseq", ask=FALSE)
```



### Running the program

The program consists in three files – *ui.R*, *server.R*, and *global.R* – that must be stored together in the same directory.

After the installation of the necessary packages and making sure that the three files (*ui.R*, *server.R*, and *global.R*) are together, you can run the program with the following line in the R console.

```R
# The path below is an example, you may need to change it according to where the files are saved. 
# It must point to the whole directory containing the three files mentioned before. 
shiny::runApp("C:/User/Example/Interomics")
```



## File Upload

The files can be uploaded in the initial page (File Upload tab), where three files are required:

- OTU counts for each sample. The first column must contain the OTU name/number. The first row must contain the sample names.
- Taxonomic table for each OTU. The first column must contain the OTU name/number. The first row must contain the taxonomic levels.
- Sample data. The first column must contain the sample names. The first row will be read as a header.

The accepted formats are: plain-text (txt), comma-separated-values (csv), tab-delimited (tab/tsv). The type of separator can be defined in the File Upload page, the options are tab, comma, and semicolon.

It is possible to load an example dataset selecting the checkbox "Load an example dataset" under the slider. When selected, the three example tables will be loaded and displayed.

### Subsetting the data (optional)

The subset will affect all the plots, with the exception of the Taxonomic Tree, that has its own filtering options.

First, select if the target group will be isolated or removed from the data, using the radio buttons above the set of three boxes that you will use. Choose whether the group is in the Taxa or Sample tables to unlock the options in the level box, according to the table chosen. In the level box, choose the taxonomic level/column where the target group is located. Finally, select the target group in the last box.

You can have up to three subsets simultaneously. You can use any box set, however, keep in mind that the subset will follow the order in the screen. So, be careful of isolating a group that was removed in a previous box set.

To use the subsetted data, select the checkbox "Use subsetted data". The program will not use the filtered data if this checkbox is not selected, even if you chose the groups in the boxes.

You can download the subsetted tables, as tab delimited text files. The button "Download subsetted data" will download a tar file containing the three files.



The tables will be displayed in the right side, and changed if the subset option is activated. The tables are interactive, which means that you can: choose the number of entries to show, sort the rows based on the values of the column in ascending or descending order, search for an specific value.



## Abundance

### Heatmap

To load the heatmap, it is necessary to select a label for the samples.

The heatmap is interactive, which means that you can zoom in by selecting an area inside the plot with the mouse, zoom out double clicking. Hovering the mouse over the plot gives you the information about the sample name, the OTU number, and the abundance. In the top right corner of the plot you will find some command buttons, including: save the plot as a png image, selection box, zoom in and out, reset axes, etc. Click the button "Download" to download the original plot as pdf – any interactive change, such as zoom or hidden objects, will not be present.

### Taxonomic Tree

In the Taxonomic Tree tab, you will find a slide bar where you can select the minimum abundance to be considered in the tree, as well as a filtering option similar to the subset area. To use the filter option, first select the taxonomy level that the target group is located, all the possible groups in that level will be available to selection in the next box. As the data can be heavy, the image will only be created after clicking the button "Create tree". The image may take a while to be displayed.

To download the taxonomic tree in pdf format, click the button "Download".  

## Function

### Biplot

Both variables, color and shape, are required to create the Biplot. You can choose any combination of variables.

The biplot is interactive, which means that you can zoom in selecting an area inside the plot with the mouse, zoom out double clicking. Hovering the mouse over the plot gives you the information about the selected variables and the positions NMDS1 and NMDS2. You can hide a group by clicking on the legend corresponding to that group in the right side, or isolate a group by double clicking in the legend corresponding to the group. In the top right corner of the plot you will find some command buttons, including: save the plot as a png image, selection box, zoom in and out, reset axes, etc. Click the button "Download" to download the original plot as pdf – any interactive change, such as zoom or hidden objects, will not be present.

### Alpha-Diversity

Only the "X" variable and the "Measures" are required for the Alpha-Diversity plot. The "Color" and "Shape" variables are optional, if not selected, all samples will be colored black and shaped as circles. You can select multiple types of measure, each of them will be displayed side by side.

The alpha-diversity plot is interactive, which means that you can zoom in selecting an area inside the plot with the mouse, zoom out double clicking. Hovering the mouse over the plot gives you the information about the selected variables and value of the measure selected. You can hide a group by clicking on the legend corresponding to that group in the right side, or isolate a group by double clicking in the legend corresponding to the group. In the top right corner of the plot you will find some command buttons, including: save the plot as a png image, selection box, zoom in and out, reset axes, etc. Click the button "Download" to download the original plot as pdf – any interactive change, such as zoom or hidden objects, will not be present.