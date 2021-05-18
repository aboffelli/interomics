# README - Interomics

## About the program



## Installation

The program is written in R (4.0.3), and uses the following packages:

- shiny 1.6.0
- metacoder 0.3.4
- phyloseq 1.34.0
- plotly 4.9.3
- ggplot2 3.3.3
- BiocManager 1.30.12

### Installing packages

To install the necessary package open the R console and use the lines below.

```R
# Run each line separately.
packages <- c("shiny", "metacoder", "ggplot2", "plotly", "BiocManager")
for(package in packages) install.packages(package)
BiocManager::install("phyloseq", ask=FALSE)
```

### Running the program

After the installation of the necessary packages, make sure that both files (*ui.R* and *server.R*) are in the same directory. You can run the program with the following line in the R console.

```R
# The path must point to the whole directory. The path below is an example, you may need to change it according to where the files are saved.
shiny::runApp("C:/User/Example/Interomics")
```



## File Upload

The files can be uploaded in the initial page (File Upload tab), where three files are required:

- OTU counts for each sample. The first column must contain the OTU name/number. The first row must contain the sample names.
- Taxonomic table for each OTU. The first column must contain the OTU name/number. The first row must contain the taxonomic levels.
- Sample data. The first column must contain the sample names. The first row will be read as a header.

The accepted formats are: text (txt), comma-separated-values (csv). The type of separator can be defined in the File Upload page, the options are tab, comma, and semicolon. Each file will be loaded in the right panel, where the number of rows displayed can be controlled with the slider in the left panel.

It is possible to load an example dataset selecting the checkbox under the slider. When selected, the three example tables will be loaded and displayed.

All the plots can only be generated after uploading all three files or using the example dataset. When all tables are loaded correctly you can continue for the Abundance and Graphics tabs. 



## Abundance

### Heatmap

To load the heatmap, it is necessary to select a label for the samples. 

## Graphics

In the Graphics tab you can visualize the Biplot and the Alpha-Diversity graphics.

### Biplot

Both variables, color and shape, are required to create the Biplot. You can choose any combination of variables.

### Alpha-Diversity

Only the "X" variable and the "Measures" are required for the Alpha-Diversity plot. The "Color" variable is optional, if not selected, all samples will be colored black. You can select multiple types of measure, each of them will be displayed side by side.