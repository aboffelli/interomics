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

```r
packages <- c("shiny", "metacoder", "ggplot2", "plotly", "BiocManager")
for(package in packages) install.packages(package)
BiocManager::install("phyloseq", ask=FALSE)
```

### Running the program

After the installation of the necessary packages, make sure that both files (*ui.R* and *server.R*) are in the same directory. You can run the program with the following line in the R console.

```R
# Remember that you may need to change the path to the directory according to where the files are saved.
shiny::runApp("~/Interomics")
```



## File Upload

The files can be uploaded in the initial page (File Upload tab), where three files are required:

- OTU counts for each sample. The first column must be the OTU name/number. The first row the sample names.
- Taxonomic table for each OTU. The first column must be the OTU name/number. The first row the taxonomic levels.
- Sample data. The first column must be sample names. The first row as a header.

The accepted formats are: text (txt), comma-separated-values (csv). The type of separator can be defined in the File Upload page, the options are tab, comma, and semicolon. The "Header" checkbox interprets the first line of the file as the header when activated.

Each file will be loaded in the right panel, where the number of rows displayed can be controlled with the slider in the left panel.

It is possible to load an example dataset selecting the checkbox under the slider. When selected, the three tables will be displayed.

When all tables are displayed correctly you can continue for the Abundance and Graphics tabs. 

