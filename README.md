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

To install the necessary package open the R console and use the commands

```r
install.packages("shiny")
install.packages("metacoder")
install.packages("ggplot2")
install.packages("plotly")
install.packages("BiocManager")
BiocManager::install("phyloseq")

```



## File Upload

You can select a file to upload in the "File Upload" tab, clicking in "Browse...". The accepted file types are: plain text (txt), comma separated (csv), tab delimited. 

The separator can be selected between: tab, comma, and semicolon. The "Header" checkbox interprets the first line of the file as the header when activated.

The file content will be displayed on the side after being selected. A slide bar is provided to select the number of rows that are displayed. To display the entire file, select the "Display all" checkbox.

