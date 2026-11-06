# FCS-Merge

## Description

FCS-Merge is a Shiny application designed to merge Flow Cytometry Standard (FCS) files from raw and processed datasets.

The application allows users to:
  
  - Upload raw FCS files.
- Select processed FCS files from different methods.
- Select parameters to include in the merge.
- Merge FCS files.
- Visualize merged data using histograms and 2D density plots.
- Apply an arcsinh transformation.
- Download merged FCS files.

## Requirements

R (version 4.0 or higher recommended).

## Required R packages

Install the required packages using:
  
  ```r
install.packages(c(
  "shiny",
  "shinyFiles",
  "DT",
  "rmarkdown",
  "knitr",
  "ggplot2",
  "dplyr",
  "tidyr",
  "shinyjs",
  "rsconnect",
  "shinydashboard",
  "shinythemes",
  "stringdist",
  "base64enc"
))

install.packages("remotes")
remotes::install_github("cipheLab/FlowCIPHE")

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("flowCore")
```

Additional packages may be required depending on the server-side processing and visualization scripts.

## Installation

Clone the repository:
  
  ```bash
git clone https://github.com/cipheLab/FCS-Merge.git
```

Open the `FCS-Merge.Rproj` file in RStudio.

## Usage

1. Install the required R packages.
2. Open the project in RStudio.
3. Run the Shiny application.
4. Upload your raw FCS files.
5. Add the processed FCS folders corresponding to the different methods.
6. Select the parameters to include.
7. Run the merge.
8. Visualize and download the merged FCS files.

## Application modes

The application supports two modes:
  
  - **Upload mode (default):** Users upload their raw FCS files directly through the interface.
- **Local mode:** Users select a local RAW folder using the directory selection interface.

The mode can be selected using the environment variable:
  
  For local mode:
  
  ```r
Sys.setenv(FCS_MERGE_MODE = "local")
```

For upload mode:
  
  ```r
Sys.setenv(FCS_MERGE_MODE = "upload")
```
