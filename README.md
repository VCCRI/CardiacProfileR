# CardiacProfileR
A R package for extraction and visualisation of heart rate dynamics profiles from wearable heart rate monitors

# Installation
You can install CardiacProfileR using the 'devtools' package:

    > install.packages("devtools")
    > library(devtools)
    > devtools::install_github("VCCRI/CardiacProfileR")
    
# Documentation
To view the full list of available functions use the following command:

    > help(package = "CardiacProfileR")

To view specific function documentation:

    > ?load_tcx_file
    > help("load_tcx_file")

# Usage
An example pipeline using CardiacProfileR has been provided in the demo/ directory on the VCCRI/CardiacProfileR github [link here](https://github.com/VCCRI/CardiacProfileR/blob/master/demo/pipeline.R).

To run the demo pipeline use the following command:
    
    > demo("pipeline", package = "CardiacProfileR")
