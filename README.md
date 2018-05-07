# CardiacProfileR
An R package for extraction and visualisation of heart rate profiles from wearable fitness monitors

The widespread availability of affordable wearable heart rate monitors has enabled collection of personalised heart rate data possible. Here we present an open source R package CardiacProfileR for extraction, analysis and visualistion of heart rate dynamics upon physical exertions from data generated from common wearable heart rate monitors such as Fitbit, Garman and AppleWatch. This package represents a first step in large-scale extractiion of a key personlised cardiovascular phenotype from modern wearable devices.

![](plot3d.PNG =400x)

# Installation
You can install CardiacProfileR using the 'devtools' package:

    > install.packages("devtools")
    > library(devtools)
    > devtools::install_github("VCCRI/CardiacProfileR")
    
# Usage
An example pipeline using CardiacProfileR has been provided in the demo/ directory on the VCCRI/CardiacProfileR github [link here](https://github.com/VCCRI/CardiacProfileR/blob/master/demo/pipeline.R).

To run the demo pipeline use the following command:
    
    > demo("pipeline", package = "CardiacProfileR")    
    
# Documentation
To view the full list of available functions use the following command:

    > help(package = "CardiacProfileR")

To view specific function documentation:

    > ?load_tcx_file
    > help("load_tcx_file")

