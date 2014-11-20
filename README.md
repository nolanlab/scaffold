SCAFFoLD
========

# Installation

## Install a C++ compiler

You need to have a working C++ compiler to install SCAFFoLD. Please refer to the following steps for installing a compiler on your system

#### Mac OSX

You need to install the XCode software from Apple that is freely available on the App Store. Depending on the specific version of XCode you are using you might also need to install the "Command Line Tools" package separately. Please refer to the Documentation for your XCode version

#### Windows

Install Visual Studio. The Express version is freely available from Microsoft

#### Linux

Install GCC. Refer to the documentation of your distribution to find the specific package name

## Install required R packages

You need to install the devtools package, available from CRAN, and the flowCore package from Bioconductor. The rest of the dependencies for SCAFFoLD will be automatically installed

#### Devtools

Open an R session, type the following command and select a CRAN mirror when prompted.

`install.packages("devtools")`

#### FlowCore

Open an R session and type the following commands

```
source("http://bioconductor.org/biocLite.R")
biocLite("flowCore")
```

## Install SCAFFoLD

Once you have succesfully completed the steps above, you have to create a Github token by following [these instructions.](https://help.github.com/articles/creating-an-access-token-for-command-line-use/) (This won't be necessary anymore when the repository goes public).
Copy the token, start an R session and type the following commands, substituing your Github token

```
library(devtools)
install_github("nolanlab/scaffold", auth_token = "YOUR TOKEN HERE")
```

This will install the SCAFFoLD R package together with all the required dependencies. If evertyhing was successful you should be able to start SCAFFoLD by typing the following commands

```
library(scaffold)
scaffold.run()
```

# Usage

When you launch the GUI you will be prompted to select a file. You can select any file in what you want to be your working directory and this will set the working directory for the remainder of the session. SCAFFoLD will only look at files in your working directory, so everything you need must be there. Also if you add files to this directory you will need to restart the interface in order to see them in the dropdown menus. The first step of the analysis is to cluster the FCS files.

## Clustering

Select the "Run clustering" tab from the navigation bar at the top. In the clustering tab select a representative FCS file and then select the markers that you want to use for the clustering. Hit start clustering and wait for the procedure to complete. For each FCS files two files will be created:

1. your-fcs-file.clustered.txt: this file contains the marker medians for each cluster
2. your-fcs-file.clustered.all_events.RData: this file is an RData object which contains all the events in the original FCS file but with an added column that specifies the cluster membership. **The data in this file is arcsinh transformed**

The clustering is the only computationally intensive part of a SCAFFoLD analysis. Luckily this only needs to be run once as you can simply reuse these files to build multiple maps

## Construct a SCAFFoLD map

Switch to the "Run SCAFFoLD Analysis" tab by using the top navigation bar. Using the first drop-down menu select the dataset that will act as the reference (The menu will only contain .clustered.txt files that are located in the current working directory). After you have chosen the markers that you want to use for the analysis select Gated as the running mode. This will use any number of gated populations as landmark nodes in the graph (Red nodes). The position of the landmark nodes will be constant across all the graphs you generate and will provide a visual reference that will allow you to compare the different datasets across each other. 

The gated populations have to be provided as single FCS files (one for each population) that need to be located in a subdirectory called "gated" of the current working directory. The program will split the name of the FCS file using "_" as separator and the last field will be used as the population name. For instance if you want an FCS file to define your "B cells" population you have to use the following naming scheme:

*WhateverYouWant*_B cells.fcs

If you check the "Add inter-cluster connections" checkbox your graph will also include connections between the unsupervised clusters (Blue nodes). The default is for the unsupervised clusters (Blue nodes) to be connected only to the landmark populations (Red nodes). Please note that this feature is still experimental.

After you have specified all the parameters you can click on the "Start analysis" button. The run should be pretty quick and it will create a single .scaffold file with the same name of the dataset that you have used as reference.




