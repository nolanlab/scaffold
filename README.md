SCAFFoLD
========

# Installation

## Install a C++ compiler

You need to have a working C++ compiler to install SCAFFoLD. Please refer to the following steps for installing a compiler on your system

#### Mac OSX

You need to install the XCode software from Apple that is freely available on the App Store. Depending on the specific version of XCode you are using you might also need to install the "Command Line Tools" package separately. Please refer to the Documentation for your XCode version

#### Windows

Install the [Rtools](https://cran.r-project.org/bin/windows/Rtools/) package, which is required for building R packaged from sources

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

Once you have succesfully completed the steps above, start an R session and type the following commands

```
library(devtools)
install_github("nolanlab/scaffold")
```

This will install the SCAFFoLD R package together with all the required dependencies. If evertyhing was successful you should be able to start SCAFFoLD by typing the following commands

```
library(scaffold)
scaffold.run()
```
to stop SCAFFoLD simply hit the "ESC" key in your R session.

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

After you have specified all the parameters you can click on the "Start analysis" button. The run should be pretty quick and it will create a single .scaffold file with the same name of the dataset that you have used as reference. This is a single self-contained bundle that contains everything you need to browse the data. You can move it in any folder you want and also share with other users, without having to share any of the original files.

## Explore a SCAFFoLD map

Switch to the "Map exploration" tab by using the top navigation bar. This is a rundown of what the operation of the differnent controls:

1. **Choose a dataset**: use this drop-down to select a .scaffold file located in your current working directory
2. **Choose a graph**: the result of a single SCAFFoLD analysis typically contain multiple maps, one for each input dataset. This dropdown allows you to select the map you want to visualize.
3. **Nodes color**: use this dataset to color the nodes according to the expression of a specific marker, or with "Default" colors (unsupervised clusters:Blue, landmark populations:Red).
4. **Color scaling**: select whether you want the color scale of the nodes to be calculated globally for the whole dataset, or locally for the currently visualized graph.
5. **Nodes size**: select whether you want the size of the nodes to be proportional to the number of cells in each cluster. Presently the size scale is calculated across the entire dataset.
6. **Display edges**: select whether you want to display all the edges in the graph, or only the highest scoring one for each cluster. Even you if you are displaying all the edges you can visualize the highest scoring one for an individual cluster by hovering the mouse over the node.
7. **Reset graph**: this button will reset the graph to its initial position, which is intended to display most of the nodes in a single image
8. **Toggle landmark labels**: toggle the display of the landmark labels on/off
9. **Toggle cluster labels**: toggle the display of the cluster labels on/off
10. **Markers to plot in cluster view**: one of the most useful ways to inspect a cluster is to plot the distribution of expression values for the cells that comprise the cluster as compared to the cells that define the landmark nodes the cluster is connected to. This can help you understand what is similar and what is different between a cluster and a landmark population. Using this box you can select the markers you want to inspect. To generate the actual plot simply click on a cluster node. A plot of the markers distributions will then appear in the lower half of the window. The figure will contains multiple subplots, one for each marker. Each subplot consists of a distribution of expression values for the cells in the cluster and the cells in all the landmark nodes the cluster is connected to. The different distribution can be distinguished by line color, with a legend to the right of each plot.



