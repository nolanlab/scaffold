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

*Note*: the latest version of devtools seems to be occasionally having problems installing dependencies on windows. If the installation of SCAFFoLD fails for a missing package, please install the offending packages manually, using the R *install.packages* function

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

This is a rundown of the different options:

- **Running mode**: keep this as "Gated"
- **Edge weight influence**: this option controls the importance assigned to the edge weights in constructing the force directed layout, and is related to the workings of the ForceAtlas2 algorithm. *Proportional* means that edge weight is proportional to the number of parameters used to construct the graph, *Fixed* will allow you to select a specific value explicitly
- **Add inter-cluster connections**: if this is checked, the graph will also include connections between the unsupervised clusters (Blue nodes). The default is for the unsupervised clusters (Blue nodes) to be connected only to the landmark populations (Red nodes). It is **highly recommended** to check this option.  
- **asinh cofatctor**: the cofactor used for the asinh transformation. The recommended values are 5 for mass cytometry data, and 150 for fluorescence-based flow cytometry.

After you have specified all the parameters you can click on the "Start analysis" button. The run should be pretty quick and it will create a single .scaffold file with the same name of the dataset that you have used as reference. This is a single self-contained bundle that contains everything you need to browse the data. You can move it in any folder you want and also share with other users, without having to share any of the original files.

## Explore a SCAFFoLD map

Switch to the "Map exploration" tab by using the top navigation bar. This is a rundown of the operation of the different controls:

- **Choose a dataset**: use this drop-down to select a .scaffold file located in your current working directory
- **Choose a graph**: the result of a single SCAFFoLD analysis typically contain multiple maps, one for each input dataset. This dropdown allows you to select the map you want to visualize.

You can interact with the graph using the mouse as follows (node selections are used for plotting and exporting data, see below):

- Scrolling: zoom in/out. 
- Left click + drag: panning
- Click on a node: select the node
- Click on a node + Shift key: add to the current selection
- Left click + drag + Alt key: select all nodes inside a rectangle

The table to the right of the graph shows statistics about either the entire graph, or the currently selected nodes. In the former case, the table shows statistics related to the number of cells for which each landmark in the *Landmark* column is the closest landmark. Conversely,  when one or more nodes are selected, the table shows statistics related to the individual clusters.

- **Nodes color**: use this dataset to color the nodes according to the expression of a specific marker, or with "Default" colors (unsupervised clusters:Blue, landmark populations:Red).
- **Color scaling**: select whether you want the color scale of the nodes to be calculated globally for the whole dataset, or locally for the currently visualized graph.
- **Nodes size**: select whether you want the size of the nodes to be proportional to the number of cells in each cluster. Presently the size scale is calculated across the entire dataset.
- **Display edges**: select whether you want to display all the edges in the graph, or only the highest scoring one for each cluster. Even you if you are displaying all the edges you can visualize the highest scoring one for an individual cluster by hovering the mouse over the node.
- **Minimum / Maximum / Landmark node size**: the minimum and maximum size for the cluster (blue) and landmark (red) nodes
- **Display edges**: whether to display all the edges, only the highest scoring ones (i.e. for each cluster node, only the highest scoring connection), only the ones between the clusters themselves, or only the ones between clusters and landmarks.
- **Reset graph position**: this button will reset the graph to its initial position, which is intended to display most of the nodes in a single image
- **Toggle landmark labels**: toggle the display of the landmark labels on/off
- **Toggle cluster labels**: toggle the display of the cluster labels on/off
- **Export selected clusters**: click this button to export the events in the selected clusters in a separate FCS file. For this to work, the original RData files corresponding to the clustered files in use must be located in the working directory. A new FCS file will be created in the working directory, with a name starting with *scaffold_export*, and ending with a random string of alpha-numeric characters, to prevent naming conflicts.

One of the most useful ways to inspect a cluster is to plot the expression values for the cells that comprise the cluster as compared to the cells that define the landmark nodes the cluster is connected to. This can help you understand what is similar and what is different between a cluster and a landmark population. The plot generated with the options below will therefore contain all the selected clusters, and all the landmarks these clusters are connected to.

- **Plot selected clusters**: plot the selected clusters, using the plot type specified in the **Plot type** dropdown menu. The plots appear at the very bottom of the page.
- **Pool cluster data**: for plotting, pool all the data from the selected clusters. If the option is not selected, each cluster will be plotted individually as a separate boxplot, or density plot. Selecting this option will pool all the clusters data together for plotting.
- **Plot type**: the type of plot to display. Either a boxplot, a density plot, or a scatteplot (biaxial). For the latter plot type only the first two selected markers (see below) will be used, corresponding to the x and y axes.
- **Markers to plot in cluster view**: Select which markers you want to display in the plot.

## Map data onto an existing reference

Instead of starting a new map from scratch, you can map a set of clustered files onto an already existing scaffold analysis. This will generate a map with the same layout and the same landmarks as the original analysis. In order to do this select a reference *.scaffold* file from the left dropdown, and one of the sample clustered files from the right one (all files need to be located in the current working directory).

You can then use the two boxes to select the markers to be included in the mapping. Markers will appear in the gray area below, in the order in which you have chosen them.

**Important**: the markers will be mapped  between the two datasets in the order displayed in the two gray boxes. The first marker in the right box, will be considered equivalent to the first marker in the left box, the second to the second and so on. It is therefore *extremely important* that the markers appear in the correct order in the gray boxes. You can drag and drop on the markers to rearrange the order, or simply input them in a different order to begin with (the latter is usually more convenient).

This is a rundown of the two options:

- **Overlap resolution method**: this controls how overlaps between the nodes are resolved in the finaly layout. *Repel* corrseponds to the standard ForceAtlas2 implementation, nodes are applied a strong repulsive force when they are on top of each other, which causes them not to overlap in the final layout. This may be problematic when a lot of nodes are clustered around a central hub, because the requirement that they do not overlap ends up completely dominating the layout, so that the nodes essentially become arranged in a grid. If that happens you can select the *expand* option. With this mode, instead of the nodes repelling, the whole graph is expanded linearly, until the nodes don't overlap anymore. This gives a much more accurate positioning of the nodes in these cases.
- **Add inter-cluster connections**: whether you want to add connections between the clusters (see above in the section **Construct a SCAFFoLD map**). It is *highly recommended* to check this option. If the option is checked, two additional options will appear
- **Markers for inter-cluster connections (if different)**: in cases where there is limited overlap between the files you are mapping and the reference, you can still use the extra markers you have measured in your sample to calculate the connections between the clusters. In such cases, you can specify here which markers you want to be included in this calculation.
- **Weight factor for inter-cluster connections**: this is the weight to be applied to inter-cluster connections, generally it is useful for inter-cluster connections to be weighted lower than connections between clusters and landmarks. The default is 0.7 and it is normally not needed to vary this parameter.

When you hit *Start analysis*, a new *.scaffold* file will be created in the working directory, containing the result of mapping the new data onto the existing reference dataset.


