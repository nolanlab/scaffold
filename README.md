SCAFFoLD
========

## Installation

You need to have a working C++ compiler to install SCAFFoLD. Please refer to the following steps for installing a compiler on your system

### Mac OSX

You need to install the XCode software from Apple that is freely available on the App Store. Depending on the specific version of XCode you are using you might also need to install the "Command Line Tools" package separately. Please refer to the Documentation for your XCode version

### Windows

Install Visual Studio. The Express version is freely available from Microsoft

### Linux

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


