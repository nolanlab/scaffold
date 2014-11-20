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




