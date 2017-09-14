cpplot3d R package
=====

This R package provides functions for creating 3D color and point plots using the rgl package, and segmentation of sonar data.

Version: 1.1
Required R version: 3.3.3

Installation
=====

``` r
# Install the packages that cpplot3d depends on. Note that this updates all the specified packages to the latest (binary) version. To skip installing already installed packages, run install.packages(setdiff(dep.pck, installed.packages()[,"Package"]), repos="http://cran.us.r-project.org") instead:
dep.pck <- c("akima", "fields", "gdata", "gsl", "pbapply", "rgl")
install.packages(dep.pck, repos="http://cran.us.r-project.org")

# Install cpplot3d and also the packages that cpplot3d depends on which are on GitHub (by Holmin):
# On Windows you will need Rtools to complete the installations. Check if you have this by running Sys.getenv('PATH'), and go to https://cran.r-project.org/bin/windows/Rtools/ to install Rtools if not.

dep.pck.git <- c("arnejohannesholmin/TSD", "arnejohannesholmin/SimradRaw", "arnejohannesholmin/sonR", "arnejohannesholmin/echoIBM", "arnejohannesholmin/cpplot3d")
devtools::install_github(dep.pck.git)

```

# For changes log see https://github.com/arnejohannesholmin/cpplot3d/NEWS

Examples
=====

``` r
### If an event of underwater acoustic data exists use this event. If not, generate an event from the example data in the sonR package:

# Set the directory of the acoustic data, here simply as the tempdir() but preferably another location:
library(sonR)
dir <- tempdir()

ev <- generate.event(event="Event1", cruise="Cruise1", esnm="EK60", dir.type = c("raw", "tsd"), dir.data=dir)
evRaw <- ev[1]
evTSD <- ev[2]
# Add one raw file to the event:
echoSounderFile <- file.path(system.file("extdata", package="sonR"), "RedSlip-D20160915-T120914.raw")
file.copy(echoSounderFile, evRaw)
# Generate the TSD files, which are faster to read with R:
EKRaw2TSD(evRaw)

# Plot an echogram of the data, which are from a netpen with herring:
system.time(cplot2d.event(evTSD, t="all", xaxis="p"))

# Plotting with time along the x axis is more time demanding, since image.plot() is used as opposed to grid.raster() when the x variable has fixed increments. 
system.time(cplot2d.event(evTSD, t="all", xaxis="t"))
# Observe that the maximum absolue fractional deviation from the  median  of the diffs of the variable on the x axis is printed. Setting tol=0.7 allows for gaps in the x variable and still plot using grid.raster():
system.time(cplot2d.event(evTSD, t="all", tol=1))
```

License
=====

The cpplot3d package is licensed under the LGPL-3.)

