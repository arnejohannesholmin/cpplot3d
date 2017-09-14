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
