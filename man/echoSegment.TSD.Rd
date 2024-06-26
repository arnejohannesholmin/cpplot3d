% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echoSegment.TSD.R
\name{echoSegment.TSD}
\alias{echoSegment.TSD}
\title{Extracts a subvolume of the data enclosed in the bounding box or ellipsoid specified in the input, and calculates the total echo energy in the subvolume. Optionally the bounding object is plotted on top of an existing plot.}
\usage{
echoSegment.TSD(
  data = NULL,
  object = c("ellipsoid", "cuboid"),
  par = c(1, 1, 1),
  center = c(0, 0, 0),
  angle = 0,
  plot = TRUE,
  seg.col = "green",
  alpha = 0.2,
  subdivide = 3,
  excl.neg = TRUE,
  full.out = FALSE,
  ...
)
}
\arguments{
\item{data}{is a list containing the acoustic data and the vessel data, as returned from read.event(var=c("vbsc","voxels","vessel")).}

\item{object}{is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).}

\item{par}{is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.}

\item{center}{is a vector of three elements representing the centre position of the object.}

\item{angle}{is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".}

\item{plot}{is TRUE if the object should be plotted as an rgl-object.}

\item{seg.col}{is the color of the object in case plot==TRUE.}

\item{alpha}{is the transparacy of the object in case plot==TRUE.}

\item{subdivide}{is the density of points on the plotted ellipsoid.}

\item{excl.neg}{is FALSE if negative mass values are to be included in the calculation of the centers of mass of the school.}

\item{full.out}{is TRUE to return voxel positions, volumes and acoustic data inside the segmente, as well as the segmentation mask.}
}
\description{
Extracts a subvolume of the data enclosed in the bounding box or ellipsoid specified in the input, and calculates the total echo energy in the subvolume. Optionally the bounding object is plotted on top of an existing plot.
}
\examples{
\dontrun{}

}
