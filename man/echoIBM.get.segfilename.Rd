% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/echoIBM.get.segfilename.R
\name{echoIBM.get.segfilename}
\alias{echoIBM.get.segfilename}
\title{Locates the path of an event specified by numeric or sting for both 'event' and 'cruise' (used in read.event()), and generates segmentation file names continuing from the extisting numbered segmentation files.}
\usage{
echoIBM.get.segfilename(
  n = 1,
  t = 0,
  nchart = NULL,
  event = 1,
  cruise = 2009116,
  esnm = "MS70",
  dir.data = NULL,
  add = "",
  startn = NULL
)
}
\arguments{
\item{n}{is an integer giving the sequence number to append to the file name. If n = 3 and there are 4 segmentation files present, the numbers 5, 6, and 7 are added to the file names.}

\item{t}{is the time step index to append to the file name.}

\item{nchart}{is the number of characters of the time point information, defaulted to the maximum number of characters of 't'.}

\item{event}{is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.}

\item{cruise}{is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.}

\item{esnm}{is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.}

\item{dir.data}{is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().}

\item{add}{is a string to add to the file name.}

\item{startn}{is the start number of the files.}
}
\description{
Locates the path of an event specified by numeric or sting for both 'event' and 'cruise' (used in read.event()), and generates segmentation file names continuing from the extisting numbered segmentation files.
}
\examples{
\dontrun{}

}
