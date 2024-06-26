% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greyx.R
\name{greyx}
\alias{greyx}
\title{Returns a color vector of heat colors.}
\usage{
greyx(x, s = 0, start = 0, end = 1, clamp = NULL, flip = FALSE, alpha = 1, ...)
}
\arguments{
\item{x}{is a numeric object to which rainbow colors combined with lightening and darkening should be matched.}

\item{start}{and 'end' are the start and end colors, given as values in [0,1].}

\item{flip}{is true if the color scale should be reversed.}

\item{alpha}{is the transparency.}

\item{...}{methods passed on to sub-functions (not used, but allowing for unused arguments).}

\item{h}{is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.}

\item{v}{is the value (lightness) of the colors.}
}
\description{
Returns a color vector of heat colors.
}
\examples{
\dontrun{
x=runif(1000)
pp(1,2)
plot(x, pch=".", cex=30, col=greyx(x))
plot(x, pch=".", cex=30, col=greyx(x,flip=TRUE))
}

}
