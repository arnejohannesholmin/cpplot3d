% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combinedx.R
\name{combinedx}
\alias{combinedx}
\title{Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.}
\usage{
combinedx(
  x,
  s = c(1, 0.3),
  v = c(0.7, 1),
  start = 0,
  end = 1,
  clamp = NULL,
  flip = FALSE,
  alpha = 1,
  ...
)
}
\arguments{
\item{x}{is a numeric object to which rainbow colors combined with lightening and darkening should be matched.}

\item{s}{is the range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.}

\item{v}{is the range of the value applied to the colors, where lower values causes darker colors. The default applies less daker colors at the lover values of x.}

\item{start}{and 'end' are the start and end colors, given as values in [0,1].}

\item{flip}{is true if the color scale should be reversed.}

\item{alpha}{is the transparency.}

\item{...}{methods passed on to sub-functions (not used, but allowing for unused arguments).}
}
\description{
Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
}
\examples{
\dontrun{
x=runif(1000)
pp(1,2)
plot(x, pch=".", cex=30, col=combinedx(x))
plot(x, pch=".", cex=30, col=combinedx(x,flip=TRUE))
}

}
