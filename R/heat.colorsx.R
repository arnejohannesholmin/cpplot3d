#*********************************************
#*********************************************
#' Returns a color vector of heat colors.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param h  is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.
#' @param v  is the value (lightness) of the colors.
#' @param start  and 'end' are the start and end colors, given as values in [0,1].
#' @param breakpoint  is the breakpoint between 0 and 1 where the color vector turns at white.
#' @param flip  is true if the color scale should be reversed.
#' @param alpha  is the transparency.
#' @param ...  methods passed on to sub-functions (not used, but allowing for unused arguments).
#'
#' @return
#'
#' @examples
#' \dontrun{
#' x=runif(1000)
#' pp(1,2)
#' plot(x, pch=".", cex=30, col=heat.colorsx(x))
#' plot(x, pch=".", cex=30, col=heat.colorsx(x, flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange
#' @importFrom grDevices hsv
#'
#' @export
#' @rdname heat.colorsx
#'
heat.colorsx<-function(x, h=1/6, v=1, start=0, end=1, clamp=NULL, breakpoint=0.75, flip=FALSE, alpha=1, ...){
	
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(breakpoint)==0){
		breakpoint=0.75
		}
	
	fit1breakpoint<-function(z,breakpoint,zlow,zhigh,y){
		out = NAs(dim_all(z))
		out[zlow] = y[1] + 1/breakpoint * z[zlow] * (y[2]-y[1])
		out[zhigh] = y[3] + (1/(1-breakpoint)) * (z[zhigh]-breakpoint) * (y[4]-y[3])
		out
		}
	# Identify NAs, and discard these from the calculation of colors:
	notna = !is.na(x)
	x = setrange(x,if(flip) c(end,start) else c(start,end), clamp=clamp)
	xlow = which(x[notna] < breakpoint)
	xhigh = which(x[notna] >= breakpoint)
	
	
	##### Execution #####
	# The hue and saturation are adopted from heat.colors():
	h = c(0,h,h,h)
	h = fit1breakpoint(x[notna],breakpoint,xlow,xhigh,h)
	s = c(1,1,1,0)
	s = fit1breakpoint(x[notna],breakpoint,xlow,xhigh,s)
	
	
	##### Output #####
	out = NAs(dim_all(x))
	out[notna] = hsv(h=h, s=s, v=v, alpha=alpha)
	out
	##################################################
	##################################################
	}
