#*********************************************
#*********************************************
#' Returns a color vector moving from cyan to magenta via white.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param v  is the value (lightness) of the colors.
#' @param start  and 'end' are the start and end colors, given as values in [0,1].
#' @param breakpoint  is the breakpoint between 0 and 1 where the color vector turns at white.
#' @param flip  is true if the color scale should be reversed.
#' @param alpha  is the transparency.
#' @param ...  variables passed on from other functions.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' x=runif(1000)
#' pp(1,2)
#' plot(x, pch=".", cex=30, col=cm.colorsx(x))
#' plot(x, pch=".", cex=30, col=cm.colorsx(x,flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange
#' @importFrom grDevices hsv
#'
#' @export
#' @rdname cm.colorsx
#'
cm.colorsx<-function(x, v=1, start=0, end=1, clamp=NULL, breakpoint=0.5, flip=FALSE, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector moving from cyan to magenta via white.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---v--- is the value (lightness) of the colors.
	# ---start--- and 'end' are the start and end colors, given as values in [0,1].
	# ---breakpoint--- is the breakpoint between 0 and 1 where the color vector turns at white.
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- variables passed on from other functions.
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(breakpoint)==0){
		breakpoint=0.5
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
	# The hue is defined differently for the cyan and magenta part of the color scale:
	h = c(6,6,10,10)/12
	h = fit1breakpoint(x[notna],breakpoint,xlow,xhigh,h)
	# The saturation moves from 0.5 via 0 and back to induce fading to white:
	s = c(0.5,0,0,0.5)
	s = fit1breakpoint(x[notna],breakpoint,xlow,xhigh,s)
	
	
	##### Output #####
	out = NAs(dim_all(x))
	out[notna] = hsv(h=h, s=s, v=v, alpha=alpha)
	out
	##################################################
	##################################################
	}
