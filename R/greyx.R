#*********************************************
#*********************************************
#' Returns a color vector of heat colors.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param h  is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.
#' @param v  is the value (lightness) of the colors.
#' @param start  and 'end' are the start and end colors, given as values in [0,1].
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
#' plot(x, pch=".", cex=30, col=greyx(x))
#' plot(x, pch=".", cex=30, col=greyx(x,flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange
#' @importFrom grDevices hsv
#'
#' @export
#' @rdname greyx
#'
greyx<-function(x, s=0, start=0, end=1, clamp=NULL, flip=FALSE, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector of heat colors.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---h--- is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.
	# ---v--- is the value (lightness) of the colors.
	# ---start--- and 'end' are the start and end colors, given as values in [0,1].
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Identify NAs, and discard these from the calculation of colors:
	notna = !is.na(x)
	x = setrange(x,if(flip) c(end,start) else c(start,end), clamp=clamp)
	
	
	##### Execution #####
	# The hue and saturation are adopted from heat.colors():
	h = x[notna]
	v = x[notna]
	
	
	##### Output #####
	out = NAs(dim_all(x))
	out[notna] = hsv(h=h, s=s, v=v, alpha=alpha)
	out
	##################################################
	##################################################
	}
