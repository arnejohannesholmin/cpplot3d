#*********************************************
#*********************************************
#' Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param s  is the range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.
#' @param v  is the range of the value applied to the colors, where lower values causes darker colors. The default applies less daker colors at the lover values of x.
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
#' plot(x, pch=".", cex=30, col=combinedx(x))
#' plot(x, pch=".", cex=30, col=combinedx(x,flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange
#' @importFrom grDevices hsv
#'
#' @export
#' @rdname combinedx
#'
combinedx<-function(x, s=c(1,0.3), v=c(0.7,1), start=0, end=1, clamp=NULL, flip=FALSE, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector of rainbow colors which are desaturated by 's' and darkened by 'v' to obtain the "combined" color scale.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---s--- is the range of the saturation values applied to the colors, where lower values suppress saturation. The default applies less strong colors at the higher values of x.
	# ---v--- is the range of the value applied to the colors, where lower values causes darker colors. The default applies less daker colors at the lover values of x.
	# ---start--- and 'end' are the start and end colors, given as values in [0,1].
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	##### Preparation and execution #####
	# Identify NAs, and discard these from the calculation of colors:
	notna = !is.na(x)
	# The hue should move from 'start' to 'end':
	h=setrange(x[notna],if(flip) c(end,start) else c(start,end), clamp=clamp)
	s=setrange(x[notna],if(flip) rev(s) else s)
	v=setrange(x[notna],if(flip) rev(v) else v)
	
	
	##### Output #####
	out = NAs(dim_all(x))
	out[notna] = hsv(h=h, s=s, v=v, alpha=alpha)
	out
	##################################################
	##################################################
	}
