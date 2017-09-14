#*********************************************
#*********************************************
#' Returns a color vector of rainbow colors. This is a special case of the combinedx() function.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param s  is the range of the saturation values applied to the colors, where lower values suppress saturation. The default induces a clean rainbow.
#' @param v  is the range of the value applied to the colors, where lower values causes darker colors. The default induces a clean rainbow.
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
#' plot(x, pch=".", cex=30, col=rainbowx(x))
#' plot(x, pch=".", cex=30, col=rainbowx(x, flip=TRUE))
#' }
#'
#' @export
#' @rdname rainbowx
#'
rainbowx<-function(x, s=1, v=1, start=0, end=1, clamp=NULL, flip=FALSE, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector of rainbow colors. This is a special case of the combinedx() function.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---s--- is the range of the saturation values applied to the colors, where lower values suppress saturation. The default induces a clean rainbow.
	# ---v--- is the range of the value applied to the colors, where lower values causes darker colors. The default induces a clean rainbow.
	# ---start--- and 'end' are the start and end colors, given as values in [0,1].
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	combinedx(x, s=s, v=v, start=start, end=end, clamp=clamp, flip=flip, alpha=alpha)
	##################################################
	##################################################
	}
