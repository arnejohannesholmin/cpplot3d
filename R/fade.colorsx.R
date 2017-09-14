#*********************************************
#*********************************************
#' Returns a color vector interpolated between the colors specified by 'start' and 'end', possitbly fading via white of black.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param start  and 'end' are the start and end colors.
#' @param breakpoint  is the breakpoint between 0 and 1 where the color vector turns at white.
#' @param flip  is true if the color scale should be reversed.
#' @param alpha  is the transparency.
#' @param ...  methods passed on to sub-functions (not used, but allowing for unused arguments).
#'
#' @return
#'
#' @examples
#' \dontrun{
#' x=runif(300)
#' pp(1,2)
#' plot(x, pch=".", cex=30, col=fade.colorsx(x, start="orange", end="green"))
#' plot(x, pch=".", cex=30, col=fade.colorsx(x, start="orange", end="green", flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange strff
#' @importFrom grDevices rgb colorRamp
#'
#' @export
#' @rdname fade.colorsx
#'
fade.colorsx<-function(x,start="red", end="violet", breakpoint=0.5, bias=1, space=c("rgb","Lab"), interpolate=c("linear","spline","fadewhite","fadeblack"), flip=FALSE, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector interpolated between the colors specified by 'start' and 'end', possitbly fading via white of black.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---start--- and 'end' are the start and end colors.
	# ---breakpoint--- is the breakpoint between 0 and 1 where the color vector turns at white.
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(breakpoint)==0){
		breakpoint=0.5
		}
	
	# Identify NAs, and discard these from the calculation of colors:
	notna = !is.na(x)
	x = setrange(x)
	if(strff("fade",interpolate[1])){
		xlow = which(x[notna] < breakpoint)
		xhigh = which(x[notna] >= breakpoint)
		}
	if(flip){
		temp=start
		start=end
		end=temp
		}
	
	
	##### Execution #####
	out=NAs(dim_all(x))
	if(strff("fadew",interpolate[1])){
		out[xlow]=rgb(colorRamp(c(start,"white"),bias=bias,space=space)(setrange(x[xlow]))/255)
		out[xhigh]=rgb(colorRamp(c("white",end),bias=bias,space=space)(setrange(x[xhigh]))/255)
		}
	else if(strff("fadeb",interpolate[1])){
		out[xlow]=rgb(colorRamp(c(start,"black"),bias=bias,space=space)(setrange(x[xlow]))/255)
		out[xhigh]=rgb(colorRamp(c("black",end),bias=bias,space=space)(setrange(x[xhigh]))/255)
		}
	else{
		out[notna]=rgb(colorRamp(c(start,end),bias=bias,space=space)(x[notna])/255)
		}
	
	
	##### Output #####
	out
	##################################################
	##################################################
	}
