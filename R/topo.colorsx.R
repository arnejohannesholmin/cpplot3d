#*********************************************
#*********************************************
#' Returns a color vector of topographic colors.
#'
#' @param x  is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
#' @param v  is the value (lightness) of the colors.
#' @param start  and 'end' are the start and end colors, given as values in [0,1].
#' @param breakpoint  is the vector of two breakpoints between 0 and 1 where the color vector turns from blue to green and from green to yellow.
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
#' plot(x,pch=".",cex=30,col=topo.colorsx(x))
#' plot(x,pch=".",cex=30,col=topo.colorsx(x,flip=TRUE))
#' }
#'
#' @importFrom TSD dim_all NAs setrange
#' @importFrom grDevices hsv
#'
#' @export
#' @rdname topo.colorsx
#'
topo.colorsx<-function(x, v=1, start=0, end=1, clamp=NULL, breakpoint=c(1/3,2/3), flip=FALSE, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-12 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a color vector of topographic colors.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a numeric object to which rainbow colors combined with lightening and darkening should be matched.
	# ---v--- is the value (lightness) of the colors.
	# ---start--- and 'end' are the start and end colors, given as values in [0,1].
	# ---breakpoint--- is the vector of two breakpoints between 0 and 1 where the color vector turns from blue to green and from green to yellow.
	# ---flip--- is true if the color scale should be reversed.
	# ---alpha--- is the transparency.
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(breakpoint)==0){
		breakpoint=c(1/3,2/3)
		}
	
	fit2breakpoints<-function(z,breakpoint,zlow,zmid,zhigh,y){
		out = NAs(dim_all(z))
		out[zlow] = y[1] + 1/breakpoint[1] * z[zlow] * (y[2]-y[1])
		out[zmid] = y[3] + (1/(breakpoint[2]-breakpoint[1])) * (z[zmid]-breakpoint[1]) * (y[4]-y[3])
		out[zhigh] = y[5] + (1/(1-breakpoint[2])) * (z[zhigh]-breakpoint[2]) * (y[6]-y[5])
		out
		}
		
	# Identify NAs, and discard these from the calculation of colors:
	notna = !is.na(x)
	x = setrange(x,if(flip) c(end,start) else c(start,end), clamp=clamp)
	xlow = which(x[notna] < breakpoint[1])
	xmid = which(x[notna] >= breakpoint[1] & x[notna] < breakpoint[2])
	xhigh = which(x[notna] >= breakpoint[2])
	
	
	##### Execution #####
	# The hue and saturation are adopted from topo.colors():
	h = c(43,31,23,10.5,10.5,6)/60
	h = fit2breakpoints(x[notna],breakpoint,xlow,xmid,xhigh,h)
	s = c(1,1,1,1,1,0.3)
	s = fit2breakpoints(x[notna],breakpoint,xlow,xmid,xhigh,s)
	
	
	##### Output #####
	out = NAs(dim_all(x))
	out[notna] = hsv(h=h, s=s, v=v, alpha=alpha)
	out
	##################################################
	##################################################
	}
