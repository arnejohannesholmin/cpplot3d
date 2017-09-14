#*********************************************
#*********************************************
#' Returns a color vector from the function 'fun' matching the input numeric object 'x', or a sequence of length 'x' from 'start' to 'end' if 'x' is a single integer value (whole number).
#'
#' @param x  is either a numeric object to which colors should be matched, or the length of the color vector.
#' @param fun  is a character giving the name of the color function or the color function itself.
#' @param start  and 'end' are the start and end colors, given either as values in [0,1], or as color character strings in which case the output colors are interpolated between the given colors as specified by 'interpolate'.
#' @param h  is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.
#' @param s  is the saturation parameter, only used in rainbowx(), where lower values suppress saturation.
#' @param v  is the value parameter, only used in rainbowx(), heat.colorsx(), topo.colorsx(), and cm.colorsx(), where lower values causes darker colors.
#' @param sc  is saturation parameter used in combinedx().
#' @param vc  is value parameter used in combinedx().
#' @param flip  is TRUE if the color vector is to be reversed.
#' @param bias  is a parameter used in colorRampPalette().
#' @param space  is a parameter used in colorRampPalette().
#' @param interpolate  is a parameter used in colorRampPalette().
#' @param breakpoint  is the breakpoint of piecewise colors, defaulted in the different function called by colscale().
#' @param alpha  is a parameter used in rainbow(), heat.colors(), terrain.colors(), topo.colors() and cm.colors().
#' @param ...  methods passed on to sub-functions (not used, but allowing for unused arguments).
#'
#' @return
#'
#' @examples
#' \dontrun{
#' x=runif(10000)
#' pp(2,7,oma=zeros(4),mar=zeros(4))
#' for(i in 1:7){
#' 	plot(x,pch=".",cex=10,col=colscale(x,fun=i))
#' 	plot(x,pch=".",cex=10,col=colscale(x,fun=i,flip=TRUE))
#' }
#'
#' @importFrom TSD setrange
#' @importFrom grDevices palette colors
#'
#' @export
#' @rdname colscale
#'
colscale<-function(x, fun=c("rainbow", "grey", "fade.colorsx", "heat.colors", "terrain.colors", "topo.colors", "cm.colors", "combined"), start=0, end=0.8, h=1/6, s=1, v=1, sc=c(1,0.3), vc=c(0.7,1), flip=FALSE, bias=1, space=c("rgb","Lab"), interpolate=c("linear","spline","fadewhite","fadeblack"), breakpoint=NULL, alpha=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-01-29 - Clean version.
	# Update: 2013-02-28 - Expanded 'dark' and 'light' to account for both the start and end of the vector.
	# Last: 2013-09-12 - Major changes. The function changed to support numeric input 'x' using the functions combinesx(), heat.colorsx() and so on.
	########### DESCRIPTION: ###########
	# Returns a color vector from the function 'fun' matching the input numeric object 'x', or a sequence of length 'x' from 'start' to 'end' if 'x' is a single integer value (whole number).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is either a numeric object to which colors should be matched, or the length of the color vector.
	# ---fun--- is a character giving the name of the color function or the color function itself.
	# ---start--- and 'end' are the start and end colors, given either as values in [0,1], or as color character strings in which case the output colors are interpolated between the given colors as specified by 'interpolate'.
	# ---h--- is the value hue at the end point of the heat color vector, defaulted to 1/6 to move from red to yellow.
	# ---s--- is the saturation parameter, only used in rainbowx(), where lower values suppress saturation.
	# ---v--- is the value parameter, only used in rainbowx(), heat.colorsx(), topo.colorsx(), and cm.colorsx(), where lower values causes darker colors.
	# ---sc--- is saturation parameter used in combinedx().
	# ---vc--- is value parameter used in combinedx().
	# ---flip--- is TRUE if the color vector is to be reversed.
	# ---bias--- is a parameter used in colorRampPalette().
	# ---space--- is a parameter used in colorRampPalette().
	# ---interpolate--- is a parameter used in colorRampPalette().
	# ---breakpoint--- is the breakpoint of piecewise colors, defaulted in the different function called by colscale().
	# ---alpha--- is a parameter used in rainbow(), heat.colors(), terrain.colors(), topo.colors() and cm.colors().
	# ---...--- methods passed on to sub-functions (not used, but allowing for unused arguments).
	
	
	##################################################
	##################################################
	##### Preparation #####
	# If 'x' is given as a single integer (whole number) it is interpreted as the number of steps from 'start' to end':
	if(length(x)==1 && x%%1==0){
		if(is.character(start) || is.character(end)){
			x <- seq.int(0, 1, length.out=x)
		}
		else{
			x <- seq.int(start, end, length.out=x)
		}
	}
		
	# Legal types:
	types <- c("rainbowx", "greyx", "fade.colorsx", "heat.colorsx", "terrain.colorsx", "topo.colorsx", "cm.colorsx", "combinedx")
	if(is.numeric(fun)){
		fun <- palette()[1 + (fun-1)%%length(palette())]
	}
	else{
		# Transform to character name of function:
		 if(!is.character(fun)){
			fun <- as.character(quote(fun))
		}
		# Locate the input function in 'types':
		funnr <- grep(tolower(fun[1]),types)
		if(length(funnr)==0){
			if(all(fun[1] %in% colors(),fun[2] %in% colors())){
				start <- fun[1]
				end <- fun[2]
			}
			else if(fun[1] %in% colors()){
				if(start %in% colors()){
					end <- start
					start <- fun[1]
				}
				else{
					start <- fun[1]
					end <- fun[1]
				}
			}
			else{
				warning("Invalid color or color function specified by 'fun'. Black returned for all values of 'x'")
				start <- "black"
				end <- "black"
			}
		}
	}
	
	# If start and end are characters they are interpreted as start and end colors between which colors are interpolated:
	if(is.character(start) && is.character(end)){
		return(fade.colorsx(x, start=start, end=end, breakpoint=breakpoint, bias=bias, space=space, interpolate=interpolate, flip=flip))
	}
	
	# Create the list used in do.call:
	if(funnr[1]==1){
		A <- list(x=x, s=s, v=v, start=start, end=end, flip=flip, alpha=alpha)
	}
	else if(funnr[1]==2){
		A <- list(x=setrange(x, if(flip) c(end,start) else c(start,end)), alpha=alpha)
	}
	else if(funnr[1]==3){
		A <- list(x=x, start=start, end=end, breakpoint=breakpoint, bias=bias, space=space, interpolate=interpolate, flip=flip)
	}
	else if(funnr[1]==4){
		A <- list(x=x, h=h, v=v, start=start, end=end, breakpoint=breakpoint, flip=flip, alpha=alpha)
	}
	else if(funnr[1]==5){
		A <- list(x=x, start=start, end=end, breakpoint=breakpoint, flip=flip, alpha=alpha)
	}
	else if(funnr[1]==6){
		A <- list(x=x, v=v, start=start, end=end, breakpoint=breakpoint, flip=flip, alpha=alpha)
	}
	else if(funnr[1]==7){
		A <- list(x=x, v=v, start=start, end=end, breakpoint=breakpoint, flip=flip, alpha=alpha)
	}
	else if(funnr[1]==8){
		A <- list(x=x, s=sc, v=vc, start=start, end=end, flip=flip, alpha=alpha)
	}
	
	
	##### Execution and output #####
	# Call the color scale function:
	do.call(types[funnr[1]], A)
	##################################################
	##################################################
}
