#*********************************************
#*********************************************
#' Internal: Generates colors and beaks for color 3d plots.
#'
#' @return 
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD NAs
#'
#' @export
#' @rdname cplot3d.col_breaks
#' 
cplot3d.col_breaks <- function(col, colpar, breaks, white, clamp=c(0,1), log, rangevbsc, endcol){
	# Treatment of breaks:
	if(is.list(breaks)){
		breaks = breaks$br
		}
	if(length(breaks)==1){
		# 'clamp' may be given in decibel:
		if(any(clamp<0,clamp>1)){
			if(log){
				breaks = seq(clamp[1],clamp[2],length.out=breaks+1)
				}
			else{
				breaks = seq(10^(clamp[1]/10),10^(clamp[2]/10),length.out=breaks+1)
				}
			}
		else{
			breaks = seq(rangevbsc[1]+clamp[1]*diff(rangevbsc), rangevbsc[1]+clamp[2]*diff(rangevbsc), length.out=breaks+1)
			}
		}
	else if(!log){
		breaks = 10^(breaks/10)
		}
	# Number of breaks:
	lbreaks = length(breaks)-1
	if(max(breaks)==min(breaks)){
		breaks = max(breaks)+seq(-1,1,length.out=lbreaks)
		warning("All data equal. 'breaks' set to data+seq(-1,1,length.out=length(breaks)-1)")
		}
	
	# 'white' may be given as a negative numeric, specifying the volume backscattering strength dB level below which data are ploted as white, or as an integer (x=round(x)) specifying the number of breaks to plot as white:
	if(white<0){
		white = sum(breaks<=white)-1
		}
	if(white<0 || white>lbreaks){
		warning("'white' must a be numeric lower than the number of breaks (set to 0)")
		white = 0
		}

	# Generate the color vector if not given as a vector of more than one element:
	if(length(col)==1){
		# If 'white' is postitve, a positive number of breaks are reserved for no plotting, thus reducing the length of the color vector to lbreaks-white, and inserting "#FFFFFF" (white) for these breaks:
		if(length(colpar)>0){
			col = do.call(colscale, c(colpar,list(x=lbreaks-white, fun=col)))
			}
		else{
			col = colscale(lbreaks-white, col)
			}
		col = c(NAs(white), col)
		}
	
	# Add endcol at the bottom and top of the color scale:
	breaks = get.endcol(endcol, col, rangevbsc, breaks, white)
	col = breaks$col
	endcol = breaks$endcol
	breaks = breaks$breaks
	#if(length(endcol)>0){
	#	if(length(endcol)==1){
	#		endcol = c(NA, endcol)
	#		}
	#	if(is.na(endcol[1]) || nchar(endcol[1])==0){
	#		endcol[1] = head(col[!is.na(col)], 1)
	#		}
	#	if(is.na(endcol[2]) || nchar(endcol[2])==0){
	#		endcol[2] = tail(col[!is.na(col)], 1)
	#		}
	#	if(white==0 && min(rangevbsc, na.rm=TRUE)<min(breaks, na.rm=TRUE)){
	#		breaks = c(min(breaks, rangevbsc, na.rm=TRUE), breaks)
	#		col = c(endcol[1], col)
	#		}
	#	#if(max(rangevbsc, na.rm=TRUE)>max(breaks, na.rm=TRUE)){
	#	#	breaks = c(breaks, max(breaks, rangevbsc, na.rm=TRUE))
	#	#	col = c(col, endcol[2])
	#	#	}
	#	}
	
	list(col=col, endcol=endcol, breaks=breaks, white=white)
	}
