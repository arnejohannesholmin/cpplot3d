#*********************************************
#*********************************************
#' Adds color at the ends of the data.
#'
#' @param endcol  The color to use for values above the highest break value.
#' @param col  The present colors.
#' @param rangevbsc	Range of the vbsc (sv) values.
#' @param breaks,white	See \code{\link{cplot3d.event}}.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail head
#'
#' @export
#'
get.endcol <- function(endcol, col, rangevbsc, breaks, white){
	# Add endcol at the bottom and top of the color scale:
	if(length(endcol)>0){
		if(length(endcol)==1){
			endcol = c(NA, endcol)
		}
		if(nchar(endcol[1])==0){
			endcol[1] = head(col[!is.na(col)], 1)
		}
		if(nchar(endcol[2])==0){
			endcol[2] = tail(col[!is.na(col)], 1)
		}
		if(white==0 && min(rangevbsc, na.rm=TRUE)<min(breaks, na.rm=TRUE)){
			breaks = c(min(breaks, rangevbsc, na.rm=TRUE), breaks)
			col = c(endcol[1], col)
		}
		if(max(rangevbsc, na.rm=TRUE)>max(breaks, na.rm=TRUE)){
			breaks = c(breaks, max(breaks, rangevbsc, na.rm=TRUE))
			col = c(col, endcol[2])
		}
	}
	list(endcol=endcol, col=col, breaks=breaks)
}
