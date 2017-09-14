#*********************************************
#*********************************************
#' Generate a series of 3-D echograms with the function cplot3d.event(), which are save as png files and can be animated.
#'
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param beams optional list of beam configuration data in the tSD format.
#' @param type the type of conversion, either "Sv" or "TS".
#' @param linear logical: if TRUE (default) return the linear values (volume backscattering coefficient), if FALSE return dB values (mean volume backscattering).
#' @param use string vector naming the conversions to include. "conv" applies the power to Sv or TS conversion and "tvg" applies the time varied gain (range compensation). 
#' @param inv logica: if TRUE do the inverse conversion from Sv or TS to power.
#' @param cali a list of calibration data as returned from readcalfile(). Must contain either 'gain' or 'sacr' or both.
#' @param Rlist StoX R list.
#'
#' @return A list of relevant data from the plot.
#'
#' @examples
#' \dontrun{
#' acplot3d.event_default(t=1:10) # Unfinished example}
#'
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR read.event
#'
#' @export
#' @rdname acplot3d.event_default
#' 
acplot3d.event_default <- function(event, t=NULL, range=list(pszx=-80:-10), size="SU90", breaks=seq(-86, -40, 2), white=5, acca=5e-4, info="every1_cs_v", bgns=FALSE, pdns=FALSE, hins=FALSE, hins_add=50, cores=1){	
	# Get the maximum range of both sonars:
	beams = read.event(event=event[1], var="beams")
	maxRange = soundbeam_range(beams, pos="max")
	origin = read.event(event[1], var="vessel", t=1)
	if(length(t)==0){
		t = seq(1, suppressWarnings(read.event(event=event[1], var="numt")$numt))
		}
	
	# Plot both instruments:
	acplot3d.event(
		event=event[1], 
		bmmd=c(0,3), 
		t=t, 
		breaks=breaks,
		white=white,
		size=size,
		view=c(-90,0), 
		zoom=0.55, 
		range=range, 
		xlim=maxRange, 
		ylim=maxRange, 
		zlim=range$pszx, 
		clock="tbl", 
		lsp.clock=0.02, 
		info="every1_cs_v_wMS70", 
		cs.pos="g", 
		cs.xyzlim="v",
		vessel.pos=TRUE, 
		fun="floor",
		nticks=c(5,5,3), 
		cores=cores, 
		origin=origin)
	} 
