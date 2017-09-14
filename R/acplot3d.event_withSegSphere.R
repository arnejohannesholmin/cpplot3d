#*********************************************
#*********************************************
#' Generate a sequence of 3d color plots of fishery sonar with a segmentation sphere (as used by SX90.segment.event()) added. (See acplot3d.event() and SX90.segment.event() for parameter documentation)
#'
#' @return 
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl ellipse3d plot3d
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR read.event
#'
#' @export
#' @rdname acplot3d.event_withSegSphere
#' 
acplot3d.event_withSegSphere <- function(event, t, range=list(pszx=-80:-10), size="SU90", breaks=seq(-86, -40, 2), white=5, acca=c(100,200), sphere=TRUE, info=paste("seg", if(isTRUE(sphere)) "sphere", sep="_"), ell=100, cores=1, segpar=1:2, fun="mod", maxRange=NULL, nticks=c(5,5,2), ...){	
	# Get the maximum range of both sonars:
	if(length(maxRange)==0){
		maxRange = read.event(event=event, var="beams", t=t)
		maxRange = soundbeam_range(maxRange, pos="max")
		}
	
	# Create a function for plotting segmentation data:
	pplot3d.event_seg <- function(i, Xevent, Xrange, Xxlim, Xylim, Xsegpar1, Xsegpar2, Xacca, Xell, ...){
		# Get the closest time point of the MS70 data:
		pplot3d.event(
			event=Xevent, 
			t=i, 
			var="sgsc", 
			acca=Xacca[1], 
			zoom=0.55, 
			range=Xrange, 
			xlim=Xxlim, 
			ylim=Xylim, 
			zlim=Xrange$pszx, 
			clock="", 
			cs.pos="g", 
			cs.xyzlim="v",
			fun=fun,
			add=TRUE,
			segpar=Xsegpar1,
			axes=FALSE
			)
		pplot3d.event(
			event=Xevent, 
			t=i, 
			var="sgsc", 
			acca=Xacca[2], 
			zoom=0.55, 
			range=Xrange, 
			xlim=Xxlim, 
			ylim=Xylim, 
			zlim=Xrange$pszx, 
			clock="", 
			cs.pos="g", 
			cs.xyzlim="v",
			fun=fun,
			add=TRUE,
			segpar=Xsegpar2,
			cols=2, 
			axes=FALSE
			)
		ss = read.event(event=Xevent, t=i, var=c("Xcex","Xcey","Xcez"), segpar=Xsegpar1)
		if(sphere && !is.na(ss$Xcex)){
			ee = rgl::ellipse3d(diag(3)*Xell^2, centre=c(ss$Xcex, ss$Xcey, ss$Xcez), t=1)
			rgl::plot3d(ee, col="green", alpha=0.1, add=TRUE)
			}
		}

	# Plot the data and segmentations:
	system.time(tvbs <- acplot3d.event(
		event=event, 
		bmmd=c(0,3), 
		t=t,  
		breaks=breaks,
		white=white,
		size=size,
		view=c(-90,0), 
		zoom=0.55, 
		color.bar="y+-",
		range=range, 
		xlim=maxRange, 
		ylim=maxRange, 
		zlim=range$pszx, 
		clock="tbl", 
		lsp.clock=0.02, 
		info=info, 
		cs.pos="g", 
		cs.xyzlim="v",
		vessel.pos=TRUE, 
		nticks=nticks, 
		cores=cores,
		addfun=pplot3d.event_seg,
		Xevent=event, 
		Xrange=range, 
		Xxlim=maxRange,
		Xylim=maxRange,
		Xsegpar1=segpar[[1]], 
		Xsegpar2=segpar[[2]],
		Xacca=acca,
		Xell=ell, ...))
	}
