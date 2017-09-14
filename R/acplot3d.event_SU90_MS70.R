#*********************************************
#*********************************************
#' Generate a sequence of 3d color plots of fishery sonar with MS70 added as points. (See acplot3d.event() for parameter documentation)
#'
#' @return 
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR read.event
#'
#' @export
#' @rdname acplot3d.event_SU90_MS70
#' 
acplot3d.event_SU90_MS70 <- function(events, t=NULL, range=list(pszx=-80:-10), size="SU90", breaks=seq(-86, -40, 2), white=5, acca=5e-4, info="every1_cs_v", bgns=FALSE, pdns=FALSE, hins=FALSE, hins_add=50, cores=1){	
	# Get the maximum range of both sonars:
	beams1 = read.event(event=events[1], var="beams")
	beams2 = read.event(event=events[2], var="beams")
	maxRange = max(soundbeam_range(beams1, pos="max"), soundbeam_range(beams2, pos="max"))
	Xutim1 = read.event(event=events[1], var="utim", t="all")$utim
	Xutim2 = read.event(event=events[2], var="utim", t="all")$utim
	origin = read.event(events[2], var="vessel", t=1)
	if(length(t)==0){
		t = seq(1, suppressWarnings(read.event(event=events[1], var="numt")$numt))
		}
	
	
	# Create a function that adds the second instrument as pplot3d to the first instrument as cplot3d:
	pplot3d.eventi<-function(i, Xevent2, Xutim1, Xutim2, Xorigin, Xrange, XmaxRange, Xacca, ...){
		# Get the closest time point of the second instrument:
		i = which.min(abs(Xutim2-Xutim1[i]))
		pplot3d.event(
			event=Xevent2, 
			bmmd=c(0,3), 
			t=i, 
			acca=Xacca, 
			bgns=bgns, 
			pdns=pdns, 
			hins=hins, 
			hins_add=hins_add, 
			zoom=0.55, 
			range=Xrange, 
			xlim=XmaxRange, 
			ylim=XmaxRange, 
			zlim=Xrange$pszx, 
			clock="", 
			cs.pos="g", 
			cs.xyzlim="v",
			fun="floor",
			add=TRUE,
			origin=Xorigin,
			dens=c(100,50,100,25),
			sonar.grid.col=c("cornflowerblue","cornflowerblue"),
			sides=c("t","t"))
		}
	
	# Plot both instruments:
	system.time(tvbs <- acplot3d.event(
		event=events[1], 
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
		origin=origin, 
		addfun=pplot3d.eventi,
		Xevent2=events[2], 
		Xutim1=Xutim1,  
		Xutim2=Xutim2, 
		Xorigin=origin,
		Xrange = range, 
		XmaxRange=maxRange,
		Xacca=acca))
	} 
