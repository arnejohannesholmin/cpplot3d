#*********************************************
#*********************************************
#' Echogram using color or points (not implemented).
#'
#' @param cpplot2d_type  is "c" for cplot2d.event() and "p" for pplot2d.event(). See \code{\link{cplot2d.event}} for the other variables.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom echoIBM sonR_implemented_ind
#' @importFrom gdata object.size
#' @importFrom sonR compr.TSD read.event
#' @importFrom TSD labl.TSD strff
#'
#' @export
#' @rdname cpplot2d.event
#'
cpplot2d.event<-function(
	# Specifying whether to use cplot2d or pplot2d:
	cpplot2d_type, 
	# Used in read.event() and elsewhere in cpplot2d.event():
	event=1, t=1, cruise=2009116, dir.data=NULL, max.memory=8e9, 
	# Used in cplot2d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), null.value=NA, beamstypes=1, grid=TRUE, 
	# Used in cplot2d.plot.color.bar():
	white=0, log=TRUE, endcol=c("white", ""), 
	# Used in pplot2d.TSD() and cplot2d.TSD():
	esnm="MS70", var=c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns"), ind=list(), range=list(), subset=NULL, plot=TRUE, cs.xyzlim="g", xlim=NULL, ylim=NULL, zlim=NULL, tlim=NULL, up=FALSE, freq=1, wb=1, rmar=5, xaxis=c("time", "dist", "pings"), gap=median, gapthr=10, tol=0.1, heave=c("interp", "pixel", "ignore"), x0=NULL, unit=NULL, date=c("unique", "all", "none"), nticksx=10, 
	# Used for plotting with plot2d():
	adds=NULL, origin=1, 
	# Used when plotting date and time:
	clock=NULL, cex.clock=1, format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, col.clock=4, 
	# Used in compr.TSD():
	tres=NULL, xres=NULL, zres=NULL, rres=NULL, bres=NULL, funvbsc=c("median","mean"), funt=c("median","mean"), cs="g", 
	# Passed on to read.event, pplot2d.TSD, and cplot2d.TSD(), and lower level plotting functions:
	...){
	
	############ AUTHOR(cplot2d_last_plotted): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-12-09 - Clean version.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	ind = sonR_implemented_ind(ind)
	# Allow for ftim input in 't':
	if(!is.numeric(t)){
		t = read.event(event, t=t, var="indt")$indt
		}
	t = locateNonVerticalBeamMode(t, event)
	# Define the vessel variables and the beams variables to read:
	vesselvar = labl.TSD("v")
	beamsvar = labl.TSD("rb")
	
	
	########## Execution ##########
	# Define the variables to read from the event, depending on the value of 'schoolsample' and 'school'. If 'schoolsample' is a string, this indicates that the school object should be plotted, if available. Else if 'school' is TRUE, the fish positions are attempted read from the event. See also the code below reading the variables 'thisvar' with read.event():
	### thisvar = c("vbsc", "utim", "indt", "voxels", vesselvar, beamsvar)
	thisvar = c("vbsc", "utim", "indt", "voxels", "vessel", "beams")
	
	### Read and plot the pings in turns: ###
	# Read the data:
	read.event_parlist_fixed = list(drop.out=TRUE, strip.out=TRUE, dir.out=FALSE, merge=TRUE, ind=NULL, pad=TRUE, split=TRUE)
	read.event_parlist_without_var = list(event=event, t=t, cruise=cruise, adds=adds, esnm=esnm, origin=origin, dir.data=dir.data)
	# Add ...:
	ll = list(...)
	ll = ll[intersect(names(ll), names(formals("read.event")))]
	read.event_parlist_without_var = c(read.event_parlist_without_var, ll)
	read.event_parlist_without_var = read.event_parlist_without_var[!duplicated(names(read.event_parlist_without_var))]
	read.event_parlist = c(read.event_parlist_without_var, list(var=c("lenb")))
	
	# Check the memory usage by reading the beam lengths:
	mem = do.call("read.event", c(read.event_parlist, read.event_parlist_fixed))
	nbytesDouble = 8
	memFact = 2.6
	#mem = memFact * nbytesDouble * max(mem$lenb) * NROW(mem$lenb) * length(t)
	mem = memFact * nbytesDouble * max(mem$lenb) * length(mem$lenb)
	if(length(mem)==0 || is.na(mem)){
		warning("No data, maybe the time is outside of the limits?")
		return(FALSE)
		}
	if(mem>max.memory){
		ans = readline(paste0("Plotting the echogram will occupy ", round(mem*1e-9, digits=2), " GB. Allow? (y/n)"))
		if(tolower(ans)!="y"){
			warnings("Function terminated. Reduce the number of time steps or increase max.memory")
			return(FALSE)
			}
		}
	
	read.event_parlist = c(read.event_parlist_without_var, list(var=thisvar))
	if(exists("cplot2d_last_plotted") && exists("cplot2d_last_plotted_parlist") && identical(read.event_parlist, cplot2d_last_plotted_parlist)){
		cat("Using last read data...\n")
		}
	else{
		cat("Reading data...\n")
		cplot2d_last_plotted <- do.call("read.event", c(read.event_parlist, read.event_parlist_fixed))
		# Compress the data if a very large dataset is to be plotted. Preferably create a compressed event by compr.event(), which places the compressed event in the same directory as the original event (named e.g. tsd_compr1), and then plot this event:
		suppressWarnings(cplot2d_last_plotted <- compr.TSD(cplot2d_last_plotted, tres=tres, xres=xres, zres=zres, rres=rres, bres=bres, funvbsc=funvbsc, funt=funt, adds=adds, split=TRUE, origin=origin, ...))
		os = memFact * object.size(cplot2d_last_plotted)
		if(os>1e9){
			cat("Memory used:", round(os * 1e-9), "GB\n")
			}
		else{
			cat("Memory used:", round(os * 1e-6), "MB\n")
			}
		assign("cplot2d_last_plotted", cplot2d_last_plotted, envir=.GlobalEnv)
		assign("cplot2d_last_plotted_parlist", read.event_parlist, envir=.GlobalEnv)
		}
	
	# Plot the points:
	ll=list(...)
	
	maxnfreq = 20
	if(is.character(freq) && identical(tolower(freq), "all")){
		freq = seq_len(nrow(cplot2d_last_plotted$freq))
		}
	else if(length(freq)>=maxnfreq){
		freq = freq[1]
		}
	for(f in freq){
		if(strff("c",cpplot2d_type)){
			# Define the list of variables used as input to pplot2d.TSD(), given in the order used in that function:
			thisl=list(
			# Main variable:
			data=cplot2d_last_plotted, t=t, 
			# Used in cplot2d.TSD():
			breaks=breaks, col=col, colpar=colpar, null.value=null.value, beamstypes=beamstypes, grid=grid, adds=adds, xlim=xlim, ylim=ylim, zlim=zlim,  tlim=tlim, up=up, freq=f, wb=wb, rmar=rmar, xaxis=xaxis, gap=gap, gapthr=gapthr, tol=tol, heave=heave, x0=x0, unit=unit, date=date, nticksx=nticksx, 
			# Used in cplot2d.plot.color.bar():
			white=white, log=log, endcol=endcol, 
			# Used in pplot2d.TSD() and cplot2d.TSD():
			esnm=esnm, var=var, ind=ind, range=range, subset=subset, plot=plot, 
			# Used when plotting date and time:
			clock=clock, cex.clock=cex.clock, format.clock=format.clock, digits.clock=digits.clock, col.clock=col.clock)
		
			# Define the variables present in '...' but not in the list 'thisl':
			otherl = ll[setdiff(names(ll),names(thisl))]
			thisout <- do.call("cplot2d.TSD",c(thisl,otherl))
			}
		else if(strff("p",cpplot2d_type)){
			}
		else{
			stop("Invalid values of 'cpplot2d_type'. Should be one of \"c\" and \"p\"")
			}
		}
		
		
	########## Output ##########
	gc()
	invisible()
	##################################################
	##################################################
	}
