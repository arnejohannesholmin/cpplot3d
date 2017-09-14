#*********************************************
#*********************************************
#' Plot a 2-D echogram.
#'
#' This function reads TSD data from an event and plots the data as a 2-D echogram. The funciton will look for the previously read data and use these if applicable, for faster plotting of the same data using different plotting settings.
#'
#' @param event						is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t							is either a vector of the numbers of the pings to be returned, as listed from 1 to the number of pings in the event, or a vector of time points given as strings "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF" from which the range of the time points to be read is extracted. If t=="all", all files are read and if t=="none" an empty list is returned.
#' @param xaxis						A character defining the variable to use for the x axis, one of (1) "time", assigning time to the x axis; (2) "dist" or "x", plotting salied distance along the x axis; (3) "pings", plotting the ping indices along the x axis; (4) "compr", ; (5) "sparse", which plots only the time points given in \code{t} with no spaces where thera are gaps (ping 13 may follow directly after ping 4).
#' @param cruise					is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' @param dir.data					is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#' @param max.memory				is the maximum memory allowed for the function to occupy.
#' @param breaks					has two possible inputs: (1) the number of breaks of the scale on which the data 'z' are arranged, equally spaced between min(z) and max(z) (or logarithmically equaly spaced when log=TRUE). (2) a vector of values for the breaks given in dB values (volume backscattering strength Sv), typically -82:-30 for EK60.
#' @param col						is either the color vector if length(col)>1, or the color function to generate the colors from. Currently the color function name must be one of "rainbow", "grey", "heat.colors", "terrain.colors", "topo.colors" and "cm.colors", but other color functions may be inplemented in the future. Set color.bar=NULL to supress plotting the color bar.
#' @param colpar					is a list of parameters used in colscale().
#' @param null.value				is is the value to set non-postitive values in the case of logarithmic plotting. The default is the only non-numeric value allowed, and implies the smallest positive value of the data.
#' @param beamstypes				***********************************
#' @param grid						***********************************
#' @param white						***********************************
#' @param log						***********************************
#' @param esnm						is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#' @param var						is a string specifying the variable to plot. Currently supported are "vbsc", for volume backscattering data, "sgsc"/"sgs0"/"sgsE"/"sgsi"/"sgsI", for (thresholded) segmentation data, "pr0s" or "psis" for unhtresholded not-school probability data, and "tlns" for pure estimated noise.
#' @param ind						is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range						is a list of elements with names matching names of 'data', specifying the range of the corresponding elements.
#' @param subset					is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param plot						is FALSE to prevent plotting.
#' @param cs.xyzlim					defines the coordinate system to use when specifying the plotting frame. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param adds						is an optional list of variables overriding the variables in 'data'.
#' @param origin 					***********************************
#' @param clock						is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' @param cex.clock					is the numeric character expansion value for the clock.
#' @param format.clock				is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\\n" apparing in 'format.clock' cause a line break.
#' @param digits.clock				is a numeric specifying the number of digits of the seconds part of the time.
#' @param col.clock					is the color of the plotted time and date.
#' @param tres,xres,zres,rres,bres	Used when compressing data which are too large for plotting. Preferably create a compressed event by compr.event(), which places the compressed event in the same directory as the original event (named e.g. tsd_compr1), and then plot this event
#' @param funvbsc					is the function to apply in the compression, either given as function or as a string, in which case the strings "mean" and "median" represents fast versions of the functions with the corresponding names (sum()/length() and fastMedian(), respectively).
#' @param funt						is the same as funvbsc, but used for averaging vessel data in the new time/distance bins.
#' @param ...						are inputs passed on to pplot2d.TSD, and cplot2d.TSD(). Also the direction of the light source in the plot is given here by the variables 'theta' and 'phi' (see rgl.light, default is to clear the light source).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname cplot2d.event
#'
cplot2d.event<-function(
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
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-12-09 - Clean version.
	########### DESCRIPTION: ###########
	# Echogram.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	
	###############################################################
	##### See cpplot2d.event() for the parameter descriptions #####
	###############################################################
	
	##################################################
	##################################################
	cpplot2d.event(
		# Specifying whether to use cplot3d or pplot3d:
		cpplot2d_type="c", 
		# Used in read.event() and elsewhere in cplot3d.event():
		event=event, t=t, cruise=cruise, dir.data=dir.data, max.memory=max.memory, 
		# Used in cplot3d.TSD():
		breaks=breaks, col=col, colpar=colpar, null.value=null.value, beamstypes=beamstypes, grid=grid, 
		# Used in cplot3d.plot.color.bar():
		white=white, log=log, endcol=endcol, 
		# Used in pplot2d.TSD() and cplot2d.TSD():
		esnm=esnm, var=var, ind=ind, range=range, subset=subset, plot=plot, cs.xyzlim=cs.xyzlim, xlim=xlim, ylim=ylim, zlim=zlim, tlim=tlim, up=up, freq=freq, wb=wb, rmar=rmar, xaxis=xaxis, gap=gap, gapthr=gapthr, tol=tol, heave=heave, x0=x0, unit=unit, date=date, nticksx=nticksx, 
		# Used for plotting with plot3d():
		adds=adds, origin=origin, 
		# Used when plotting date and time:
		clock=clock, cex.clock=cex.clock, format.clock=format.clock, digits.clock=digits.clock, col.clock=col.clock, 
		# Used in compr.TSD():
		tres=tres, xres=xres, zres=zres, rres=rres, bres=bres, funvbsc=funvbsc, funt=funt, cs=cs, 
		# Passed on to other functions:
		...)
	##################################################
	##################################################
	}
