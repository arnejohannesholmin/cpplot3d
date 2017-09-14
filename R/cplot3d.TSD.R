#*********************************************
#*********************************************
#' Interactive scatterplot of the (3 dimensional) position data and response data included in the input list 'data', where the position data must have names ("psxx", "psyx", "pszx"), ("x", "y", "z"), ("s1", "s2", "s3"), ("psx", "psy", "psz"), or be named "coords", including all postition data in a column matrix. The response data must be named "vbsc", "mvbs", "data", "z", "sv" or "b" (case insentive). If none of the position data names are recognized, the first element of 'data' is taken to be the response and the next three to be the postition data, if more than 3 elements are present.
#'
#' @param data  is a list containing the data to plot, as returned from read.event(...,var=c("vbsc","voxels","ctd","beams","vessel","time")). May include school positions data$psxf, data$psyf and data$pszf. If non-school voxel probabilities are given in an array 'pr0s', or thresholded segmentation values 'sgsc'/'sgs0' are given, these can be plotted using the input 'var'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d plot3d rgl.viewpoint
#' @importFrom sonR get.specs.esnm meanSv.TSD subset_TSD
#' @importFrom TSD ind.expand labl.TSD NAs sph2car strff utim.TSD
#'
#' @export
#' @rdname cplot3d.TSD
#'
cplot3d.TSD<-function(
	# Main variable:
	data, 
	# Used in cplot3d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), size=cplot3d.size("pow",y=20,par=4), clamp=c(0,1), shrink=TRUE, null.value=NA, beamstypes=1, 
	# Used in cplot3d.plot.color.bar():
	white=1, log=TRUE, color.bar="x--", color.bar.lwd=8, color.bar.adj=0, color.bar.tadj=0.1, color.bar.noWhite=TRUE, color.bar.nticks=8, color.bar.tickw=0.005, color.bar.tickcol="black", db=TRUE, voxels.out=FALSE, endcol=c("", ""), 
	# Used in pplot3d.TSD() and cplot3d.TSD():
	esnm="MS70", var=c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns"), ind=list(-(1:150),NULL), range=list(), subset=NULL, ideal=TRUE, seabed=-12000, rot=2, compensation=c("pitch","roll"), plot=TRUE, cs.xyzlim="g", add=FALSE, 
	# Used for plotting with plot3d():
	adds=NULL, xlim=NULL, ylim=NULL, zlim=NULL, view=c("free","top","bottom","south","west","north","east"), zoom=0.7, fov=60, 
	# Used for plotting the school:
	school=FALSE, schoolcol="purple", schoolsize=0.3, schoolsample=0.01, schoollen=4, schoollwd=1, schooltype="p", schoolcrop=FALSE, 
	# Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
	plot.seg=FALSE, seg.col="green", seg.alpha=0.2, subdivide=3, excl.neg=TRUE, object=c("ellipsoid","cuboid"), par=double(3), center=c(0,0,0), angle=0, 
	# Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate():
	aspect="iso", nticks=5, origin=1, xlab="x", ylab="y", zlab="z", full.box=FALSE, 
	# Used when plotting vessel position:
	edge.vpos="", line.vpos=0, at.vpos=NULL, cex.vpos=1, col.vpos="blue", 
	# Used when plotting date and time:
	clock="bbl", cex.clock=1, adj.clock=c(0,0), format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, lsp.clock=0.04, col.clock=4, 
	# Used when plotting the sonar grid:
	sonar.grid="frame", sonar.grid.col=c("orange","cornflowerblue"), sonar.grid.lwd=1, cs.pos="g", cs.view="g", sides=c("tb","tb"), dens=c(200,100,100,1), every=Inf, 
	# Used when plotting the global grid:
	global.grid=FALSE, global.grid.lwd=0.5, global.grid.lty=1, 
	# Passed on to add.sonar.grid(), meanSv.TSD(), plot3d(), and decorate3d():
	...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-02 - Clean version.
	# Update: 2010-02-19 - Changed to supporting the subset list 'subset', used in extract.
	# Update: 2010-03-02 - Added support for specifying the ranges of "x", "y" and "z" ('range').
	# Update: 2010-04-17 - Altered to letting the first and the last break move to infinity.
	# Update: 2011-01-06 - Added the option 'null.value' and 'white'.
	# Update: 2011-01-11 - Changed the plotting of the color bar, introducing the inputs 'color.bar' (placement of the color bar), 'color.brakes' (number of tick marks on the color bar), 'color.bar.tickw' (the thickness of the separating marks/ticks on the color bar) and 'color.bar.tickcol' (the color of the separating marks/ticks on the color bar).
	# Update: 2011-01-14 - Changed the input to one single list 'data', including 'vbsc', 'voxels', 'vessel' and 'beams'. Also added option of plotting the frame of the sonar volume using plot_volume().
	# Update: 2011-01-17 - Added the option 'shrink'.
	# Update: 2011-01-25 - Changed to be named cplot3d.TSD() instead of the old cplot3d(). The difference is that cplot3d.TSD() takes a list 'data' as input, in contrast to cplot3d() which takes response 'z' and spatial postitions 's' as input. Also some changes and bug fixes done.
	# Update: 2011-06-19 - Restructured and added the option xlim=NA or ylim=NA or zlim=NA, indicating that the the entire sonar frame should be uncluded in the plot.
	# Update: 2011-06-27 - Added the option 'color.bar.lwd' for specifying the width of the color bar.
	# Update: 2011-10-07 - Updated documentation, changed to having 'psxx', 'psyx', 'pszx' and 'vbsc' as input (discarding 's' and 'z' and the like), and changed to correspond to the method used in pplot3d.TSD().
	# Update: 2012-03-15 - Added the options 'nticks' and 'edge' for customizing axes individually.
	# Update: 2012-06-28 - Added support for plotting 'pr0s', 'sgsc' and 'sgs0', specified by 'var'.
	# Update: 2012-07-25 - Fixed bug for view="free".
	# Update: 2012-11-15 - Added 'color.bar.noWhite'.
	# Update: 2013-01-05 - Added the option var="tlns".
	# Update: 2013-01-06 - Cleaned up a bug with 'color.breaks'.
	# Update: 2013-01-08 - Added plotting of the objects enclosing the school, by setting schoolsample="s" (or some other character).
	# Update: 2013-03-18 - Changed extent of the color bar to fit the current plotting fram extracted from par3d().
	# Update: 2013-08-19 - Restructured function and categorized inputs.
	# Last: 2014-03-20 - Added support for lines representing the fish, through schooltype="l", with supporting parameter 'schoollen'
	########### DESCRIPTION: ###########
	# Interactive scatterplot of the (3 dimensional) position data and response data included in the input list 'data', where the position data must have names ("psxx", "psyx", "pszx"), ("x", "y", "z"), ("s1", "s2", "s3"), ("psx", "psy", "psz"), or be named "coords", including all postition data in a column matrix. The response data must be named "vbsc", "mvbs", "data", "z", "sv" or "b" (case insentive). If none of the position data names are recognized, the first element of 'data' is taken to be the response and the next three to be the postition data, if more than 3 elements are present.
	########## DEPENDENCIES: ###########
	# labl.TSD(), ind.expand(), subset_TSD(), echoSegment.TSD(), add.sonar.grid(), get.xyzlim(), colscale(), cplot3d.size(), cplot3d.plot.color.bar(), cpplot3d.decorate()
	############ VARIABLES: ############
	##########################
	##### Main variable: #####
	##########################
	# ---data--- is a list containing the data to plot, as returned from read.event(...,var=c("vbsc","voxels","ctd","beams","vessel","time")). May include school positions data$psxf, data$psyf and data$pszf. If non-school voxel probabilities are given in an array 'pr0s', or thresholded segmentation values 'sgsc'/'sgs0' are given, these can be plotted using the input 'var'.
	
	###############################################################
	##### See cpplot3d.event() for the parameter descriptions #####
	###############################################################
	
	
	##################################################
	##################################################
	##### Preparation #####
	### Variables used both in pplot.TSD() and cplot.TSD(): ###
	# 'clamp' needs to be a range vector:
	clamp = c(min(clamp), max(clamp))
	
	# Store the arguments to be passed on to plot3d():
	ll = list(...)
	# Merge the 'adds' and the data:
	if(length(adds)>0){
		data[names(adds)] = adds
		}
	# Define the vessel variables and the beams variables to read:
	vesselvar = labl.TSD("v")
	beamsvar = labl.TSD("rb")
	# Read the device name if present in 'data':
	if(length(data$esnm)>0){
		esnm = data$esnm
		}
	# Recycle 'dens' to length 4:
	dens = rep(dens, length.out=4)
	# If 'cs.pos' has length 3, it specifies all of 'cs.pos', 'cs.view', 'cs.xyzlim', in that order:
	if(length(cs.pos)==3){
		cs.view = cs.pos[2]
		cs.xyzlim = cs.pos[3]
		cs.pos = cs.pos[1]
		}
	# Remove voxel position data, to avoid redundant information when subsetting (??????????):
	if(length(plot.seg)==0){
		data = data[setdiff(names(data), c("psxx","psyx","pszx"))]
		}
	# Define the vessel variables to return:
	vesselnames = c("mtim","utim","psxv","psyv","pszv","rtzv","latv","lonv")
	# Use the current zoom of the plot if 'view' is "free", FALSE, or empty:
	if(length(view)==0 || identical(view,FALSE) || identical(view,"free")){
		zoom = par3d()$zoom
		}
	# Get the 'utim' values if missing:
	if(is.null(data$utim)){
		data$utim = unique(unlist(utim.TSD(data)))
		}
	# Set the 'view':
	view = set.view3d(view,cs.pos,cs.view,data$rtzv)
	# Define the number of time steps:
	numt = 1
	whichpresent = intersect(labl.TSD(var="as"),names(data))[1]
	if(is.list(data[[whichpresent]])){
		numt = length(data[[whichpresent]])
		}
	else if(length(dim(data[[whichpresent]]))==3){
		numt = dim(data[[whichpresent]])[3]
		}
	# Expand 'schoolcol' to 'numt':
	schoolcol = rep(schoolcol,length.out=numt)
	###
	
	
	########## Execution ##########
	##### Preparations for plotting: #####
	# Extract the specified variable to plot:
	data$vbsc = cpplot3d.extract_var(data,var)
	vbscpresent = length(data$vbsc)>0
	
	# Expand 'ind' to fit the dimension of the acoustic data:
	ind = ind.expand(ind, dim(data$vbsc), ...)
	ind = get.specs.esnm(data, ind=ind, beamstypes=beamstypes, var="ind")$ind
	
	# Subset the data:
	data = subset_TSD(data, ind=ind, range=range, subset=subset, ind.out=TRUE, drop=FALSE)
	# Linearize:
	if(length(data$vbsc)==0 && length(data$mvbs)>0){
		data$vbsc = 10^(data$mvbs/10)
		}
	# If data$vbsc is complex, take the Mod():
	if(is.complex(data$vbsc)){
		data$vbsc = array(Mod(data$vbsc),dim=dim(data$vbsc))
		}
	# If data$vbsc is logical, transform to numeric:
	if(is.logical(data$vbsc)){
		data$vbsc = as.numeric(data$vbsc)
		}
	# Abort with a warning if no 'vbsc' remain in the data:
	if(length(data$vbsc)==0){
		if(vbscpresent){
			warning("No data plotted, possibly due to empty segmentation mask")
			return(list())
		}
		else{
			warning("No data plotted, possibly due to full exclusion of the data using 'range', 'ind', or 'subset'")
			return(list())
		}
	}
	if(sum(data$vbsc, na.rm=TRUE)==0){
		warning("All data equal to 0")
		return(list())
	}
	# Remove dimension on the position and acoustic data:
	data[c("psxx","psyx","pszx","vbsc")] = lapply(data[c("psxx","psyx","pszx","vbsc")],c)
	###
	
	# Store the indexes for the subvolume defined by the object given by inputs 'object', 'par', 'center' and 'angle'. These indexes are used for plotting of only the voxels inside the subvolume or only the voxels outside of the subvolume (only used in the case that plot.seg is "only" or "but"):
	inside = list(tvbs=NA)
	if(tolower(substr(plot.seg[1],1,2))=="on"){
		inside = echoSegment.TSD(data,object=object,par=par,center=center,angle=angle,plot=FALSE)
		data$psxx = data$psxx[inside$sgmt,,drop=FALSE]
		data$psyx = data$psyx[inside$sgmt,,drop=FALSE]
		data$pszx = data$pszx[inside$sgmt,,drop=FALSE]
		data$vbsc = data$vbsc[inside$sgmt]
		}
	else if(tolower(substr(plot.seg[1],1,2))=="bu"){
		inside = echoSegment.TSD(data,object=object,par=par,center=center,angle=angle,plot=FALSE)
		data$psxx = data$psxx[!inside$sgmt,,drop=FALSE]
		data$psyx = data$psyx[!inside$sgmt,,drop=FALSE]
		data$pszx = data$pszx[!inside$sgmt,,drop=FALSE]
		data$vbsc = data$vbsc[!inside$sgmt]
		}
		
	# Log transfomation may enhace the appearence of the plot (set to the default on 2011-01-11):
	if(log){
		if(min(data$vbsc,na.rm=TRUE)<=0){
			# Set all non-postitve values to the minimum of the data (or .Machine$double.neg.eps if the miminum is not available) if null.value=="min.pos", and to 'null.value' otherwise:
			if(identical(null.value,"min.pos")){
				if(sum(data$vbsc>0,na.rm=TRUE)==0){
					null.value = .Machine$double.neg.eps
					warning("No data in the specified subset and range are positive. All set to .Machine$double.neg.eps for logarithmic plot")
					}
				else{
					null.value = min(data$vbsc[data$vbsc>0],na.rm=TRUE)
					if(identical(null.value,max(data$vbsc,na.rm=TRUE))){
						null.value = .Machine$double.neg.eps
						}
					else{
						warning(paste("Non-positive input data set to null.value = ",format(null.value,scientific=TRUE,digits=3)," for logarithmic plot",sep=""))
						}
					}
				}
			else{
				data$vbsc[data$vbsc<=0] = null.value
				}
			}
		data$vbsc = 10*log10(data$vbsc)
		}
	
	# Get the ranges of the inputs:
	rangevbsc = c(min(data$vbsc,na.rm=TRUE),max(data$vbsc,na.rm=TRUE))
	
	# Treatment of breaks and color:
	breaks = cplot3d.col_breaks(col, colpar, breaks, white, clamp, log, rangevbsc, endcol)
	col = breaks$col
	white = breaks$white
	breaks = breaks$breaks
	lbreaks = length(breaks)-1
	
	# Shrink the data due to 'white':
	if(shrink){
		tobeplotted = data$vbsc>breaks[white+1] & !is.na(data$vbsc)
		data$vbsc = data$vbsc[tobeplotted]
		data$psxx = data$psxx[tobeplotted]
		data$psyx = data$psyx[tobeplotted]
		data$pszx = data$pszx[tobeplotted]
		}
	
	# If the individual fish positions are available and 'school' is a numeric, plot the school:
	if(!identical(school,FALSE) && !any(is.null(data$psxf),is.null(data$psyf),is.null(data$pszf)) && !is.character(schoolsample)){
		# Convert the fish poritions to a list of each time step, to ease the treament of the positions:
		convert2list = function(x,n){
			if(length(dim(x))==0){
				c(list(x),vector("list",n-1))
				}
			else if(length(dim(x))==2){
				split(x, col(x))
				}
			else if(is.list(x) && length(x)<n){
				c(x,vector("list",n-length(x)))
				}
			}
		data[c("psxf","psyf","pszf")] = lapply(data[c("psxf","psyf","pszf")],convert2list,numt)
		if(length(data$rtxf)>0){
			data$rtxf = convert2list(data$rtxf,numt)
			}
		if(length(data$rtzf)>0){
			data$rtzf = convert2list(data$rtzf,numt)
			}
		
		# Draw the school sample to reduce the number of points plotted:
		for(p in seq_along(data$psxf)){
			thissample = sample(seq_along(data$psxf[[p]]),length(data$psxf[[p]])*schoolsample)
			data$psxf[[p]] = data$psxf[[p]][thissample]
			data$psyf[[p]] = data$psyf[[p]][thissample]
			data$pszf[[p]] = data$pszf[[p]][thissample]
			if(length(data$rtxf)>0){
				data$rtxf[[p]] = data$rtxf[[p]][thissample]
				}
			if(length(data$rtzf)>0){
				data$rtzf[[p]] = data$rtzf[[p]][thissample]
				}
			}
		}
	
	# Expand the ranges to the sonar plot grid if any of 'xlim', 'ylim' or 'zlim' are equal to NA (no plotting due to xyzlim=NA):
	if(suppressWarnings(any(is.na(xlim),is.na(ylim),is.na(zlim))) && sonar.grid[1] %in% c("frame","proj")){
		xyzlim = add.sonar.grid(data, xyzlim.out=TRUE, sonar.grid=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, ...)
		}
	# Else set the range of the plotting volume:
	else{
		xyzlim = get.xyzlim(xlim=xlim,ylim=ylim,zlim=zlim,data=data,data$indt[1],school=school,cs.pos=cs.pos,cs.xyzlim=cs.xyzlim)
		}
	
	# Extract the color bar colors from 'col':
	if(color.bar.noWhite){
		color.bar.col = col[seq(white+1,lbreaks)]
		}
	else{
		color.bar.col = col
		}
	
	# Expand the length of 'size' if needed:
	if(is.function(size)){
		size = size(lbreaks)
		}
	else if(is.list(size)){
		size = do.call("cplot3d.size", size)(lbreaks)
		}
	if(length(size)!=lbreaks){
		size = rep(size,length.out=lbreaks)
		}
	
	# Add a small value to non-positive size values:
	size[size==0] = 1e-20
	
	
	########## Plot only if plot=TRUE: ##########
	plotted=0
	if(plot){
		# The number of bins of the breaks that contain data and are not skiped by 'white':
		thisadd = add
		if(voxels.out){
			vind = NULL
			xout = NULL
			yout = NULL
			zout = NULL
			svout = NULL
			}
		
		# New method: Plot all at once:
		#if(packageVersion("rgl") > 0.9){
		#	colvec <- col[findInterval(data$vbsc, breaks, rightmost.closed=TRUE)]
		#	notna =! is.na(data$psxx[this])
		#	calllist = c(list(x=data$psxx[notna], y=data$psyx[notna], z=data$pszx[notna], col=colvec, size=size[i], add=thisadd, xlim=xyzlim[,1], ylim=xyzlim[,2], zlim=xyzlim[,3], axes=FALSE, box=FALSE, xlab="", ylab="", zlab=""), ll, list(xlab="", ylab="", zlab=""))
		#	do.call("plot3d",calllist[unique(names(calllist))])
		#	}
		
		# Plot the data in breaks (before this was to meke it possible to plot different colors, but now as of 2016-12-12 (at least) this works. However different sizes of the points does not work):
		else{
			for(i in seq_along(breaks[-1])){
				# Include the lowest point:
				if(i==1){
					this = data$vbsc<=breaks[i+1]
					}
				else if(i==lbreaks){
					this = data$vbsc>breaks[i]
					}
				else{
					this = breaks[i]<data$vbsc & data$vbsc<=breaks[i+1]
					}
				# Only plot if there are any non-NA values and the current break is not discarded from plotting by 'white':
				if(any(this,na.rm=TRUE) && !is.na(col[i])){
					notna=!is.na(data$psxx[this])
					if(voxels.out){
						vind = c(vind,which(this)[notna])
						xout = c(xout,data$psxx[this][notna])
						yout = c(yout,data$psyx[this][notna])
						zout = c(zout,data$pszx[this][notna])
						svout = c(svout,10^(data$vbsc[this][notna]/10))
						}
					calllist = c(list(x=data$psxx[this][notna], y=data$psyx[this][notna], z=data$pszx[this][notna], col=col[i], size=size[i], add=thisadd, xlim=xyzlim[,1], ylim=xyzlim[,2], zlim=xyzlim[,3], axes=FALSE, box=FALSE, xlab="", ylab="", zlab=""), ll, list(xlab="", ylab="", zlab=""))
					do.call("plot3d",calllist[unique(names(calllist))])
					# Update 'thisadd':
					thisadd = TRUE
					# Update 'plotted':
					plotted = plotted+length(data$psxx[this][notna])
					}
				}
			}
		if(plotted==0){
			warning("No data plotted. 'white' equal to the number of breaks, or too high so that all bins with data are set to white, or possibly no data are contained in the specified plotting frame.")
			}
	
		# Plot the color bar of the plot:
		if(!any(length(color.bar)==0, nchar(color.bar)==0, identical(color.bar,FALSE))){
			xyzlim = cplot3d.plot.color.bar(color.bar.col, breaks=breaks, white=white, log=log, color.bar=color.bar, color.bar.lwd=color.bar.lwd, color.bar.adj=color.bar.adj, color.bar.tadj=color.bar.tadj, color.bar.noWhite=color.bar.noWhite, color.bar.nticks=color.bar.nticks, color.bar.tickw=color.bar.tickw, color.bar.tickcol=color.bar.tickcol, db=db)
			}
	
		# Plot the school:
		if(!identical(school,FALSE) && !any(is.null(data$psxf),is.null(data$psyf),is.null(data$pszf)) && !is.character(schoolsample)){
			for(p in seq_along(data$psxf)){
				if(schoolcrop){
					inside = xyzlim[1,1]<data$psxf[[p]] & xyzlim[2,1]>data$psxf[[p]] & xyzlim[1,2]<data$psyf[[p]] & xyzlim[2,2]>data$psyf[[p]] & xyzlim[1,3]<data$pszf[[p]] & xyzlim[2,3]>data$pszf[[p]]
					}
				else{
					inside = seq_along(data$psxf[[p]])
					}
				data$psxf[[p]] = data$psxf[[p]][inside]
				data$psyf[[p]] = data$psyf[[p]][inside]
				data$pszf[[p]] = data$pszf[[p]][inside]
				
				# Plot the directions of the fish if the rotation angles were read in cpploted.event():
				if(any(strff(c("l","o"),schooltype))){
					data$rtxf[[p]] = data$rtxf[[p]][inside]
					data$rtzf[[p]] = data$rtzf[[p]][inside]
					# Define line segments representing fish with directions:
					addxyz = sph2car(cbind(schoollen/2,pi/2+data$rtzf[[p]],pi/2+data$rtxf[[p]]))
					fromto = NAs(3*length(data$psxf[[p]]),3)
					fromto[seq(1,nrow(fromto),3),] = cbind(data$psxf[[p]],data$psyf[[p]],data$pszf[[p]])-addxyz
					fromto[seq(2,nrow(fromto),3),] = cbind(data$psxf[[p]],data$psyf[[p]],data$pszf[[p]])+addxyz
					# Plot lines:
					calllist = c(list(x=fromto[,1],y=fromto[,2],z=fromto[,3],add=TRUE,axes=FALSE,box=FALSE,col=schoolcol[p],lwd=schoollwd,type="l"), ll, list(xlab="",ylab="",zlab=""))
					do.call("plot3d",calllist[unique(names(calllist))])
					# Add points at the heads:
					if(strff("o",schooltype)){
						calllist = c(list(x=fromto[seq(2,nrow(fromto),3),1],y=fromto[seq(2,nrow(fromto),3),2],z=fromto[seq(2,nrow(fromto),3),3],add=TRUE,axes=FALSE,box=FALSE,col=schoolcol[p],size=schoolsize), ll, list(xlab="",ylab="",zlab=""))
						do.call("plot3d",calllist[unique(names(calllist))])
						}
					}
				# Plot points:
				if(strff("p",schooltype)){
					calllist = c(list(x=data$psxf[[p]],y=data$psyf[[p]],z=data$pszf[[p]],add=add | p!=1 | length(plotted)>0,axes=FALSE,box=FALSE,col=schoolcol[p],size=schoolsize), ll, list(xlab="",ylab="",zlab=""))
					do.call("plot3d",calllist[unique(names(calllist))])
					}		
				}
			}
		else if(!identical(school,FALSE) && is.character(schoolsample) && all(c("Sctr","Sobj","Spar","Sang") %in% names(data))){
			if(length(data$Sobj)>1){
				dim(data$Spar) = c(length(data$Sobj),3)
				dim(data$Sctr) = c(length(data$Sobj),3)
				for(p in seq_len(nrow(data$Spar))){
					echoSegment.TSD(object=data$Sobj[p],par=data$Spar[p,],center=data$Sctr[p,],angle=data$Sang[p],plot=TRUE,seg.col=schoolcol[p],alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
					}
				}
			else{
				echoSegment.TSD(object=data$Sobj,par=data$Spar,center=data$Sctr,angle=data$Sang,plot=TRUE,seg.col=schoolcol[1],alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
				}
			}
	
		##### The following features (view, zoom, field of view, aspect) can be plotted at the level of pplot3d.TSD(), and will remain unchanged at the level of pplot3d.event(): #####
		# Set the view, zoom, field of view, and aspect:
		do.call("rgl.viewpoint",list(userMatrix=view,zoom=zoom,fov=fov))
		# Plot the global grid:
		if(isTRUE(global.grid)){
			global.grid = 100
			}
		
		##### Add the following features to the plot: (1) bounding box and titles, (2) axes, (3) vessel positions, (4) date and time, (5) sonar grid, (6) global grid. If pplot3d.TSD() is called from pplot3d.event(), these features are plotted at the level of pplot3d.event() instead, indicated by the following parameters specified when calling pplot3d.TSD() in pplot3d.event(): box=FALSE, axes=FALSE, title=list(xlab="", ylab="", zlab=""), edge.vpos=FALSE, clock=FALSE, sonar.grid=FALSE: #####
		cpplot3d.decorate(
			# (1) Used in decorate_pplot_cplot():
			data=data[c("utim",vesselvar,beamsvar)], plot=TRUE, 
			# (2) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout decorate_pplot_cplot():
			aspect=aspect, xyzlim=xyzlim, nticks=nticks, origin=origin, xlab=xlab, ylab=ylab, zlab=zlab, full.box=full.box, 
			# (3) Used when plotting vessel position:
			edge.vpos=edge.vpos, line.vpos=line.vpos, at.vpos=at.vpos, cex.vpos=cex.vpos, col.vpos=col.vpos, 
			# (4) Used when plotting date and time:
			clock=clock, cex.clock=cex.clock, adj.clock=adj.clock, format.clock=format.clock, digits.clock=digits.clock, lsp.clock=lsp.clock, col.clock=col.clock, 
			# (5) Used when plotting the sonar grid:
			sonar.grid=sonar.grid, sonar.grid.col=sonar.grid.col, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, 
			# (6) Used when plotting the global grid:
			global.grid=global.grid, global.grid.lwd=global.grid.lwd, global.grid.lty=global.grid.lty, 
			# Passed on to add.sonar.grid():
			...)
		}
	########## End of plot ##########
		
	
	########## Output ##########
	# Add the subvolume used as a segmentation of the voxels:
	msvM = NULL
	if(isTRUE(plot.seg) || tolower(substr(plot.seg[1],1,2))=="ob"){
		if(length(dim(center))==2){
			for(i in seq_len(nrow(center))){
				inside = echoSegment.TSD(data,object=object,par=if(length(dim(par))==2) par[i,] else par,center=center[i,],angle=angle,plot=plot,seg.col=seg.col,alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
				warning("Estimation of mean sv with multiple segmentation objects is not supported in this version")
				}
			}
		else{
			inside = echoSegment.TSD(data,object=object,par=par,center=center,angle=angle,plot=plot,seg.col=seg.col,alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
			# Estimate the mean Sv inside the segment:
			if(length(data$vbsc)>0){
				suppressWarnings(msvM<-meanSv.TSD(inside,...)[1])
				}
			}
		}
	else{
		inside<-echoSegment.TSD(data,object=object,par=par,center=center,angle=angle,plot=FALSE,seg.col=seg.col,alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
		}
	
	# Output:
	if(voxels.out){
		invisible(list(br = if(!log) 10*log10(breaks) else breaks, view = view, xyzlim=xyzlim, tvbs=inside$tvbs, tvol=inside$tvol, sgmt=inside$sgmt, psxs=inside$psxs, psys=inside$psys, pszs=inside$pszs, vbss=inside$vbss, vols=inside$vols, dsts=inside$dsts, vind=vind, xout=xout, yout=yout, zout=zout, svout=svout, msvM=msvM, plotted=plotted))
		}
	else{
		invisible(list(br = if(!log) 10*log10(breaks) else breaks, view = view, xyzlim=xyzlim, tvbs=inside$tvbs, tvol=inside$tvol, sgmt=inside$sgmt, psxs=inside$psxs, psys=inside$psys, pszs=inside$pszs, vbss=inside$vbss, vols=inside$vols, dsts=inside$dsts, msvM=msvM, plotted=plotted))
		}
	##################################################
	##################################################
	}
