#*********************************************
#*********************************************
#' This function is the point representation analogue to cplot3d.TSD, and plots the acoustic data in 3d by point representation using the R package rgl (similar to applot.event() but only one time step) ????.
#'
#' @param data  is a list containing the data to plot, as returned from read.event(...,var=c("vbsc","voxels","ctd","beams","vessel","time")). May include school positions data$psxf, data$psyf and data$pszf. If non-school voxel probabilities are given in an array 'pr0s', or thresholded segmentation values 'sgsc'/'sgs0' are given, these can be plotted using the input 'var'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d plot3d rgl.viewpoint
#' @importFrom sonR meanSv.TSD
#' @importFrom TSD dim_all labl.TSD NAs sph2car strff utim.TSD zeros
#'
#' @export
#' @rdname pplot3d.TSD
#'
pplot3d.TSD<-function(
	# Main variable:
	data, 
	# Used in pplot3d_sv2pos.TSD() and elsewhere in pplot3d.event():
	N=1e5, acca=NULL, fun="mod", allert=1e8, nlim=NULL, fact=NULL, cols=c("black","navyblue","magenta4","red3","darkorange2","yellow"), stretch=1, scale.voxels=1, rand.gen=c("unif","beta","norm"), beamstypes=1, possample=1, 
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
	clock="bbl", cex.clock=1, adj.clock=c(0,0), format.clock="yyyy-mm-dd\nHH:MM:SS.FFF\nPing: indt", digits.clock=2, lsp.clock=0.04, col.clock=4, 
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
	# Start: 2011-04-26 - Clean version.
	# Update: 2011-09-11 - Changed from having 'xyzlim' as input to having individual 'xlim', 'ylim' and 'zlim'. Also changed to accept multiple time steps and fixed some bugs.
	# Update: 2011-09-11 - Changed to applying subset_TSD() in pplot3d.TSD() instead of inside sv2pos.TSD(), so that for more than one time step, the subertting is only done once, saving time. Also applied use of add.sonar.grid(), and subset_TSD() instead of the old subset.MS70(), thus adding support for EK60, ME70, MS70 and SH80.
	# Update: 2011-10-07 - Various changes, such as reassigning subsetting of the data to sv2pos.TSD(), due to problems in keeping the voxels structure when 'range' is given.
	# Update: 2012-03-15 - Added the options 'nticks' and 'edge' for customizing axes individually.
	# Update: 2012-05-16 - Changed from using 'bigcol' for the color of the points representing many fish, to 'cols', which is a two element color vector holding both the color of the normal points and the big points.
	# Update: 2012-06-02 - Added the parameter 'N', which if given defines the approximate number of points to plot. A suited value is 1e5.
	# Update: 2012-06-28 - Changed default to using 'N' instead of 'acca'. If 'acca' is given, this overrides N (see "sv2pos.TSD.R").
	# Update: 2012-06-28 - Added support for plotting 'pr0s', 'sgsc' and 'sgs0', specified by 'var'.
	# Update: 2012-07-25 - Fixed bug for view="free".
	# Update: 2013-01-05 - Added the option var="tlns".
	# Update: 2013-01-08 - Added plotting of the objects enclosing the school, by setting schoolsample="s" (or some other character).
	# Update: 2013-03-11 - Added fun="mod3" and "mod2.7" and so on.
	# Update: 2013-07-16 - Restructured function and categorized inputs.
	# Update: 2013-08-09 - Fixed bug with the bounding box not being plotted.
	# Update: 2013-08-16 - Changed to apply the function cpplot3d.decorate() to plot axes, titles, vessel positions, date and time, and sonar grid.
	# Update: 2013-09-12 - Added the parameters 'scale.voxels' and 'rand.gen'.
	# Last: 2014-03-20 - Added support for lines representing the fish, through schooltype="l", with supporting parameter 'schoollen'
	########### DESCRIPTION: ###########
	# This function is the point representation analogue to cplot3d.TSD, and plots the acoustic data in 3d by point representation using the R package rgl (similar to applot.event() but only one time step) ????.
	########## DEPENDENCIES: ###########
	# labl.TSD(), set.view3d(), pplot3d.set.default.nlim(), pplot3d_sv2pos.TSD(), add.sonar.grid(), get.xyzlim(), echoSegment.TSD(), cpplot3d.decorate(), meanSv.TSD()
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
	# Store the arguments to be passed on to plot3d():
	ll = list(...)
	# Accept only the supported variables in 'var':
	#legalvar = c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns")
	legalvar = labl.TSD("as")
	varClean = gsub( "[^[:alnum:],]", "", var)
	var = var[varClean %in% legalvar]
	#var = intersect(gsub( "[^[:alnum:],]", "", var ), legalvar)
	# Merge the 'adds' and the data:
	if(length(adds)>0){
		data[names(adds)] = adds
		}
	# Define the vessel variables and the beams variables to read:
	vesselvar = labl.TSD("v")
	beamsvar = labl.TSD("rb")
	# Read the device name if present in 'data':
	if(length(data$esnm)>0){
		esnm = data$esnm[1]
		}
	# Recycle 'dens' to length 4:
	dens = rep(dens,length.out=4)
	# If 'cs.pos' has length 3, it specifies all of 'cs.pos', 'cs.view', 'cs.xyzlim', in that order:
	if(length(cs.pos)==3){
		cs.view = cs.pos[2]
		cs.xyzlim = cs.pos[3]
		cs.pos = cs.pos[1]
		}
	# Remove voxel position data, to avoid redundant information when subsetting:
	if(length(plot.seg)==0){
		data = data[setdiff(names(data),c("psxx","psyx","pszx"))]
		}
	# Define the vessel variables to return:
	vesselnames = c("mtim","utim","psxv","psyv","pszv","rtzv","latv","lonv")
	# Use the current zoom of the plot if 'view' is "free", FALSE, or empty:
	if(plot && length(view)==0 || identical(view,FALSE) || identical(view,"free")){
		zoom = par3d()$zoom
		}
	# Get the 'utim' values if missing:
	if(is.null(data$utim)){
		data$utim = unique(unlist(utim.TSD(data)))
		}
	# Set the 'view':
	if(plot){
		view = set.view3d(view, cs.pos, cs.view, data$rtzv[1])
		}
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
	
	### Variables specific for pplot.TSD() (not used by cplot.TSD()): ###
	# Default 'nlim':
	nlim = pplot3d.set.default.nlim(nlim=nlim,var=var,esnm=esnm)
	# Recycle 'cols' to length 2:
	#if(length(cols)==1){
	#	cols = c(cols,cols)
	#	}
	###
	
	
	##### Execution #####
	# Generate positions based on the acoustic data:
	finalacca = NULL
	finalN = 0
	if(!all(c("psxr","psyr","pszr") %in% names(data))){
		# Declare the position data in lists of voxel positions vomponents in size classes:
		data$psxr = lapply(vector("list",length(nlim)+1), function(x) vector("list",numt))
		data$psyr = lapply(vector("list",length(nlim)+1), function(x) vector("list",numt))
		data$pszr = lapply(vector("list",length(nlim)+1), function(x) vector("list",numt))
		# Store multiple time steps:
		for(p in seq_len(numt)){
			pos = pplot3d_sv2pos.TSD(data, N=N/numt, acca=acca, fun=fun, t=p, esnm=esnm, var=var, ind=ind, range=range, subset=subset, beamstypes=beamstypes, cs=cs.pos, ideal=ideal, allert=allert, plot=FALSE, seabed=seabed, rot=rot, compensation=compensation, nlim=nlim, fact=fact, cols=cols, stretch=stretch, scale.voxels=scale.voxels, rand.gen=rand.gen, total.out=FALSE, ...)
			
			finalacca = c(finalacca,pos$acca)
			# Insert the regenerated points into 'data':
			for(l in seq_along(pos$regen)){
				if(length(pos$regen[[l]])>0){
					data$psxr[[l]][[p]] = pos$regen[[l]][,1]
					data$psyr[[l]][[p]] = pos$regen[[l]][,2]
					data$pszr[[l]][[p]] = pos$regen[[l]][,3]
					}
				}
			finalN = finalN+pos$N
			}
		}
	
	
	# Drop levels with no points if there are no higher levels with points:
	finalNatlevels = sapply(lapply(dim_all(data$psxr),unlist),sum)
	if(any(finalNatlevels>0)){
		lastnonemptylevel = max(which(finalNatlevels>0))
		data$psxr = data$psxr[seq_len(lastnonemptylevel)]
		data$psyr = data$psyr[seq_len(lastnonemptylevel)]
		data$pszr = data$pszr[seq_len(lastnonemptylevel)]
		}
	else{
		data$psxr = list()
		data$psyr = list()
		data$pszr = list()
		}
	
	# Generate positions based on the acoustic data:
	#finalacca=NULL
	#finalN=0
	#if(!all(c("psxr","psyr","pszr") %in% names(data))){
	#	# Declare the position data:
	#	data$psxr=vector("list",length(nlim)+1)
	#	data$psyr=vector("list",length(nlim)+1)
	#	data$pszr=vector("list",length(nlim)+1)
	#	# Store multiple time steps:
	#	for(p in seq_len(numt)){
	#		pos=pplot3d_sv2pos.TSD(data, N=N/numt, acca=acca, fun=fun, t=p, esnm=esnm, var=var, ind=ind, range=range, subset=subset, beamstypes=beamstypes, cs=cs.pos, ideal=ideal, allert=allert, plot=FALSE, seabed=seabed, rot=rot, compensation=compensation, nlim=nlim, fact=fact, cols=cols, stretch=stretch, total.out=FALSE)
	#		finalacca=c(finalacca,pos$acca)
	#		# Insert the regenerated points into 'data':
	#		for(p in seq_along(pos$regen)){
	#			if(length(pos$regen[[p]])>0){
	#				data$psxr[[p]]=c(data$psxr[[p]],pos$regen[[p]][,1])
	#				data$psyr[[p]]=c(data$psyr[[p]],pos$regen[[p]][,2])
	#				data$pszr[[p]]=c(data$pszr[[p]],pos$regen[[p]][,3])
	#				finalN=finalN+length(pos$regen[[p]][,1])
	#				}
	#			}
	#		}
	#	}
	
	
	# If the individual fish positions are available and 'school' is a numeric, plot the school:
	if(!identical(school,FALSE) && !any(is.null(data$psxf),is.null(data$psyf),is.null(data$pszf)) && !is.character(schoolsample)){
		# Convert the fish positions to a list for each time step, to ease the treament of the positions:
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
		xyzlim = get.xyzlim(xlim=xlim, ylim=ylim, zlim=zlim, data=data, data$indt[1], school=isTRUE(school), cs.pos=cs.pos, cs.xyzlim=cs.xyzlim)
		}
	
	
	# Unlist the time steps:
	plotpsxr = lapply(data$psxr, unlist)
	plotpsyr = lapply(data$psyr, unlist)
	plotpszr = lapply(data$pszr, unlist)
	
	
	########## Plot only if plot=TRUE: ##########
	if(plot){
		# Store the rgl plotting indices:
		#idx = zeros(length(plotpsxr),5)
		idx = NAs(max(length(plotpsxr),1), 5)
		
		# Allow for different size for the different levels:
		if(length(ll$size)>0){
			if(is.function(ll$size)){
				size = 0.3
				ll$size = NULL
				}
			else{
				size = rep(ll$size,length.out=length(plotpsxr))
				ll$size = NULL
				}
			}
		else{
			size = double(length(plotpsxr))+1
			}
		# Plot the regenerated points in 3D:
		for(p in seq_along(plotpsxr)){
			# Draw a random sample of the points:
			if(possample<1 && possample>0){
				len = length(plotpsxr[[p]])
				valid = sample.int(len,len*possample)
				plotpsxr[[p]] = plotpsxr[[p]][valid]
				plotpsyr[[p]] = plotpsyr[[p]][valid]
				plotpszr[[p]] = plotpszr[[p]][valid]
				}
			
			if(length(plotpsxr[[p]])>0){
				# Select only the points inside the 'xyzlim':
				inside = xyzlim[1,1]<plotpsxr[[p]] & xyzlim[2,1]>plotpsxr[[p]] & xyzlim[1,2]<plotpsyr[[p]] & xyzlim[2,2]>plotpsyr[[p]] & xyzlim[1,3]<plotpszr[[p]] & xyzlim[2,3]>plotpszr[[p]]
				plotpsxr[[p]] = plotpsxr[[p]][inside]
				plotpsyr[[p]] = plotpsyr[[p]][inside]
				plotpszr[[p]] = plotpszr[[p]][inside]
				calllist = c(list(x=plotpsxr[[p]],y=plotpsyr[[p]],z=plotpszr[[p]],xlim=xyzlim[,1],ylim=xyzlim[,2],zlim=xyzlim[,3],add=add | p!=1,axes=FALSE,box=FALSE,col=cols[p], size=size[p]), ll, list(xlab="",ylab="",zlab=""))
				}	
			else{
				calllist = c(list(x=NULL,y=NULL,z=NULL,xlim=xyzlim[,1],ylim=xyzlim[,2],zlim=xyzlim[,3],add=add | p!=1,axes=FALSE,box=FALSE,col=cols[p]), ll, list(xlab="",ylab="",zlab=""))
				}
			thisidx = do.call("plot3d",calllist[unique(names(calllist))])["data"]
			idx[p, seq_along(thisidx)] = thisidx
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
					calllist = c(list(x=data$psxf[[p]],y=data$psyf[[p]],z=data$pszf[[p]],add=add | p!=1 | sum(sapply(plotpsxr,length))!=0,axes=FALSE,box=FALSE,col=schoolcol[p],size=schoolsize), ll, list(xlab="",ylab="",zlab=""))
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
	########## End of if(plot) ##########
	
		
	##### Output #####
	# Add the subvolume used as a segmentation of the voxels:
	msvM = NULL
	if(isTRUE(plot.seg) || tolower(substr(plot.seg[1],1,2))=="ob"){
		if(length(dim(center))==2){
			for(i in seq_len(nrow(center))){
				inside  =  echoSegment.TSD(data,object=object,par=if(length(dim(par))==2) par[i,] else par,center=center[i,],angle=angle,plot=plot,seg.col=seg.col,alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
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
		inside = echoSegment.TSD(data,object=object,par=par,center=center,angle=angle,plot=FALSE,seg.col=seg.col,alpha=seg.alpha,subdivide=subdivide,excl.neg=excl.neg)
		}
		
	# Output:
	# The 'n' output only reflects the latest time step!
	#invisible(c(list(utim=data$utim, view = view, xyzlim=xyzlim, nlim=pos$nlim, fact=pos$fact), inside, list(msvM=msvM, finalN=finalN, finalacca=finalacca, n=pos$n, psxr=plotpsxr, psyr=plotpsyr, pszr=plotpszr, idx=if(plot) idx else NULL), data[intersect(vesselnames,names(data))]))
	if(isTRUE(ll$pos.out)){
		invisible(c(list(utim=data$utim, view = view, xyzlim=xyzlim, nlim=pos$nlim, fact=pos$fact), inside, list(msvM=msvM, finalN=finalN, finalacca=finalacca, n=pos$n, psxr=data$psxr, psyr=data$psyr, pszr=data$pszr, idx=if(plot) idx else NULL), data[intersect(vesselnames,names(data))]))
	}
	else{
		invisible(c(list(utim=data$utim, view = view, xyzlim=xyzlim, nlim=pos$nlim, fact=pos$fact), inside, list(msvM=msvM, finalN=finalN, finalacca=finalacca, n=pos$n, idx=if(plot) idx else NULL), data[intersect(vesselnames,names(data))]))
	}
	##################################################
	##################################################
}
