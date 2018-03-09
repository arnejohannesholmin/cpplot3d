#*********************************************
#*********************************************
#' Animates the data in the specified acoustic event as images of three dimensional point plot representations using the R package rgl.
#'
#' @param imgout  specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param segout  specifies the directory in which to put the segmentation data files. If segout==FALSE, no segmentation files are saved, if segout==TRUE or segout==NULL segmentation files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'segout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param names_img  is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
#' @param fmt  is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
#' @param info  is a string to be added to the frame and tsd names.
#' @param ndigits  is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
#' @param vessel.pos  is TRUE if every elapsed vessel position is to be plotted by a single dot on the surface.
#' @param vessel.pos.col  is the color of the vessel positions.
#' @param vessel.pos.size  is the size of the vessel positions.
#' @param force  is FALSE if the user should be aske for permission to overwrite files.
#' @param cores  is the number of cores used for parallel processing. If larger than 1, only image files are saved.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom echoIBM sonR_implemented_ind
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom rgl open3d par3d points3d rgl.dev.list rgl.postscript rgl.snapshot
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR read.event
#' @importFrom TSD ang2rot labl.TSD strff write.TSD
#' @importFrom utils tail
#'
#' @export
#' @rdname acpplot3d.event
#'
acpplot3d.event <- function(
	# (-16) Used in applot3d.event():
	imgout=NULL, segout=FALSE, names_img="frame", fmt="png", info="", ndigits=NULL,  vessel.pos=FALSE, vessel.pos.col="blue", vessel.pos.size=0.3, force=FALSE, cores=1, windowRect=c(0, 49, 1680, 1028), seq_names=FALSE, cex.3d=3, 
	# (-15) Specifying whether to use cplot3d or pplot3d:
	cpplot3d_type, 
	# (-14) Used in read.event() and elsewhere in cpplot3d.event():
	event=1, t=1, turns=10, cruise=2009116, TVG=TRUE, TVG.exp=2, dir.data=NULL, Paout=TRUE, exact=FALSE, bgns=TRUE, pdns=TRUE, nrns=TRUE, hins=TRUE, kern=NULL, segpar=NULL, pamkpar=list(), nsind=0.75, hins_add=10, pdns_scale=1e-14, TOV=0, cal=1, bmmd=NULL, 
	# (-13) Used in cplot3d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), clamp=c(0,1), shrink=TRUE, null.value=NA, 
	# (-12) Used in cplot3d.plot.color.bar():
	white=1, log=TRUE, color.bar="y--", color.bar.lwd=8, color.bar.adj=0, color.bar.tadj=0.1, color.bar.noWhite=TRUE, color.bar.nticks=8, color.bar.tickw=0.005, color.bar.tickcol="black", db=TRUE, voxels.out=FALSE, 
	# (-11) Used in pplot3d_sv2pos.TSD() and elsewhere in pplot3d.event():
	N=1e5, acca=NULL, fun="mod", allert=1e8, nlim=NULL, fact=NULL, cols=c("black","navyblue","magenta4","red3","darkorange2","yellow"), stretch=1, scale.voxels=1, rand.gen=c("unif","beta","norm"), possample=1, 
	# (-10) Used in pplot3d.TSD() and cplot3d.TSD():
	esnm="MS70", var="vbsc", ind=list(-(1:150),NULL), range=list(), subset=NULL, ideal=TRUE, seabed=-12000, rot=2, compensation=c("pitch","roll"), plot=TRUE, cs.xyzlim="g", add=FALSE, beamstypes=1, size=cplot3d.size("pow",y=20,par=4), 
	# (-9) Used in cpplot3d.bottom.TSD():
	bottom=FALSE, bottom.res=5, bottom.smooth=NULL, bottom.col="topo.col", bottom.N=1e5, bottom.seg=list(sgth=0.2, misM=1e-3, bwGp=2, turns=100), 
	# (-8) Used for plotting with plot3d():
	adds=NULL, xlim=NULL, ylim=NULL, zlim=NULL, view=c("free","top","bottom","south","west","north","east"), zoom=0.7, fov=60, 
	# (-7) Used for plotting the school:
	school=FALSE, schoolcol="purple", schoolsize=0.3, schoolsample=0.01, schoollen=4, schoollwd=1, schooltype="p", schoolnumt=NULL, schoolcrop=FALSE, 
	# (-6) Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
	plot.seg=FALSE, seg.col="green", seg.alpha=0.2, subdivide=3, excl.neg=TRUE, object=c("ellipsoid","cuboid"), par=double(3), center=c(0,0,0), angle=0, 
	# (-5) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate():
	aspect="iso", nticks=5, origin=1, xlab="x", ylab="y", zlab="z", full.box=FALSE, 
	# (-4) Used when plotting vessel position:
	edge.vpos="", line.vpos=0, at.vpos=NULL, cex.vpos=1, col.vpos="blue", 
	# (-3) Used when plotting date and time:
	clock="bbl", cex.clock=1, adj.clock=c(0,0), format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, lsp.clock=0.04, col.clock=4, 
	# (-2) Used when plotting the sonar grid:
	sonar.grid="frame", sonar.grid.col=c("orange","cornflowerblue"), sonar.grid.lwd=1, cs.pos="g", cs.view="g", sides=c("tb","tb"), dens=c(200,100,100,1), every=Inf, 
	# (-1) Used when plotting the global grid:
	global.grid=FALSE, global.grid.lwd=0.5, global.grid.lty=1, 
	# (0) Passed on to add.sonar.grid(), pplot3d.TSD, and decorate3d():
	...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-05-08 - Clean version.
	# Update: 2010-07-06 - Canged to be more dynamic when it comes to writing images from already regenerated fish positions, intruducing the opotions 'posout' and 'imgout'.
	# Update: 2010-10-22 - Added the option of adding a string to the names of the directory and files to be written, using 'info'.
	# Update: 2010-10-29 - Added lots of options for plotting of sonar grid, global grid and vessel position, and for moving the plotting fram with the vessel (useful for large schools).
	# Update: 2010-11-15 - Added the option 'indt'.
	# Update: 2010-11-25 - Three major changes: (1) Added the option of plotting a grid frame around the sonar volume, in addition to the existing option of plotting a projected grid on the top of the plotting frame. (2) Replaced 'cs' by 'cs.pos', 'cs.xyzlim' and 'cs.view', specifying the coordinate system to use when generating points, setting the plotting frame and setting the view point. (3) Added the option of specifying azimuth angle (theta) and elevation angle (phi) of the view point. Also added the option view="free" for continusly interactive adjustment of the plotting frame.
	# Update: 2010-12-08 - Added the option of regenerating points for larger objects in voxels where n>nlim, using the value of 'fact' as the factor to scale the number of points by (fact=0.01 corresponding to 100 times larger objects in the dense voxels). Also the color of these points are set by 'bigcol'.
	# Update: 2011-01-15 - Moved the function plot_volume() to a separate file.
	# Update: 2011-02-11 - Removed the option 'indt'.
	# Update: 2011-05-14 - Added options 'plot.seg', 'segout', 'object', 'par', 'center', 'angle', 'seg.col' and 'seg.alpha'. These parameters provides segmentation of the school by the volume object specified, and calculates the total echo and plots the segmentation object.
	# Update: 2011-05-15 - Added the option 'plot', which supresses plotting of any kind if FALSE.
	# Update: 2011-05-15 - Fixed bugs related to the coodinate system specified.
	# Update: 2012-03-15 - Added the options 'nticks' and 'edge' for customizing axes individually.
	# Update: 2012-05-16 - Changed from using 'bigcol' for the color of the points representing many fish, to 'cols', which is a two element color vector holding both the color of the normal points and the big points.
	# Update: 2012-06-28 - Added 'N' which specifies the number of points to draw. Different from pplot3d.event() and pplot3d.TSD(), which use 'N' as default instead of 'acca', it is not desirable to have the same number of points when animating. If 'N' should be used, set acca=NULL.
	# Update: 2013-01-05 - Added the option var="tlns".
	# Update: 2013-04-08 - Removed writing position data and updated to support multiple levels of the points.
	# Update: 2013-05-14 - Removed the option 't1'.
	# Update: 2013-07-16 - Restructured function and categorized inputs.
	# Update: 2013-08-19 - Further restructuring.
	# Update: 2013-08-19 - Added 'cores' and 'cal'.
	# Last: 2015-05-23 - Added support for multiple plotting types given by 'var' (e.g., "vbsc" and "sgsc").
	########### DESCRIPTION: ###########
	# Animates the data in the specified acoustic event as images of three dimensional point plot representations using the R package rgl.
	########## DEPENDENCIES: ###########
	# read.event(), pplot3d.event(), ang2rot(), write.TSD()
	############ VARIABLES: ############
	
	#####################################
	##### Used in acpplot3d.event(): #####
	#####################################
	# ---imgout--- specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---segout--- specifies the directory in which to put the segmentation data files. If segout==FALSE, no segmentation files are saved, if segout==TRUE or segout==NULL segmentation files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'segout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---names_img--- is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
	# ---fmt--- is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
	# ---info--- is a string to be added to the frame and tsd names.
	# ---ndigits--- is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
	# ---vessel.pos--- is TRUE if every elapsed vessel position is to be plotted by a single dot on the surface.
	# ---vessel.pos.col--- is the color of the vessel positions.
	# ---vessel.pos.size--- is the size of the vessel positions.
	# ---force--- is FALSE if the user should be aske for permission to overwrite files.
	# ---cores--- is the number of cores used for parallel processing. If larger than 1, only image files are saved.
	#########################################################################
	##### See cpplot3d.event() for the remaining parameter descriptions #####
	#########################################################################
	
	##################################################
	##################################################
	##### Preparation #####
	ind = sonR_implemented_ind(ind)
	# If 'segpar' is empty, set it to 1:
	if(length(segpar)==0 && var[1] %in% c("sgsc","sgs0")){
		warning("'segpar' not given, and was defaulted to 1")
		segpar=1
	}
	# Get all the time points of the event and pick out the time points selected by fractional t:
	if(length(t)==1 && t<1){
		time = read.event(cruise=cruise, event=event, esnm=esnm, t="all", var=c("time", "bmmd"), merge=TRUE)
		if(length(bmmd)>0 && length(time$bmmd)>0){
			time = time$indt[time$bmmd %in% bmmd]
			t = time[which(c(1, diff(floor(time*t)))==1)]
		}
		else{
			t = time$indt[c(1, diff(floor(time$indt*t)))]
		}
	}
	# Else pick out the time points specified by t:
	else{
		time = read.event(cruise=cruise, event=event, esnm=esnm, t=t, var=c("time", "bmmd"), merge=TRUE)
		if(length(bmmd)>0 && length(time$bmmd)>0){
			t = time$indt[time$bmmd %in% bmmd]
		}
	}
	
	time=read.event(cruise=cruise, event=event, esnm=esnm, t=t, var="time", merge=TRUE)
	
	# Error if time points in 't' were not found in the TSD files:
	if(length(time$indt)<length(t) && t!="all"){
		warning(paste("The following time steps are not present in event ",event,"\n:",paste(setdiff(t,time$indt), collapse=", "),sep=""))
	}
	# Keep integer:
	if(is.integer(t)){
		t = as.integer(time$indt)
	}
	else{
		t = time$indt
	}
	t = locateNonVerticalBeamMode(t, event)
	if(seq_names==TRUE){
		t_dirname = seq_along(t)
	}
	else{
		t_dirname = t
	}
	
	# Initial settings:
	if(identical(segout,FALSE)){
		plot.seg=FALSE
	}
	if(!plot){
		plot.seg=FALSE
		imgout=FALSE
	}
	# Assure correct dimension of the parameters:
	if(length(dim(par))<2){
		par=matrix(par,nrow=length(t),ncol=3,byrow=TRUE)
	}
	if(length(dim(center))<2){
		center=matrix(center,nrow=length(t),ncol=3,byrow=TRUE)
	}
	if(length(dim(angle))<length(t)){
		angle=rep(angle,length.out=length(t))
	}
	# Error if the event is not found:
	if(length(event)==0){
		stop("'event' and 'cruise' need to be given")
	}
	ll=list(...)
	# Read the vessel positions to add to the plots:
	if(vessel.pos || any(is.na(xlim), is.na(ylim))){
		vessel.pos = read.event(cruise=cruise, event=event, esnm=esnm, t=t, var=c("psxv","psyv"), adds=adds, cs=cs.pos, origin=origin, dir.data=dir.data, TOV=TOV)
		if(is.na(xlim) || is.na(ylim)){
			xlim = read.event(cruise=cruise, event=event, esnm=esnm, var=c("lenb", "asps", "sint"))
			xlim = soundbeam_range(xlim, pos="max")
			ylim = c(-xlim, xlim) + range(vessel.pos$psyv)
			xlim = c(-xlim, xlim) + range(vessel.pos$psxv)
			range$psxx = xlim
			range$psyx = ylim
			zlim = range$pszx
		}
	}
	# Add info in the file and directory names if specified:
	if(nchar(info)>0){
		if(substr(info,1,1)!="_"){
			info=paste("_",info,sep="")
		}
		if(substr(info,nchar(info),nchar(info))=="_"){
			info=substr(info,1,nchar(info)-1)
		}
	}
	
	# 'imgout', the directory to which the images of regenerated points are written:
	if(!identical(imgout,FALSE)){
		if(is.null(imgout) || isTRUE(imgout)){
			if(strff("p",cpplot3d_type)){
				imgout = file.path(dirname(read.event(cruise=cruise, event=event, esnm=esnm, dir.out=TRUE)), "sv2pos", paste0("acca_", format(acca,scientific=TRUE), "_indt_", t_dirname[1], "__", tail(t_dirname,1), info), fmt)
			}
			else{
				imgout = file.path(dirname(read.event(cruise=cruise, event=event, esnm=esnm, dir.out=TRUE)), "cplot3d", paste0("br", if(length(breaks)==1) breaks else length(breaks), "wh", white, "_indt_", t_dirname[1] ,"__", tail(t_dirname,1), info), fmt)
			}
		}
		if(!file.exists(as.character(imgout))){
			suppressWarnings(dir.create(imgout,recursive=TRUE))
		}
		else if(any(basename(list.files(imgout)) %in% basename(names_img)) && !force){
			answer=readline(paste("Previously generated images located in \"",imgout,"\". Overwrite (y/n) \n\n",sep=""))
			if(answer!="y"){
				imgout=FALSE
			}
		}
	}
	
	# Create the names of the frames:
	if(is.null(ndigits) || ndigits==0 || ndigits<nchar(max(t_dirname))){
		ndigits=nchar(max(t_dirname))
	}
	tchar=sprintf(paste("%0",ndigits,"d",sep=""), t_dirname)
	names_img=paste(imgout,"/",names_img,info,"_",tchar,".",fmt,sep="")
	
	
	##### Execution #####
	# Create av funciton that plots one ping:
	cpplot3d.event_oneping = function(thistind, t, cpplot3d_type, event, cruise, TVG, TVG.exp, dir.data, Paout, exact, bgns, pdns, nrns, hins, kern, segpar, pamkpar, nsind, hins_add, pdns_scale, TOV, cal, bmmd, breaks, col, colpar, clamp, shrink, null.value, white, log, color.bar, color.bar.lwd, color.bar.adj, color.bar.tadj, color.bar.noWhite, color.bar.nticks, color.bar.tickw, color.bar.tickcol, db, voxels.out, N, acca, fun, allert, nlim, fact, cols, stretch, scale.voxels, rand.gen, possample, esnm, var, ind, range, subset, ideal, seabed, rot, compensation, plot, cs.xyzlim, add, beamstypes, size, bottom, bottom.res, bottom.smooth, bottom.col, bottom.N, bottom.seg, adds, xlim, ylim, zlim, view, zoom, fov, school, schoolcol, schoolsize, schoolsample, schoollen, schoollwd, schooltype, schoolnumt, schoolcrop, plot.seg, seg.col, seg.alpha, subdivide, excl.neg, object, par, center, angle, aspect, nticks, origin, xlab, ylab, zlab, edge.vpos, line.vpos, at.vpos, cex.vpos, col.vpos, clock, cex.clock, adj.clock, format.clock, digits.clock, lsp.clock, col.clock, sonar.grid, sonar.grid.col, sonar.grid.lwd, cs.pos, cs.view, sides, dens, every, global.grid, global.grid.lwd, global.grid.lty, vessel.pos, vessel.pos.col, vessel.pos.size, imgout, fmt, names_img, windowRect, ...){
		#library(rgl)
		if(length(rgl.dev.list())==0){
			rgl::open3d()
			rgl::par3d(cex=cex.3d, windowRect=windowRect)
		}
		else{
			rgl::rgl.clear()
		}
		
		# Apply cpplot3d.event():
		for(i in seq_along(var)){
			thisout <- cpplot3d::cpplot3d.event(
				# (-15) Specifying whether to use cplot3d or pplot3d:
				if(i==1) cpplot3d_type else setdiff(c("c","p"),cpplot3d_type)[1], 
				# (-14) Used in read.event() and elsewhere in cpplot3d.event():
				event=event, t=t[thistind], turns=1, cruise=cruise, TVG=TVG, TVG.exp=TVG.exp, dir.data=dir.data, Paout=Paout, exact=exact, bgns=bgns, pdns=pdns, nrns=nrns, hins=hins, kern=kern, segpar=segpar, pamkpar=pamkpar, nsind=nsind, hins_add=hins_add, pdns_scale=pdns_scale, TOV=TOV, cal=cal, bmmd=bmmd, 
				# (-13) Used in cplot3d.TSD():
				breaks=breaks, col=col, colpar=colpar, clamp=clamp, shrink=shrink, null.value=null.value, 
				# (-12) Used in cplot3d.plot.color.bar():
				white=white, log=log, color.bar=color.bar, color.bar.lwd=color.bar.lwd, color.bar.adj=color.bar.adj, color.bar.tadj=color.bar.tadj, color.bar.noWhite=color.bar.noWhite, color.bar.nticks=color.bar.nticks, color.bar.tickw=color.bar.tickw, color.bar.tickcol=color.bar.tickcol, db=db, voxels.out=voxels.out, 
				# (-11) Used in pplot3d_sv2pos.TSD() and elsewhere in pplot3d.event():
				N=N, acca=acca, fun=fun, allert=allert, nlim=nlim, fact=fact, cols=cols, stretch=stretch, scale.voxels=scale.voxels, rand.gen=rand.gen, possample=possample, 
				# (-10) Used in pplot3d.TSD() and cplot3d.TSD():
				esnm=esnm, var=var[i], ind=ind, range=range, subset=subset, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, plot=plot, cs.xyzlim=cs.xyzlim, add=if(i==1) add else TRUE, beamstypes=beamstypes, size=if(i==1) size else 0.3, 
				# (-9) Used in cpplot3d.bottom.TSD():
				bottom=bottom, bottom.res=bottom.res, bottom.smooth=bottom.smooth, bottom.col=bottom.col, bottom.N=bottom.N, bottom.seg=bottom.seg, 
				# (-8) Used for plotting with plot3d():
				adds=adds, xlim=xlim, ylim=ylim, zlim=zlim, view=if(is.function(view)) view(t[thistind]) else view, zoom=zoom, fov=fov, 
				# (-7) Used for plotting the school:
				school=school, schoolcol=schoolcol, schoolsize=schoolsize, schoolsample=schoolsample, schoollen=schoollen, schoollwd=schoollwd, schooltype=schooltype, schoolnumt=schoolnumt, schoolcrop=schoolcrop, 
				# (-6) Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
				plot.seg=plot.seg, seg.col=seg.col, seg.alpha=seg.alpha, subdivide=subdivide, excl.neg=excl.neg, object=object, par=par[thistind,], center=center[thistind,], angle=angle[thistind], 
				# (-5) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate():
				aspect=aspect, nticks=nticks, origin=origin, xlab=xlab, ylab=ylab, zlab=zlab, full.box=full.box, 
				# (-4) Used when plotting vessel position:
				edge.vpos=edge.vpos, line.vpos=line.vpos, at.vpos=at.vpos, cex.vpos=cex.vpos, col.vpos=col.vpos, 
				# (-3) Used when plotting date and time:
				clock=clock, cex.clock=cex.clock, adj.clock=adj.clock, format.clock=format.clock, digits.clock=digits.clock, lsp.clock=lsp.clock, col.clock=col.clock, 
				# (-2) Used when plotting the sonar grid:
				sonar.grid=sonar.grid, sonar.grid.col=sonar.grid.col, sonar.grid.lwd=sonar.grid.lwd, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, 
				# (-1) Used when plotting the global grid:
				global.grid=global.grid, global.grid.lwd=global.grid.lwd, global.grid.lty=global.grid.lty, 
				# (0) Passed on to add.sonar.grid(), pplot3d.TSD, and decorate3d():
				top=FALSE, ...)
		}
			
		# Run optional time step dependent function addfun():
		if(is.function(list(...)[["addfun"]])){
			do.call(list(...)[["addfun"]], list(t[thistind], ...))
			#list(...)[["addfun"]](thistind, ...)
		}
		
		# Add vessel position:
		if(plot){
			if( length(vessel.pos)>0 && !identical(vessel.pos,FALSE)){
				# If the coordinate system of the vessel is used for generating the points, the vessel positions are relative to the present vessel position:
				par3d_bbox = rgl::par3d()$bbox
				if(identical(cs.pos,"v")){
					thisvessel.pos = cbind(vessel.pos$psxv[seq_len(thistind)]-vessel.pos$psxv[thistind], vessel.pos$psyv[seq_len(thistind)]-vessel.pos$psyv[thistind], par3d_bbox[6])
				}
				else{
					thisvessel.pos = cbind(vessel.pos$psxv[seq_len(thistind)], vessel.pos$psyv[seq_len(thistind)], par3d_bbox[6])
				}
				valid.vessel.pos = (par3d_bbox[1]<=thisvessel.pos[,1] & thisvessel.pos[,1] <= par3d_bbox[2]) & (par3d_bbox[3]<=thisvessel.pos[,2] & thisvessel.pos[,2] <= par3d_bbox[4])
				rgl::points3d(thisvessel.pos[valid.vessel.pos,,drop=FALSE], col=vessel.pos.col, size=vessel.pos.size)
			}
			# Save the plot to file:
			if(!identical(imgout,FALSE)){
				if(length(fmt)>0){
					if(fmt=="png"){
						rgl::rgl.snapshot(names_img[thistind],top=FALSE)
					}
					else{
						rgl::rgl.postscript(names_img[thistind],fmt=fmt)
					}
				}
			}
			cat("Frame ",t[thistind]," (",thistind,"/",length(t),")\n",sep="")
		}
		thisout
	}
		
	

	# Parallel processing using the pblapply() function in the pbapply package:
	parallel <- cores > 1
	##### NOTE THAT WE NEEDED TO MAKE THE CLUSTER OUTSIDE OF pblapply() TO AVOID FORKING ERROR. #####
	if(parallel){
		# Detect the number of cores and use the minimum of this and the number of requested cores:	
		cores = min(cores, length(t), detectCores())
		cat("Parallel processing on", cores, "cores:\n")
		cores <- makeCluster(cores)
	}
	# Progress bar parallel processing (if cores>1):	
	out <- pblapply(seq_along(t), cpplot3d.event_oneping, 
		t=t, 
		cpplot3d_type=cpplot3d_type, 
		event=event, 
		cruise=cruise, 
		TVG=TVG, 
		TVG.exp=TVG.exp, 
		dir.data=dir.data, 
		Paout=Paout, 
		exact=exact, 
		bgns=bgns, 
		pdns=pdns, 
		nrns=nrns, 
		hins=hins, 
		kern=kern, 
		segpar=segpar, 
		pamkpar=pamkpar, 
		nsind=nsind, 
		hins_add=hins_add, 
		pdns_scale=pdns_scale, 
		TOV=TOV, 
		cal=cal, 
		bmmd=bmmd, 
		breaks=breaks, 
		col=col, 
		colpar=colpar, 
		clamp=clamp, 
		shrink=shrink, 
		null.value=null.value, 
		white=white, 
		log=log, 
		color.bar=color.bar, 
		color.bar.lwd=color.bar.lwd, 
		color.bar.adj=color.bar.adj, 
		color.bar.tadj=color.bar.tadj, 
		color.bar.noWhite=color.bar.noWhite, 
		color.bar.nticks=color.bar.nticks, 
		color.bar.tickw=color.bar.tickw, 
		color.bar.tickcol=color.bar.tickcol, 
		db=db, 
		voxels.out=voxels.out, 
		N=N, 
		acca=acca, 
		fun=fun, 
		allert=allert, 
		nlim=nlim, 
		fact=fact, 
		cols=cols, 
		stretch=stretch, 
		scale.voxels=scale.voxels, 
		rand.gen=rand.gen, 
		possample=possample, 
		esnm=esnm, 
		var=var, 
		ind=ind, 
		range=range, 
		subset=subset, 
		ideal=ideal, 
		seabed=seabed, 
		rot=rot, 
		compensation=compensation, 
		plot=plot, 
		cs.xyzlim=cs.xyzlim, 
		add=add, 
		beamstypes=beamstypes, 
		size=size, 
		bottom=bottom, 
		bottom.res=bottom.res, 
		bottom.smooth=bottom.smooth, 
		bottom.col=bottom.col, 
		bottom.N=bottom.N, 
		bottom.seg=bottom.seg, 
		adds=adds, 
		xlim=xlim, 
		ylim=ylim, 
		zlim=zlim, 
		view=view, 
		zoom=zoom, 
		fov=fov, 
		school=school, 
		schoolcol=schoolcol, 
		schoolsize=schoolsize, 
		schoolsample=schoolsample, 
		schoollen=schoollen, 
		schoollwd=schoollwd, 
		schooltype=schooltype, 
		schoolnumt=schoolnumt, 
		schoolcrop=schoolcrop, 
		plot.seg=plot.seg, 
		seg.col=seg.col, 
		seg.alpha=seg.alpha, 
		subdivide=subdivide, 
		excl.neg=excl.neg, 
		object=object, 
		par=par, 
		center=center, 
		angle=angle, 
		aspect=aspect, 
		nticks=nticks, 
		origin=origin, 
		xlab=xlab, 
		ylab=ylab, 
		zlab=zlab, 
		edge.vpos=edge.vpos, 
		line.vpos=line.vpos, 
		at.vpos=at.vpos, 
		cex.vpos=cex.vpos, 
		col.vpos=col.vpos, 
		clock=clock, 
		cex.clock=cex.clock, 
		adj.clock=adj.clock, 
		format.clock=format.clock, 
		digits.clock=digits.clock, 
		lsp.clock=lsp.clock, 
		col.clock=col.clock, 
		sonar.grid=sonar.grid, 
		sonar.grid.col=sonar.grid.col, 
		sonar.grid.lwd=sonar.grid.lwd, 
		cs.pos=cs.pos, 
		cs.view=cs.view, 
		sides=sides, 
		dens=dens, 
		every=every, 
		global.grid=global.grid, 
		global.grid.lwd=global.grid.lwd, 
		global.grid.lty=global.grid.lty, 
		vessel.pos=vessel.pos, 
		vessel.pos.col=vessel.pos.col, 
		vessel.pos.size=vessel.pos.size, 
		imgout=imgout, 
		fmt=fmt, 
		names_img=names_img, 
		windowRect=windowRect, 
		..., 
		cl=cores)
		
	#out <- pblapply(seq_along(t), cpplot3d.event_oneping, t, cpplot3d_type, event, cruise, TVG, TVG.exp, dir.data, Paout, exact, bgns, pdns, nrns, hins, kern, segpar, pamkpar, nsind, hins_add, pdns_scale, TOV, cal, bmmd, breaks, col, colpar, clamp, shrink, null.value, white, log, color.bar, color.bar.lwd, color.bar.adj, color.bar.tadj, color.bar.noWhite, color.bar.nticks, color.bar.tickw, color.bar.tickcol, db, voxels.out, N, acca, fun, allert, nlim, fact, cols, stretch, scale.voxels, rand.gen, possample, esnm, var, ind, range, subset, ideal, seabed, rot, compensation, plot, cs.xyzlim, add, beamstypes, size, bottom, bottom.res, bottom.smooth, bottom.col, bottom.N, bottom.seg, adds, xlim, ylim, zlim, view, zoom, fov, school, schoolcol, schoolsize, schoolsample, schoollen, schoollwd, schooltype, schoolnumt, schoolcrop, plot.seg, seg.col, seg.alpha, subdivide, excl.neg, object, par, center, angle, aspect, nticks, origin, xlab, ylab, zlab, edge.vpos, line.vpos, at.vpos, cex.vpos, col.vpos, clock, cex.clock, adj.clock, format.clock, digits.clock, lsp.clock, col.clock, sonar.grid, sonar.grid.col, sonar.grid.lwd, cs.pos, cs.view, sides, dens, every, global.grid, global.grid.lwd, global.grid.lty, vessel.pos, vessel.pos.col, vessel.pos.size, imgout, fmt, names_img, windowRect, ..., cl=cores)	
	if(parallel){
		stopCluster(cores)
	}
	out=lapply(out,drop)
	
	
	##### Output #####
	# Write segmentation data to file:
	if(!identical(segout,FALSE) && parallel){
		# Transform the angles of incidence to the centers of mass of the school (calculated from the voxels inside the segmentation object) from angles to rotations:
		out$aniS=ang2rot(out$aniS)
		out$anio=ang2rot(out$anio)
		
		# Define the names of the file to which the data are written:
		if(!is.null(segout)){
			# Create the directory of the segmentation data if not existing
			if(strff("p",cpplot3d_type)){
				segoutdir = file.path(dirname(read.event(cruise=cruise, event=event, esnm=esnm, dir.out=TRUE)), "sv2pos", paste("acca_",format(acca,scientific=TRUE),"_indt_",t_dirname[1],"__",tail(t_dirname,1),info,sep=""), "tsd")
			}
			else{
				segoutdir = file.path(dirname(read.event(cruise=cruise, event=event, esnm=esnm, dir.out=TRUE)), "cplot3d", paste("br",if(length(breaks)==1) breaks else length(breaks),"wh",white,"_indt_",t_dirname[1],"__",tail(t_dirname,1),info,sep=""), "tsd")
			}
			# Create the directory if non-existent:
			if(!file.exists(as.character(segoutdir))){
				suppressWarnings(dir.create(segoutdir,recursive=TRUE))
			}
			
			# Define the name of the output file for the segmentation data:
			if(isTRUE(segout)){
				if(strff("p",cpplot3d_type)){
					segout = file.path(segoutdir, paste("acca_",format(acca,scientific=TRUE),"_indt_",t_dirname[1],"__",tail(t_dirname,1),info,".tsd",sep=""))
				}
				else{
					segout = file.path(segoutdir, paste("br",if(length(breaks)==1) breaks else length(breaks),"wh",white,"_indt_",t_dirname[1],"__",tail(t_dirname,1),info,".tsd",sep=""))
				}
			}
			
			# Write the data:
			write.TSD(x=out[labl.TSD(c("applotoutput", "v"), list.out=FALSE)], con=segout, keep.null=TRUE, append=FALSE, numt=length(t))
		}
		invisible(out)
	}
	else{
		invisible(out)
	}
	##################################################
	##################################################
}
