#*********************************************
#*********************************************
#' Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.
#'
#' @param imgout  specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param segout  specifies the directory in which to put the segmentation data files. If segout==FALSE, no image files are saved, if segout==TRUE or segout==NULL segmentation files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'segout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
#' @param names_img  is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
#' @param fmt  is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
#' @param info  is a string to be added to the frame and tsd names.
#' @param ndigits  is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
#' @param vessel.pos  is TRUE if every elapsed vessel position is to be plotted by a single dot on the surface.
#' @param vessel.pos.col  is the color of the vessel positions.
#' @param vessel.pos.size  is the size of the vessel positions.
#' @param force  is FALSE if the user should be aske for permission to overwrite files.
#' @param full.box  is FALSE (default) to make the bounding box in front of the data invisible, and TRUE ot make the full bounding box visible.
#' @param cex.3d  is the character expansion of the labels and axes in 3D.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname acplot3d.event
#'
acplot3d.event<-function(
	# (-16) Used in applot3d.event():
	imgout=NULL, segout=FALSE, names_img="frame", fmt="png", info="", ndigits=NULL,  vessel.pos=FALSE, vessel.pos.col="blue", vessel.pos.size=0.3, force=FALSE, cores=1, windowRect=c(0, 49, 1680, 1028), cex.3d=3, 
	# (-14) Used in read.event() and elsewhere in cplot3d.event():
	event=1, t=1, turns=10, cruise=2009116, TVG=TRUE, TVG.exp=2, dir.data=NULL, Paout=TRUE, exact=FALSE, bgns=TRUE, pdns=TRUE, nrns=TRUE, hins=TRUE, kern=NULL, segpar=NULL, pamkpar=list(), nsind=0.75, hins_add=10, pdns_scale=1e-14, TOV=0, cal=1, bmmd=NULL, 
	# (-13) Used in cplot3d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), clamp=c(0,1), shrink=TRUE, null.value=NA, 
	# (-12) Used in cplot3d.plot.color.bar():
	white=1, log=TRUE, color.bar="y--", color.bar.lwd=8, color.bar.adj=0, color.bar.tadj=0.1, color.bar.noWhite=TRUE, color.bar.nticks=8, color.bar.tickw=0.005, color.bar.tickcol="black", db=TRUE, voxels.out=FALSE, 
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
	# Start: 2009-09-02 - Clean version.
	# Update: 2010-02-19 - Changed to supporting the subset list 'subset', used in extract.
	# Update: 2010-03-02 - Added support for specifying the ranges of "x", "y" and "z" ('range').
	# Update: 2010-04-17 - Altered to letting the first and the last break move to infinity.
	# Update: 2011-01-06 - Added the option 'null.value'.
	# Update: 2011-01-17 - Added the option 'shrink'.
	# Update: 2011-01-17 - Simplified using the new cplot3d.TSD(), that was expanded to support multiple time steps in the input 'data'.
	# Update: 2011-10-02 - Changed to add axes and bounding box at the end of the function, and added the option 'beams' for use when plotting sonar grid og the ME70 echosounder. Also added plotting of sonar grid at the end of the funciton, using add.sonar.grid().
	# Update: 2012-03-15 - Added the options 'nticks' and 'edge' for customizing axes individually.
	# Update: 2012-06-28 - Added support for plotting 'pr0s', 'sgsc' and 'sgs0', specified by 'type'.
	# Update: 2012-11-15 - Added 'color.bar.noWhite'.
	# Update: 2013-01-05 - Added the option type="tlns".
	# Update: 2013-08-19 - Restructured function and categorized inputs.
	# Update: 2013-10-07 - Added the option of var[1]="-vbsc" indicating disabeling plotting of the acoustic data.
	# Last: 2013-10-07 - Merged cplot3d.event() and pplot3d.event().
	########### DESCRIPTION: ###########
	# Interactive scatterplot of the (3 dimensional) points either plotted by color and size or number of points in each voxel corresponding to the value of the points.
	########## DEPENDENCIES: ###########
	# add.sonar.grid(), strff(), echoIBM.segment.event(), read.event(), cpplot3d_type(),ones(), pplot3d.TSD(), cpplot3d.decorate(), cpplot3d.bottom.TSD()
	############ VARIABLES: ############
	
	#####################################
	##### Used in applot3d.event(): #####
	#####################################
	# ---imgout--- specifies the directory in which to put the image files of the 3D representation of the regenerated points (plotted using the package rgl). If imgout==FALSE, no image files are saved, if imgout==TRUE or imgout==NULL image files are stored in the default directory named "..../sv2pos/acca___/fmt", and if 'imgout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---segout--- specifies the directory in which to put the segmentation data files. If segout==FALSE, no image files are saved, if segout==TRUE or segout==NULL segmentation files are stored in the default directory named "..../sv2pos/acca___/tsd", and if 'segout' is a character string, a directory named accordingly is created and image files stored. If already existing, the used is asked to confirm overwriting the existing files.
	# ---names_img--- is a string representing the names of the png files (excluding numbering and the file extension ".png"). As an example names_img="frame" and 100<length(t)<1000 result in the names "frame001.png", "frame002.png", ... .
	# ---fmt--- is a string representing the format of the image files. Currently supported are "png", "ps", "eps", "tex", "pdf", "svg" and "pgf". If fmt==NULL no images will be saved, only plotted.
	# ---info--- is a string to be added to the frame and tsd names.
	# ---ndigits--- is the number of digits in the numbering indexes in the image names and .school file names. If ndigits=5 and 100<length(t)<1000 the resulting names will be "frame00001.png", "frame00002.png", ..., for the default values of 'fmt' and 'names_img'.
	# ---vessel.pos--- is TRUE if every elapsed vessel position is to be plotted by a single dot on the surface.
	# ---vessel.pos.col--- is the color of the vessel positions.
	# ---vessel.pos.size--- is the size of the vessel positions.
	# ---force--- is FALSE if the user should be aske for permission to overwrite files.
	#########################################################################
	##### See cpplot3d.event() for the remaining parameter descriptions #####
	#########################################################################
	
	
	##################################################
	##################################################
	acpplot3d.event(
		# (-16) Used in applot3d.event():
		imgout=imgout, segout=segout, names_img=names_img, fmt=fmt, info=info, ndigits=ndigits, vessel.pos=vessel.pos, vessel.pos.col=vessel.pos.col, vessel.pos.size=vessel.pos.size, force=force, cores=cores, windowRect=windowRect, cex.3d=cex.3d, 
		# Specifying whether to use cplot3d or pplot3d:
		cpplot3d_type="c", 
		# Used in read.event() and elsewhere in cplot3d.event():
		event=event, t=t, turns=turns, cruise=cruise, TVG=TVG, TVG.exp=TVG.exp, dir.data=dir.data, Paout=Paout, exact=exact, bgns=bgns, pdns=pdns, nrns=nrns, hins=hins, kern=kern, segpar=segpar, pamkpar=pamkpar, nsind=nsind, hins_add=hins_add, pdns_scale=pdns_scale, TOV=TOV, cal=cal, bmmd=bmmd, 
		# Used in cplot3d.TSD():
		breaks=breaks, col=col, colpar=colpar, clamp=clamp, shrink=shrink, null.value=null.value, 
		# Used in cplot3d.plot.color.bar():
		white=white, log=log, color.bar=color.bar, color.bar.lwd=color.bar.lwd, color.bar.adj=color.bar.adj, color.bar.tadj=color.bar.tadj, color.bar.noWhite=color.bar.noWhite, color.bar.nticks=color.bar.nticks, color.bar.tickw=color.bar.tickw, color.bar.tickcol=color.bar.tickcol, db=db, voxels.out=voxels.out, 
		# Used in pplot3d.TSD() and cplot3d.TSD():
		esnm=esnm, var=var, ind=ind, range=range, subset=subset, ideal=ideal, seabed=seabed, rot=rot, compensation=compensation, plot=plot, cs.xyzlim=cs.xyzlim, add=add, beamstypes=beamstypes, size=size, 
		# Used in cpplot3d.bottom.TSD():
		bottom=bottom, bottom.res=bottom.res, bottom.smooth=bottom.smooth, bottom.col=bottom.col, bottom.N=bottom.N, bottom.seg=bottom.seg, 
		# Used for plotting with plot3d():
		adds=adds, xlim=xlim, ylim=ylim, zlim=zlim, view=view, zoom=zoom, fov=fov, 
		# Used for plotting the school:
		school=school, schoolcol=schoolcol, schoolsize=schoolsize, schoolsample=schoolsample, schoollen=schoollen, schoollwd=schoollwd, schooltype=schooltype, schoolnumt=schoolnumt, schoolcrop=schoolcrop, 
		# Used for plotting the school (when schoolsample is a character = "obj") and the segmentation object:
		plot.seg=plot.seg, seg.col=seg.col, seg.alpha=seg.alpha, subdivide=subdivide, excl.neg=excl.neg, object=object, par=par, center=center, angle=angle, 
		# Used when plotting frame bounding box, aspect, titles, and axes, and else throughout cpplot3d.decorate():
		aspect=aspect, nticks=nticks, origin=origin, xlab=xlab, ylab=ylab, zlab=zlab, full.box=full.box, 
		# Used when plotting vessel position:
		edge.vpos=edge.vpos, line.vpos=line.vpos, at.vpos=at.vpos, cex.vpos=cex.vpos, col.vpos=col.vpos, 
		# Used when plotting date and time:
		clock=clock, cex.clock=cex.clock, adj.clock=adj.clock, format.clock=format.clock, digits.clock=digits.clock, lsp.clock=lsp.clock, col.clock=col.clock, 
		# Used when plotting the sonar grid:
		sonar.grid=sonar.grid, sonar.grid.col=sonar.grid.col, sonar.grid.lwd=sonar.grid.lwd, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, 
		# Used when plotting the global grid:
		global.grid=global.grid, global.grid.lwd=global.grid.lwd, global.grid.lty=global.grid.lty, 
		# Passed on to add.sonar.grid(), pplot3d.TSD, and decorate3d():
		...)
	##################################################
	##################################################
	}
