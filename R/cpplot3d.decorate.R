#*********************************************
#*********************************************
#' Plots bounding box, titles, axes, vessel positions, date and time, sonar grid, and global grid to pplot.TSD()/pplot3d.event or cplot.TSD()/cplot3d.event.
#'
#' @param data  is a list containing "utim", "dira", "dire", "asps", "sint", "lenb", "psxv", "psyv", "pszv", "psze", and "rtzv".
#' @param add  is TRUE if points are to be added to an existing plot.
#' @param aspect  is used to set the dimension of the plotting frame (see aspect3d()).
#' @param xyzlim  is a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values.
#' @param nticks  is either a single integer specifying the suggested number of ticks used on the axes, or a vector of three elements giving the number of ticks on each axis, in which case the axes will NOT move so that they may obscure the data.
#' @param origin  is (1) a vector of two elements representing the origin of the global coordinate system (G), (2) the numbering index of the ping in the total sequence of pings of the event, which is to be regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea), or (3) NULL, implying that the origin be put to the mid point of the vessel posistions.
#' @param xlab  is the label for the x axis.
#' @param ylab  is the label for the y axis.
#' @param zlab  is the label for the z axis.
#' @param full.box  is FALSE (default) to make the bounding box in front of the data invisible, and TRUE ot make the full bounding box visible.
#' @param edge.vpos  defines the placement of the vessel position, where edge.vpos="x-+" places the vessel position information on the upper south edge of the plot.
#' @param line.vpos  adjusts the position of the text by the given number of lines
#' @param at.vpos  adjusts the position left/right (see mtext3d()).
#' @param cex.vpos  is the character expansion of the vessel information.
#' @param col.vpos   is the color for the vessel position information.
#' @param clock  is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' @param cex.clock  is the numeric character expansion value for the clock.
#' @param adj.clock  is used in text3d() when plotting the date and time.
#' @param format.clock  is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\\n" apparing in 'format.clock' cause a line break.
#' @param digits.clock  is a numeric specifying the number of digits of the seconds part of the time.
#' @param lsp.clock  is the spacing factor for the line break, given as a multiple of the corresponding x-, y- or zlim. Default is 0.04.
#' @param col.clock  is the color of the plotted time and date.
#' @param sonar.grid  is a string representing whether to plot the sonar grid as a projection onto the surface ("proj"), as a grid around the sonar volume ("frame") or no grid ("").
#' @param sonar.grid.col  is a vector of two strings specifying the color to use for plotting the sonar grid edges, where the first is the main color, and the second is the color to use for lines that are above the plotting frame, but that are plotted onto the top of the frame.
#' @param sonar.grid.lwd  is the line width of the sonar grid.
#' @param cs.pos  defines the coordinate system to use when generating the points. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel. If given as a three element vector, 'cs.pos' is interpreted as c(cs.pos,cs.view,cs.xyzlim).
#' @param cs.view  defines the coordinate system to use when specifying the view point. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param sides  is a vector of two strings specifying on which sides of the sonar volume to plot edges of the beams and bows, respectively, where "t" is "top", "r" is "right", "b" is "bottom" and "l" is "left".
#' @param dens  is a vector of 4 elements specifying 
#' @param every  (used in plot_volume.ME70()) is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers givint the time steps for which the frame should be plotted (not including the first and last time step).
#' @param global.grid  is the spacing between the gridlines of the global grid, plotted if 'global.grid' is not FALSE or empty. If global.grid==TRUE, the grid width is set to 100.
#' @param global.grid.lwd  is the line width of the global grid lines.
#' @param global.grid.lty  is the line type of the global grid lines.
#' @param ...  are inputs passed on to add.sonar.grid().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl grid3d mtext3d decorate3d axes3d title3d box3d
#' @importFrom TSD DD2DMS
#'
#' @export
#' @rdname cpplot3d.decorate
#'
cpplot3d.decorate<-function(
	# (1) Used in decorate_pplot_cplot():
	data=NULL, plot=TRUE, 
	# (2) Used when plotting frame bounding box, aspect, titles, and axes, and else throughout decorate_pplot_cplot():
	aspect="iso", xyzlim=NULL, nticks=5, origin=1, xlab="x", ylab="y", zlab="z", full.box=FALSE, 
	# (3) Used when plotting vessel position:
	edge.vpos="", line.vpos=0, at.vpos=NULL, cex.vpos=1, col.vpos="blue", 
	# (4) Used when plotting date and time:
	clock="bbl", cex.clock=1, adj.clock=c(0,0), format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, lsp.clock=0.04, col.clock=4, 
	# (5) Used when plotting the sonar grid:
	sonar.grid="frame", sonar.grid.col=c("orange","cornflowerblue"), sonar.grid.lwd=1, cs.pos="g", cs.view="g", sides=c("tb","tb"), dens=c(200,100,100,1), every=Inf, 
	# (6) Used when plotting the global grid:
	global.grid=FALSE, global.grid.lwd=0.5, global.grid.lty=1, 
	# Passed on to add.sonar.grid():
	...){ 
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-08-15 - Clean version.
	# Last: 2014-05-27 - Updated to include 'indt'.
	########### DESCRIPTION: ###########
	# Plots bounding box, titles, axes, vessel positions, date and time, sonar grid, and global grid to pplot.TSD()/pplot3d.event or cplot.TSD()/cplot3d.event.
	########## DEPENDENCIES: ###########
	# add.sonar.grid()
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	##### (1) Used in decorate_pplot_cplot(): #####
	# ---data--- is a list containing "utim", "dira", "dire", "asps", "sint", "lenb", "psxv", "psyv", "pszv", "psze", and "rtzv".
	# ---add--- is TRUE if points are to be added to an existing plot.
	##### (2) Used when plotting frame bounding box, aspect, titles, and axes, and elsewhere throughout decorate_pplot_cplot(): #####
	# ---aspect--- is used to set the dimension of the plotting frame (see aspect3d()).
	# ---xyzlim--- is a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values.
	# ---nticks--- is either a single integer specifying the suggested number of ticks used on the axes, or a vector of three elements giving the number of ticks on each axis, in which case the axes will NOT move so that they may obscure the data.
	# ---origin--- is (1) a vector of two elements representing the origin of the global coordinate system (G), (2) the numbering index of the ping in the total sequence of pings of the event, which is to be regarded as the origin of (G) (ignoring heave so that the x-y-plane of (G) is on the surface of the sea), or (3) NULL, implying that the origin be put to the mid point of the vessel posistions.
	# ---xlab--- is the label for the x axis.
	# ---ylab--- is the label for the y axis.
	# ---zlab--- is the label for the z axis.
	##### (3) Used when plotting vessel position: #####
	# ---edge.vpos--- defines the placement of the vessel position, where edge.vpos="x-+" places the vessel position information on the upper south edge of the plot.
	# ---line.vpos--- adjusts the position of the text by the given number of lines
	# ---at.vpos--- adjusts the position left/right (see mtext3d()).
	# ---cex.vpos--- is the character expansion of the vessel information.
	# ---col.vpos---  is the color for the vessel position information.
	##### (4) Used when plotting date and time: #####
	# ---clock--- is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
	#		"bbl" = bottom (minimum of z) bottom (minimum of y) left (minimum of x)
	#		"bbr" = bottom (minimum of z) bottom (minimum of y) right (maximum of x)
	#		"btl" = bottom (minimum of z) top (maximum of y) left (minimum of x)
	#		"btr" = bottom (minimum of z) top (maximum of y) right (maximum of x)
	#		"tbl" = top (maximum of z) bottom (minimum of y) left (minimum of x)
	#		"tbr" = top (maximum of z) bottom (minimum of y) right (maximum of x)
	#		"ttl" = top (maximum of z) top (maximum of y) left (minimum of x)
	#		"ttr" = top (maximum of z) top (maximum of y) right (maximum of x)
	# ---cex.clock--- is the numeric character expansion value for the clock.
	# ---adj.clock--- is used in text3d() when plotting the date and time.
	# ---format.clock--- is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\n" apparing in 'format.clock' cause a line break.
	# ---digits.clock--- is a numeric specifying the number of digits of the seconds part of the time.
	# ---lsp.clock--- is the spacing factor for the line break, given as a multiple of the corresponding x-, y- or zlim. Default is 0.04.
	# ---col.clock--- is the color of the plotted time and date.
	##### (5) Used when plotting the sonar grid: #####
	# ---sonar.grid--- is a string representing whether to plot the sonar grid as a projection onto the surface ("proj"), as a grid around the sonar volume ("frame") or no grid ("").
	# ---sonar.grid.col--- is a vector of two strings specifying the color to use for plotting the sonar grid edges, where the first is the main color, and the second is the color to use for lines that are above the plotting frame, but that are plotted onto the top of the frame.
	# ---sonar.grid.lwd--- is the line width of the sonar grid.
	# ---cs.pos--- defines the coordinate system to use when generating the points. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel. If given as a three element vector, 'cs.pos' is interpreted as c(cs.pos,cs.view,cs.xyzlim).
	# ---cs.view--- defines the coordinate system to use when specifying the view point. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
	# ---sides--- is a vector of two strings specifying on which sides of the sonar volume to plot edges of the beams and bows, respectively, where "t" is "top", "r" is "right", "b" is "bottom" and "l" is "left".
	# ---dens--- is a vector of 4 elements specifying 
	#		(1) the number of points along the edges of the frame to plot ('length_edges' in plot_volume.MS70()), 
	#		(2) the number of points along each bow (on each side to plot, so that 100 gives 400 points along each bow if all sides are to be plotted, equal to 'length_bows' in plot_volume.MS70()), 
	#		(3) the radial distance between the bows (defaulted to 100 for the MS70 sonar equal to 'bowdens' in plot_volume.MS70()), and 
	#		(4) the density of the beams 'beamdens'.
	# ---every--- (used in plot_volume.ME70()) is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers givint the time steps for which the frame should be plotted (not including the first and last time step).
	##### (6) Used when plotting the global grid: #####
	# ---global.grid--- is the spacing between the gridlines of the global grid, plotted if 'global.grid' is not FALSE or empty. If global.grid==TRUE, the grid width is set to 100.
	# ---global.grid.lwd--- is the line width of the global grid lines.
	# ---global.grid.lty--- is the line type of the global grid lines.
	##### Passed on to add.sonar.grid(): #####
	# ---...--- are inputs passed on to add.sonar.grid().
	
	
	##################################################
	##################################################
	##### Preparation #####
	getSpec <- function(ll, key){
		if(length(ll)==0){
			return(list(extract=NULL, rest=NULL))
		}
		n <- names(ll)
		hit <- endsWith(n, key)
		extract <- ll[hit]
		names(extract) <- gsub(key, "", names(extract))
		list(extract=extract, rest=ll[!hit])
	}
	
	
	# Store the arguments to be passed on to plot3d():
	ll=list(...)
	
	
	##### Execution end output #####
	### (1) Add frame bounding box, aspect, titles: ###
	if(plot){
		# If not set, default the colors to black:
		if(length(ll$col.lab)==0){
			ll$col.lab <- 1
		}
		if(length(ll$col.axis)==0){
			ll$col.axis <- 1
		}
		
		# Get cex.lab, col.lab and font.lab:
		add <- getSpec(ll, ".lab")
		ll <- add$rest
		add <- add$extract
		#callist=c(list(axes=FALSE, aspect=aspect, xlim=xyzlim[,1], ylim=xyzlim[,2], zlim=xyzlim[,3], xlab=xlab, ylab=ylab, zlab=zlab), add, ll)
		#do.call("decorate3d", callist[unique(names(callist))])
		
		# This was added on 2017-03-23 to be able to specify cex and col of labels and axes, and splitting up into decorate for the bounding box and title for the labels was needed since decorate3d did not succeed in plotting labels (bug?):
		callist=c(list(axes=FALSE, aspect=aspect, xlim=xyzlim[,1], ylim=xyzlim[,2], zlim=xyzlim[,3], xlab=NULL, ylab=NULL, zlab=NULL), ll)
		do.call("decorate3d", callist[unique(names(callist))])
		
		callist=c(list(xlab=xlab, ylab=ylab, zlab=zlab), add, ll)
		do.call("title3d", callist[unique(names(callist))])
	}
	### (2) Add axes: ###
	if(plot && !identical(ll$axes, FALSE)){
		nticks=rep(nticks, length.out=3)
		if(length(origin)==0){
			# Define labels for the tick marks, calculated relative to the center of the plot when origin==NULL:
			tickxlab=pretty(xyzlim[1:2]-mean(xyzlim[1:2]), n=nticks[1])
			tickylab=pretty(xyzlim[3:4]-mean(xyzlim[3:4]), n=nticks[2])
			# Translate back to the actual values in the plot:
			xat=tickxlab+mean(xyzlim[1:2])
			yat=tickylab+mean(xyzlim[3:4])
			# Discard points on/outside of the bounding box:
			validx=xat>xyzlim[1]-diff(xyzlim[1:2])*0.01 & xat<xyzlim[2]+diff(xyzlim[1:2])*0.01
			validy=yat>xyzlim[3]-diff(xyzlim[3:4])*0.01 & yat<xyzlim[4]+diff(xyzlim[3:4])*0.01
			xat=xat[validx]
			yat=yat[validy]
			tickxlab=tickxlab[validx]
			tickylab=tickylab[validy]
			}
		else{
			# Define positions for the tick marks:
			xat=pretty(xyzlim[1:2], n=nticks[1])
			yat=pretty(xyzlim[3:4], n=nticks[2])
			# Discard points on/outside of the bounding box, but allow for one percent margin:
			validx=xat>xyzlim[1]-diff(xyzlim[1:2])*0.01 & xat<xyzlim[2]+diff(xyzlim[1:2])*0.01
			validy=yat>xyzlim[3]-diff(xyzlim[3:4])*0.01 & yat<xyzlim[4]+diff(xyzlim[3:4])*0.01
			xat=xat[validx]
			yat=yat[validy]
			# Use the positions as labels:
			tickxlab=xat
			tickylab=yat
			}
		# Define positions for the tick marks:
		zat=pretty(xyzlim[5:6], n=nticks[3])
		# Discard points on/outside of the bounding box:
		validz=zat>xyzlim[5] & zat<xyzlim[6]
		zat=zat[validz]
		# If any nticks are set to 0, move these ticks far away: 
		if(nticks[1]==0){
			xat=1e100
			}
		if(nticks[2]==0){
			yat=1e100
			}
		if(nticks[3]==0){
			zat=1e100
			}
		
		# Plot the axes:
		# Get cex.axis, col.axis and font.axis:
		# cex.axis cannot be set in axes3d(), which does not accept a cex argument. This we need to extract the cex.axis and use this in par3d(), changing the global settings of the figure:
		if(length(ll$cex.axis)){
			save <- par3d(cex=ll$cex.axis)
			#on.exit(par3d(save))
		}
		
		add <- getSpec(ll, ".axis")
		ll <- add$rest
		add <- add$extract
		callist <- c(list(edges="bbox", xat=xat, yat=yat, zat=zat, xlab=tickxlab, ylab=tickylab, box=ll$box), add)
		#do.call("axes3d", list(edges="bbox", xat=xat, yat=yat, zat=zat, xlab=tickxlab, ylab=tickylab, box=ll$box))
		do.call("axes3d", callist[unique(names(callist))])
		if(full.box){
			box3d()
			}
		}
		
	### (3) Add vessel position: ###
	if(plot && length(edge.vpos)>0 && sum(nchar(edge.vpos))>0 && !identical(edge.vpos, FALSE)){
		mtext3d(as.expression(DD2DMS(data$lonv[1], data$latv[1])), edge=edge.vpos, line=line.vpos, at=at.vpos, col=col.vpos, cex=cex.vpos)
		}
	### (4) Add date and time: ###
	if(plot){
		add.clock(clock=clock, utim=data$utim, indt=data$indt, cex.clock=cex.clock, adj.clock=adj.clock, format.clock=format.clock, digits.clock=digits.clock, lsp.clock=lsp.clock, col.clock=col.clock)
		}
	### (5) Add the sonar grid: ###
	if(plot && sonar.grid[1] %in% c("frame","proj")){
		add.sonar.grid(data, xyzlim=xyzlim, xyzlim.out=FALSE, sonar.grid=sonar.grid, sonar.grid.col=sonar.grid.col, sonar.grid.lwd=sonar.grid.lwd, cs.pos=cs.pos, cs.view=cs.view, sides=sides, dens=dens, every=every, ...)
		}
	### (6) Add the global grid: ###
	if(plot && length(global.grid)>0 && !identical(global.grid,FALSE)){
		global.grid.x=ceiling(xyzlim[1,1]/global.grid[1]):floor(xyzlim[2,1]/global.grid[1])*global.grid[1]
		global.grid.y=ceiling(xyzlim[1,2]/global.grid[1]):floor(xyzlim[2,2]/global.grid[1])*global.grid[1]
		grid3d("z", at = list(x=global.grid.x,y=global.grid.y), col="gray", lwd = global.grid.lwd, lty = global.grid.lty, n = 5)
		}
	##################################################
	##################################################
	}
