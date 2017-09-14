#*********************************************
#*********************************************
#' Plots or calculates the limits of sonar grids for the MS70 or the ME70.
#'
#' @param data  is a list containing the data to plot, as returned from read.event(..., var=c("vbsc", "voxels", "ctd", "beams", "vessel", "time")). May include school positions data$psxf, data$psyf and data$pszf.
#' @param xyzlim  is a 2 x 3 matrix of the limits of the bounding frame.
#' @param xyzlim.out  is TRUE if the xyzlim expanded to the limits of the sonar grid should be returned.
#' @param sonar.grid  is a string representing whether to plot the sonar grid as a projection onto the surface ("proj"), as a grid around the sonar volume ("frame") or no grid ("").
#' @param sonar.grid.col  is a vector of two strings specifying the color to use for plotting the sonar grid edges, where the first is the main color, and the second is the color to use for lines that are above the plotting frame, but that are plotted onto the top of the frame.
#' @param sonar.grid.lwd  is the line width of the sonar grid.
#' @param cs.pos  is a single character representing the coordinate system used for the plotted data (one of "g" for global or "v" for vessel).
#' @param cs.view  is a single character representing the coordinate system used for the view point (one of "g" for global or "v" for vessel).
#' @param sides  is a vector of two strings specifying on which sides of the sonar volume to plot edges of the beams and bows, respectively, where "t" is "top", "r" is "right", "b" is "bottom" and "l" is "left".
#' @param dens  is a vector of 4 elements specifying 
#' @param every  (used in plot_volume.ME70()) is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers giving the time steps for which the frame should be plotted (not including the first and last time step).
#' @param ...  are inputs passed on to plot_volume.ME70() and plot_volume.MS70().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d
#' @importFrom TSD NAs strff
#'
#' @export
#' @rdname add.sonar.grid
#'
add.sonar.grid<-function(data, xyzlim=NULL, xyzlim.out=FALSE, sonar.grid="frame", sonar.grid.col=c("orange", "cornflowerblue"), sonar.grid.lwd=1, cs.pos="g", cs.view="g", sides=c("tb", "tb"), dens=c(200, 100, 100, 1), every=Inf, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-10-02 - Clean version.
	########### DESCRIPTION: ###########
	# Plots or calculates the limits of sonar grids for the MS70 or the ME70.
	########## DEPENDENCIES: ###########
	# plot_volume.ME70(), plot_volume.MS70()
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	# ---data--- is a list containing the data to plot, as returned from read.event(..., var=c("vbsc", "voxels", "ctd", "beams", "vessel", "time")). May include school positions data$psxf, data$psyf and data$pszf.
	# ---xyzlim--- is a 2 x 3 matrix of the limits of the bounding frame.
	# ---xyzlim.out--- is TRUE if the xyzlim expanded to the limits of the sonar grid should be returned.
	# ---sonar.grid--- is a string representing whether to plot the sonar grid as a projection onto the surface ("proj"), as a grid around the sonar volume ("frame") or no grid ("").
	# ---sonar.grid.col--- is a vector of two strings specifying the color to use for plotting the sonar grid edges, where the first is the main color, and the second is the color to use for lines that are above the plotting frame, but that are plotted onto the top of the frame.
	# ---sonar.grid.lwd--- is the line width of the sonar grid.
	# ---cs.pos--- is a single character representing the coordinate system used for the plotted data (one of "g" for global or "v" for vessel).
	# ---cs.view--- is a single character representing the coordinate system used for the view point (one of "g" for global or "v" for vessel).
	# ---sides--- is a vector of two strings specifying on which sides of the sonar volume to plot edges of the beams and bows, respectively, where "t" is "top", "r" is "right", "b" is "bottom" and "l" is "left".
	# ---dens--- is a vector of 4 elements specifying 
	#		(1) the number of points along the edges of the frame to plot ('length_edges' in plot_volume.MS70()), 
	#		(2) the number of points along each bow (on each side to plot, so that 100 gives 400 points along each bow if all sides are to be plotted, equal to 'length_bows' in plot_volume.MS70()), 
	#		(3) the radial distance between the bows (defaulted to 100 for the MS70 sonar equal to 'bowdens' in plot_volume.MS70()), and 
	#		(4) the density of the beams 'beamdens'.
	# ---every--- (used in plot_volume.ME70()) is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers giving the time steps for which the frame should be plotted (not including the first and last time step).
	# ---...--- are inputs passed on to plot_volume.ME70() and plot_volume.MS70().
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(length(xyzlim)==0){
		xyzlim=par3d()$bbox
		#xyzlim=ones(6)
		dim(xyzlim)=c(2, 3)
		}
	if(sonar.grid=="proj" && !strff("ms70", data$esnm[1])){
		warning("sonar.grid==\"proj\" only applies to the MS70 sonar")
		sonar.grid="frame"
		}
	
	# 'every' can either be a single integer defining that the sonar grid should be plotted every 'every' time step, or a vector of time steps:
	if(length(every)==1){
		# Allow for every=Inf:
		if(every>length(data$psxv)){
			every=length(data$psxv)+1
			}
		# Create the time steps for which the grid should be plotted:
		every=unique(c(seq(1,length(data$psxv),every),length(data$psxv)))
		}
	else{
		every=unique(every)
		}

	
	##### Execution end output #####
	# Expand the ranges to the sonar plot grid if any of 'xlim', 'ylim' or 'zlim' are equal to NA (no plotting due to xyzlim=NA):
	if(xyzlim.out){
		# Get the sonar volume ranges for all time steps:
		if(sonar.grid[1] %in% c("frame", "proj")){
			if(is.null(data$esnm[1])){
				warning("No acoustical instrument 'esnm' specified in 'data'. MS70 sonar chosen")
				data$esnm[1]="MS70"
				}
			# MS70 (in which case multiple time steps are plotted in a for loop, while the ME70 considers many time steps at once):
			if(strff(c("ms70"), data$esnm[1])){
				# Plot each sonar.grid in a for loop using the 't' option in plot_volume.MS70():
				if(length(data$rtzv)>1){
					ranges_grid=NAs(2, 3)
					for(i in every){
						ranges_grid=merge_ranges(ranges_grid, plot_volume.MS70(data, xyzlim=NA, t=i, plot=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], beamdens=dens[-(1:3)], col=sonar.grid.col, lwd=sonar.grid.lwd, sides=sides, ...))
						}
					}
				else{
					ranges_grid=plot_volume.MS70(data, xyzlim=NA, plot=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], beamdens=dens[-(1:3)], col=sonar.grid.col, lwd=sonar.grid.lwd, sides=sides, ...)
					}
				}
			# ME70:
			else if(strff("me70", data$esnm[1])){
				ranges_grid=plot_volume.ME70(data, xyzlim=NA, every=every, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], col=sonar.grid.col[1], lwd=sonar.grid.lwd, ...)
				}
			else if(strff("sx90", data$esnm[1])){
				warning("Not implemented yet. Do it!")
				ranges_grid=plot_volume.MS70(data, xyzlim=NA, plot=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], beamdens=dens[-(1:3)], col=sonar.grid.col, lwd=sonar.grid.lwd, sides=sides, ...)
				}
			# Update the ranges by the sonar grid limits:
			xyzlim=merge_ranges(xyzlim, ranges_grid)
			}
		return(xyzlim)
		}
	else{
		# Plot the sonar volume:
		if(sonar.grid[1] %in% c("frame", "proj")){
			if(is.null(data$esnm[1])){
				warning("No acoustical instrument 'esnm' specified in 'data'. MS70 sonar chosen")
				data$esnm[1]="MS70"
				}
			# MS70 (in which case multiple time steps are plotted in a foor loop, while the ME70 considers many time steps at once):
			if(strff("ms70", data$esnm[1])){
				# Plot each sonar.grid in a for loop using the 't' option in plot_volume.MS70():
				if(length(data$rtzv)>1){
					for(i in every){
						plot_volume.MS70(data, xyzlim=xyzlim, t=i, plot=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], beamdens=dens[-(1:3)], col=sonar.grid.col, lwd=sonar.grid.lwd, sides=sides, ...)
						}
					}
				else{
					plot_volume.MS70(data, xyzlim=xyzlim, plot=sonar.grid, cs.pos=cs.pos, cs.view=cs.view, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], beamdens=dens[-(1:3)], col=sonar.grid.col, lwd=sonar.grid.lwd, sides=sides, ...)
					}
				}
			# ME70:
			else if(strff("me70", data$esnm[1])){
				plot_volume.ME70(data, xyzlim=xyzlim, every=every, length_edges=dens[1], length_bows=dens[2], bowdens=dens[3], col=sonar.grid.col[1], lwd=sonar.grid.lwd, ...)
				}
			}
		}
	##################################################
	##################################################
	}
