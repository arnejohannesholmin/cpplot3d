#*********************************************
#*********************************************
#' Plots fish as points or lines in 3-D, from data "psxf", "psyf", "pszf", "rtzf", "rtxf" located in the event.
#'
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t  is either a vector of the numbers of the pings to be returned, as listed from 1 to the number of pings in the event, or a vector of time points given as strings "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF" from which the range of the time points to be read is extracted. If t=="all", all files are read and if t=="none" an empty list is returned.
#' @param schoolcol  is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsize  is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsample  is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
#' @param schoollen  is the length of the fish when plotted as lines.
#' @param schoollwd  is the width of the fish when plotted as lines.
#' @param schooltype  is either "p" (default) for plotting points a the fish positions, "l" for plotting the fish as lines in the direcitons of the fish, or "o", for plotting lines with a dot at the head of the fish. Several values can be given.
#' @param schoolnumt  is the number of time steps of the school, specified if the school data has been recycled and thus should be recycled when plotting as well.
#' @param add  is TRUE if points are to be added to an existing plot.
#' @param aspect  is used to set the dimension of the plotting frame (see aspect3d()).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event
#'
#' @export
#' @rdname fplot3d.event
#'
fplot3d.event<-function(event, t=1, schoolcol="red2", schoolsize=0.3, schoolsample=0.01, schoollen=4, schoollwd=1, schooltype="p", schoolnumt=NULL, add=FALSE, aspect="iso", ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-04-03 - Clean version.
	########### DESCRIPTION: ###########
	# Plots fish as points or lines in 3-D, from data "psxf", "psyf", "pszf", "rtzf", "rtxf" located in the event.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# ---t--- is either a vector of the numbers of the pings to be returned, as listed from 1 to the number of pings in the event, or a vector of time points given as strings "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF" from which the range of the time points to be read is extracted. If t=="all", all files are read and if t=="none" an empty list is returned.
	# ---schoolcol--- is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
	# ---schoolsize--- is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
	# ---schoolsample--- is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
	# ---schoollen--- is the length of the fish when plotted as lines.
	# ---schoollwd--- is the width of the fish when plotted as lines.
	# ---schooltype--- is either "p" (default) for plotting points a the fish positions, "l" for plotting the fish as lines in the direcitons of the fish, or "o", for plotting lines with a dot at the head of the fish. Several values can be given.
	# ---schoolnumt--- is the number of time steps of the school, specified if the school data has been recycled and thus should be recycled when plotting as well.
	# ---add--- is TRUE if points are to be added to an existing plot.
	# ---aspect--- is used to set the dimension of the plotting frame (see aspect3d()).
	
	
	##################################################
	##################################################
	data=read.event(event=event,t=if(length(schoolnumt)>0) t%%schoolnumt else t,var="school")
	fplot3d.TSD(data,schoolcol=schoolcol, schoolsize=schoolsize, schoolsample=schoolsample, schoollen=schoollen, schoollwd=schoollwd, schooltype=schooltype, add=add, aspect=aspect, ...)
	##################################################
	##################################################
	}
