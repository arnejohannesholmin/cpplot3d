#*********************************************
#*********************************************
#' Plots fish as points or lines in 3-D, from data "psxf", "psyf", "pszf", "rtzf", "rtxf" given in 'data'.
#'
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t  is either a vector of the numbers of the pings to be returned, as listed from 1 to the number of pings in the event, or a vector of time points given as strings "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF" from which the range of the time points to be read is extracted. If t=="all", all files are read and if t=="none" an empty list is returned.
#' @param schoolcol  is the color of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsize  is the size of the points of the fish positions if school==TRUE or a string representing the directory containing .school files to be read for fish positions.
#' @param schoolsample  is the proportion of the fish positions given by the .school specified by 'school', that are to be sampled for plotting. Useful when the number of fish is large.
#' @param schoollen  is the length of the fish when plotted as lines.
#' @param schoollwd  is the width of the fish when plotted as lines.
#' @param schooltype  is either "p" (default) for plotting points a the fish positions, "l" for plotting the fish as lines in the direcitons of the fish, or "o", for plotting lines with a dot at the head of the fish. Several values can be given.
#' @param add  is TRUE if points are to be added to an existing plot.
#' @param aspect  is used to set the dimension of the plotting frame (see aspect3d()).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl plot3d
#' @importFrom TSD NAs sph2car strff
#'
#' @export
#' @rdname fplot3d.TSD
#'
fplot3d.TSD<-function(data, schoolcol="red2", schoolsize=0.3, schoolsample=0.01, schoollen=4, schoollwd=1, schooltype="p", add=FALSE, aspect="iso", ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-04-03 - Clean version.
	########### DESCRIPTION: ###########
	# Plots fish as points or lines in 3-D, from data "psxf", "psyf", "pszf", "rtzf", "rtxf" given in 'data'.
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
	# ---add--- is TRUE if points are to be added to an existing plot.
	# ---aspect--- is used to set the dimension of the plotting frame (see aspect3d()).
	
	
	##################################################
	##################################################
	########## Preparation ##########
	ll=list(...)
	# Draw the school sample to reduce the number of points plotted:
	thissample=sample(seq_along(data$psxf),length(data$psxf)*schoolsample)
	data$psxf=data$psxf[thissample]
	data$psyf=data$psyf[thissample]
	data$pszf=data$pszf[thissample]
	if(length(data$rtxf)>0){
		data$rtxf=data$rtxf[thissample]
		}
	if(length(data$rtzf)>0){
		data$rtzf=data$rtzf[thissample]
		}
	
	
	########## Execution and output ##########
	# Plot the directions of the fish if the rotation angles were read in cpploted.event():
	if(any(strff(c("l","o"),schooltype))){
		# Define line segments representing fish with directions:
		addxyz=sph2car(cbind(schoollen/2,pi/2+data$rtzf,pi/2+data$rtxf))
		fromto=NAs(3*length(data$psxf),3)
		fromto[seq(1,nrow(fromto),3),]=cbind(data$psxf,data$psyf,data$pszf)-addxyz
		fromto[seq(2,nrow(fromto),3),]=cbind(data$psxf,data$psyf,data$pszf)+addxyz
		# Plot lines:
		calllist=c(list(x=fromto[,1],y=fromto[,2],z=fromto[,3],add=add,col=schoolcol,lwd=schoollwd,type="l",aspect=aspect), ll, list(xlab="x",ylab="y",zlab="z"))
		do.call("plot3d",calllist[unique(names(calllist))])
		# Add points at the heads:
		if(strff("o",schooltype)){
			calllist=c(list(x=fromto[seq(2,nrow(fromto),3),1],y=fromto[seq(2,nrow(fromto),3),2],z=fromto[seq(2,nrow(fromto),3),3],add=TRUE,col=schoolcol,size=schoolsize,aspect=aspect), ll, list(xlab="x",ylab="y",zlab="z"))
			do.call("plot3d",calllist[unique(names(calllist))])
			}
		}
	# Plot points:
	if(strff("p",schooltype)){
		calllist=c(list(x=data$psxf,y=data$psyf,z=data$pszf,add=add,col=schoolcol,size=schoolsize,aspect=aspect), ll, list(xlab="x",ylab="y",zlab="z"))
		do.call("plot3d",calllist[unique(names(calllist))])
		}
	##################################################
	##################################################
	}
