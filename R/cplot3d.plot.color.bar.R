#*********************************************
#*********************************************
#' Plots the color bar of a cplot3d-plot.
#'
#' @param color.bar.col  is the color of the color bar, generated inside cplot3d.TSD().
#' @param breaks  is the number of breaks of the scale on which the data 'z' are arranged (generated in cplot3d.TSD()).
#' @param white  is an integer value representing the number of the lower breaks for which points should not be plotted (corresponding to white points).
#' @param log  is TRUE if log transformed data are to be plotted. If so, all values must be positive.
#' @param color.bar  is a string of three characters, where the first is one of "x", "y", "z", and denotes along which dimension the color bar should be parallel, the second and third characters are either "-" or "+", and denotes along which of the four edges parallel to the axis given by the first charater, the color bar should be put (where the axes are ranged "x", "y", "z"). color.bar="x-+" puts the color bar on the top south edge, if the x-axis is defined east, the y-axis north and the z-axis vertical.
#' @param color.bar.lwd  is the line width of the color bar (thickness).
#' @param color.bar.adj  is a vector of adjustment values for the color bar (in the x, y, and z direction), where the values are given as fractions of the size of the corresponding dimensions. If given as a single numeric, 'color.bar.adj' is interpreted as the adjustment in the lowest available dimension (for example in the y direciton if color.bar="x--").
#' @param color.bar.tadj  is similar to 'color.bar.adj', but for the labels of the tickmarks on the color bar.
#' @param color.bar.noWhite  is FALSE to include all values which are not plotted due to 'white' in the color bar.
#' @param color.bar.nticks  is the desired number of ticks on the color bar, used in pretty().
#' @param color.bar.tickw  is the thickness of the tick marks on the color bar, as a fraction of the plotting frame.
#' @param color.bar.tickcol  is the color of the tich marks.
#' @param db  is FALSE if the color bar should display linear (not desibel) values.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl lines3d mtext3d par3d
#'
#' @export
#' @rdname cplot3d.plot.color.bar
#'
cplot3d.plot.color.bar<-function(color.bar.col, breaks=40, white=1, log=TRUE, color.bar="x--", color.bar.lwd=8, color.bar.adj=0, color.bar.tadj=0.1, color.bar.noWhite=TRUE, color.bar.nticks=8, color.bar.tickw=0.005, color.bar.tickcol="black", db=TRUE){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-08-16 - Clean version.
	# Last: 2013-08-19 - Added 'color.bar.tadj' and removed 'fit.inside'.
	# Last: 2013-08-19 - Removed 'color.breaks' and fixed bugs.
	########### DESCRIPTION: ###########
	# Plots the color bar of a cplot3d-plot.
	########## DEPENDENCIES: ###########
	# zeros()
	############ VARIABLES: ############
	# ---color.bar.col--- is the color of the color bar, generated inside cplot3d.TSD().
	# ---breaks--- is the number of breaks of the scale on which the data 'z' are arranged (generated in cplot3d.TSD()).
	# ---white--- is an integer value representing the number of the lower breaks for which points should not be plotted (corresponding to white points).
	# ---log--- is TRUE if log transformed data are to be plotted. If so, all values must be positive.
	# ---color.bar--- is a string of three characters, where the first is one of "x", "y", "z", and denotes along which dimension the color bar should be parallel, the second and third characters are either "-" or "+", and denotes along which of the four edges parallel to the axis given by the first charater, the color bar should be put (where the axes are ranged "x", "y", "z"). color.bar="x-+" puts the color bar on the top south edge, if the x-axis is defined east, the y-axis north and the z-axis vertical.
	# ---color.bar.lwd--- is the line width of the color bar (thickness).
	# ---color.bar.adj--- is a vector of adjustment values for the color bar (in the x, y, and z direction), where the values are given as fractions of the size of the corresponding dimensions. If given as a single numeric, 'color.bar.adj' is interpreted as the adjustment in the lowest available dimension (for example in the y direciton if color.bar="x--").
	# ---color.bar.tadj--- is similar to 'color.bar.adj', but for the labels of the tickmarks on the color bar.
	# ---color.bar.noWhite--- is FALSE to include all values which are not plotted due to 'white' in the color bar.
	# ---color.bar.nticks--- is the desired number of ticks on the color bar, used in pretty().
	# ---color.bar.tickw--- is the thickness of the tick marks on the color bar, as a fraction of the plotting frame.
	# ---color.bar.tickcol--- is the color of the tich marks.
	# ---db--- is FALSE if the color bar should display linear (not desibel) values.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# Get the number of beaks
	lbreaks=length(color.bar.col)
	# Set the positions and other specifications for plotting the color bar of the plot. This may be in the way of the data, but effort is made to avoid this using the 'addoutside' variable:
	# If the data are displayed logarithmically, construct equally spaced desibel valued bins of width as a multiple of 5 dB, where the highest tick mark is as close to the max value of 'data$vbsc' as possible, and the other tick marks are calculated so that the number of tick marks does not exceed 'color.breaks':
	if(color.bar.noWhite){
		db0=min(breaks[seq(white,length(breaks))])
		}
	else{
		db0=min(breaks)
		}
	db1=max(breaks)
	# Apply pretty() to get the ticks on the color bar:
	ticks=pretty(c(db0,db1),n=color.bar.nticks)
	# Make sure these are inside the range of the data:
	ticks=ticks[ticks>db0 & ticks<db1]
	# 'atticks' is the positions of the tick marks on the range of x-, y- or z-values, depending on the first character in 'color.bar':
	atticks=(ticks-db0)/(db1-db0)
	
	# Make sure the ticks have the corregt value according to 'db':	
	if(log && !db){
		ticks=10^(ticks/10)
		}
	else if(!log && db){
		ticks=10*log10(ticks)
		}
	# If the color bar is in linear values and the minimum value is small, use sientific formating, and add more space between the color bar and the notation on the tick marks:
	if(!db && min(ticks)<0.01){
		scientific=TRUE
		}
	else{
		scientific=FALSE
		}
	latticks=length(atticks)
	
	# Decode the value of "color.bar":
	if(isTRUE(color.bar)){
		color.bar="y--"
		}
	color.bar=strsplit(tolower(color.bar),"")[[1]]
	if(length(color.bar)<3){
		color.bar=c(color.bar,rep("-",3-length(color.bar)))
		}
	if( !all(color.bar[1] %in% c("x","y","z"), color.bar[2] %in% c("-","+"), color.bar[3] %in% c("-","+")) ){
		color.bar=c("y","-","-")
		warning("Invalid value of 'color.bar'. Set to \"y--\"")
		}
	charpm1=as.numeric(paste(color.bar[2],1,sep=""))
	charpm2=as.numeric(paste(color.bar[3],1,sep=""))
	charonetwo1=1+(charpm1>0)
	charonetwo2=1+(charpm2>0)
	
	color.bar.adj=unlist(color.bar.adj)
	
	
	# Place the color bar according to the value of 'color.bar':
	xyzlim=par3d()$bbox
	dim(xyzlim)=c(2,3)
	
	# 'color.bar.adj' is given as fractions of the plotting frame dimensions:
	color.bar.adj=color.bar.adj * (xyzlim[2,]-xyzlim[1,])
		
	if(color.bar[1]=="x"){
		# If 'color.bar.adj' or 'color.bar.adj' has length 1, it is interpreted as the adjustment in the first available dimension:
		if(length(color.bar.adj)==1){
			color.bar.adj=c(0,color.bar.adj,0)
			}
		if(length(color.bar.tadj)==1){
			color.bar.tadj=c(0,color.bar.tadj,0)
			}
		# Expand 'xyzlim' if 'color.bar.adj' is positive along the "y" or "z" dimension:
		if(color.bar.adj[2]>0){
			xyzlim[charonetwo1,2]=xyzlim[charonetwo1,2]+charpm1*color.bar.adj[2]
			color.bar.adj[2]=0
			}
		else{
			color.bar.adj[2]=charpm1*color.bar.adj[2]
			}
		if(color.bar.adj[3]>0){
			xyzlim[charonetwo2,3]=xyzlim[charonetwo2,3]+charpm2*color.bar.adj[3]
			color.bar.adj[3]=0
			}
		else{
			color.bar.adj[3]=charpm1*color.bar.adj[3]
			}
		
		# Set the tick width of the tichmarks on the color bar, given as a fraction of the width of the plot:
		color.bar.tickw=diff(xyzlim[,1])*color.bar.tickw/2
		# Set the positions of the color bar:
		x=seq(xyzlim[1,1],xyzlim[2,1],length.out=lbreaks)
		y=rep(xyzlim[charonetwo1,2],length.out=lbreaks)+color.bar.adj[2]
		z=rep(xyzlim[charonetwo2,3],length.out=lbreaks)+color.bar.adj[3]
		# Set the positions of the tick marks:
		tickx=xyzlim[1,1]+atticks*diff(xyzlim[,1])
		ticky=rep(xyzlim[charonetwo1,2],length.out=latticks)+color.bar.adj[2]
		tickz=rep(xyzlim[charonetwo2,3],length.out=latticks)+color.bar.adj[3]
		tickpos=tickx
		# Set the values to add to the tick mark positions:
		addtoticks=cbind(c(-color.bar.tickw,color.bar.tickw),c(0,0),c(0,0))
		}
	else if(color.bar[1]=="y"){
		# If 'color.bar.adj' or 'color.bar.adj' has length 1, it is interpreted as the adjustment in the first available dimension:
		if(length(color.bar.adj)==1){
			color.bar.adj=c(color.bar.adj,0,0)
			}
		if(length(color.bar.tadj)==1){
			color.bar.tadj=c(color.bar.tadj,0,0)
			}
		# Expand 'xyzlim' if 'color.bar.adj' is positive along the "y" or "z" dimension:
		if(color.bar.adj[1]>0){
			xyzlim[charonetwo1,1]=xyzlim[charonetwo1,1]+charpm1*color.bar.adj[1]
			color.bar.adj[1]=0
			}
		else{
			color.bar.adj[1]=charpm1*color.bar.adj[1]
			}
		if(color.bar.adj[3]>0){
			xyzlim[charonetwo2,3]=xyzlim[charonetwo2,3]+charpm2*color.bar.adj[3]
			color.bar.adj[3]=0
			}
		else{
			color.bar.adj[3]=charpm1*color.bar.adj[3]
			}
		
		# Set the tick width of the tichmarks on the color bar, given as a fraction of the width of the plot:
		color.bar.tickw=diff(xyzlim[,2])*color.bar.tickw/2
		# Set the positions of the color bar:
		x=rep(xyzlim[charonetwo1,1],length.out=lbreaks)+color.bar.adj[1]
		y=seq(xyzlim[1,2],xyzlim[2,2],length.out=lbreaks)
		z=rep(xyzlim[charonetwo2,3],length.out=lbreaks)+color.bar.adj[3]
		# Set the positions of the tick marks:
		tickx=rep(xyzlim[charonetwo1,1],length.out=latticks)+color.bar.adj[1]
		ticky=xyzlim[1,2]+atticks*diff(xyzlim[,2])
		tickz=rep(xyzlim[charonetwo2,3],length.out=latticks)+color.bar.adj[3]
		tickpos=ticky
		# Set the values to add to the tick mark positions:
		addtoticks=cbind(c(0,0),c(-color.bar.tickw,color.bar.tickw),c(0,0))
		}
	else if(color.bar[1]=="z"){
		# If 'color.bar.adj' or 'color.bar.adj' has length 1, it is interpreted as the adjustment in the first available dimension:
		if(length(color.bar.adj)==1){
			color.bar.adj=c(0,color.bar.adj,0)
			}
		if(length(color.bar.tadj)==1){
			color.bar.tadj=c(0,color.bar.tadj,0)
			}
		# Expand 'xyzlim' if 'color.bar.adj' is positive along the "y" or "z" dimension:
		if(color.bar.adj[1]>0){
			xyzlim[charonetwo1,1]=xyzlim[charonetwo1,1]+charpm1*color.bar.adj[1]
			color.bar.adj[1]=0
			}
		else{
			color.bar.adj[1]=charpm1*color.bar.adj[1]
			}
		if(color.bar.adj[2]>0){
			xyzlim[charonetwo2,2]=xyzlim[charonetwo2,2]+charpm2*color.bar.adj[2]
			color.bar.adj[2]=0
			}
		else{
			color.bar.adj[2]=charpm1*color.bar.adj[2]
			}
		
		# Set the tick width of the tichmarks on the color bar, given as a fraction of the width of the plot:
		color.bar.tickw=diff(xyzlim[,3])*color.bar.tickw/2
		# Set the positions of the color bar:
		x=rep(xyzlim[charonetwo1,1],length.out=lbreaks)+color.bar.adj[1]
		y=rep(xyzlim[charonetwo2,2],length.out=lbreaks)+color.bar.adj[2]
		z=seq(xyzlim[1,3],xyzlim[2,3],length.out=lbreaks)
		# Set the positions of the tick marks:
		tickx=rep(xyzlim[charonetwo1,1],length.out=latticks)+color.bar.adj[1]
		ticky=rep(xyzlim[charonetwo2,2],length.out=latticks)+color.bar.adj[2]
		tickz=xyzlim[1,3]+atticks*diff(xyzlim[,3])
		tickpos=tickz
		# Set the values to add to the tick mark positions:
		addtoticks=cbind(c(0,0),c(0,0),c(-color.bar.tickw,color.bar.tickw))
		}
	
				
	########## Execution ##########
	# Add ticks first, so that these are visible on the color bar. Here the widhts 'addtoticks' of the tickmarks are added, and 'addoutside' is subtracted to put the color bar further into the plotting fram than the tick annotation:
	for(i in seq_along(tickx)){
		lines3d(tickx[i]+addtoticks[,1], ticky[i]+addtoticks[,2], tickz[i]+addtoticks[,3], lwd=color.bar.lwd, col=color.bar.tickcol)
		}
	# Plot the color bar using lines3d() and the tickmarks using text3d():
	lines3d(x,y,z,col=color.bar.col,lwd=color.bar.lwd)
	# The notation on the color bar:
	if(db){
		ticks=paste(round(ticks,digits=1),"dB")
		}
	else{
		ticks=format(ticks,scientific=scientific,digits=3)
		}
	
	# Add labels:
	# 'color.bar.tadj' is given as fractions of the plotting frame dimensions:
	color.bar.tadj=color.bar.tadj * (xyzlim[2,]-xyzlim[1,])
	if(color.bar[1]=="x"){
		color.bar.tadj[2:3]=c(charpm1,charpm2)*color.bar.tadj[2:3]
		mtext3d(text=ticks,edge=paste(color.bar,collapse=""),at=tickpos,pos=c(NA,xyzlim[charonetwo1,2],xyzlim[charonetwo2,3])+color.bar.tadj)
		}
	else if(color.bar[1]=="y"){
		color.bar.tadj[c(1,3)]=c(charpm1,charpm2)*color.bar.tadj[c(1,3)]
		mtext3d(text=ticks,edge=paste(color.bar,collapse=""),at=tickpos,pos=c(xyzlim[charonetwo1,1],NA,xyzlim[charonetwo2,3])+color.bar.tadj)
		}
	else if(color.bar[1]=="z"){
		color.bar.tadj[1:2]=c(charpm1,charpm2)*color.bar.tadj[1:2]
		mtext3d(text=ticks,edge=paste(color.bar,collapse=""),at=tickpos,pos=c(xyzlim[charonetwo1,1],xyzlim[charonetwo2,2],NA)+color.bar.tadj)
		}
	
			
	########## Output ##########
	xyzlim
	##################################################
	##################################################
	}
