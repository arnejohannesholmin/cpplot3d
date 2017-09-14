#*********************************************
#*********************************************
#' A simple but unelegant way of plotting the sonar volume.
#'
#' @param data  is a list of data including the elements named "dira", "dire", "asps", "sint", "lenb", "rtzv", "psxv", "psyv", "pszv" and "psze".
#' @param xyzlim  is a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values.
#' @param every  is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers givint the time steps for which the frame should be plotted (not including the first and last time step).
#' @param length_edges  is the number of points along the bows, which if chosen too low can result in lines appearing not to reach the bounding box.
#' @param length_bows  is the number of points along the edges, which if chosen too low can result in lines appearing not to reach the bounding box.
#' @param bowdens  is the distance in meters between bows.
#' @param col  is the color for the plotted frame.
#' @param ...  are inputs passed on to lines3d().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d lines3d
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR get.specs.esnm rotate3D
#' @importFrom TSD NAs sph2car
#'
#' @export
#' @rdname plot_volume.ME70
#'
plot_volume.ME70=function(data, xyzlim=NULL, every=Inf, length_edges=200, length_bows=100, bowdens=100, col="black", ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-01-15 - Clean version (used in animate.event() for ages).
	# Update: 2011-06-15 - Changed the use of lines3d() to be through do.call(), to avoid error when alpha is used in the parent function (alpha set to 1 for this funciton).
	# Update: 2011-09-15 - Changed to have an extra NA at the end of the sonar volume, so that the entire sonar grid can be plotted in one turn using lines3d() (no time sonsuming for loop).
	# Last: 2013-09-10 - Removed 'beams'.
	########### DESCRIPTION: ###########
	# A simple but unelegant way of plotting the sonar volume.
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---data--- is a list of data including the elements named "dira", "dire", "asps", "sint", "lenb", "rtzv", "psxv", "psyv", "pszv" and "psze".
	# ---xyzlim--- is a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values.
	# ---every--- is a single integer representing that the frame should be plotted every 'every' time step, where the first and last are included, or a vector of integers givint the time steps for which the frame should be plotted (not including the first and last time step).
	# ---length_edges--- is the number of points along the bows, which if chosen too low can result in lines appearing not to reach the bounding box.
	# ---length_bows--- is the number of points along the edges, which if chosen too low can result in lines appearing not to reach the bounding box.
	# ---bowdens--- is the distance in meters between bows.
	# ---col--- is the color for the plotted frame.
	# ---...--- are inputs passed on to lines3d().
	

	##################################################
	##################################################
	##### Preparation, execution and output #####
	# The list 'll' aids in ensuring that error is avoided when alpha is used in the parent function (alpha set to 1 for this funciton):
	ll=list(...)
	# Check if the required variables are present:
	if(!all(c("dira","dire","asps","sint","lenb","psxv","psyv","pszv","psze","rtzv") %in% names(data))){
		warning("The required variables were not present in 'data' (\"dira\",\"dire\",\"asps\",\"sint\",\"lenb\",\"psxv\",\"psyv\",\"pszv\",\"psze\",\"rtzv\"). No sonar grid plotted")
		return()
		}
	
	# Return the limits 'xyzlim0' of the entire plotting frame, and do not plot if xyzlim=NA:
	xyzlim0=NAs(2,3)
	# 'every' can either be a single integer defining that the sonar grid should be plotted every 'every' time step, or a vector of time steps:
	#if(length(every)==1){
	#	# Allow for every=Inf:
	#	if(every>length(data$psxv)){
	#		every=length(data$psxv)+1
	#		}
	#	# Create the time steps for which the grid should be plotted:
	#	every=unique(c(seq(1,length(data$psxv),every),length(data$psxv)))
	#	}
	#else{
	#	every=unique(every)
	#	}
	# Radial, azimuth and elevation partitioning:
	if(length(bowdens)==0 || bowdens==0){
		minz=par3d()$bbox[5]
		maxR=min(data$pszv+data$psze)-minz
		bowdens=maxR
		}
	else{
		maxR = soundbeam_range(data, pos="max")
		#maxR=data$asps*data$sint/2*(max(data$lenb)-1)
		}
	R=seq(0,maxR,l=length_edges)
	dr=diff(R[1:2])
	
	# Select the rectangular of circular beams or both:
	valid=get.specs.esnm(data)$valid
	data$dira=data$dira[valid]
	data$dire=data$dire[valid]
	
	# Determine the two outer beams:
	leftbeams=which(data$dira>pi/2)
	minleftbeam=which.min(data$dire[leftbeams])
	minleftdire=data$dire[leftbeams[minleftbeam]]
	leftdiff=min(data$dire[leftbeams[-minleftbeam]]-minleftdire)
	minleftdire=minleftdire-leftdiff/2
	
	rightbeams=which(data$dira<pi/2)
	minrightbeam=which.min(data$dire[rightbeams])
	minrightdire=data$dire[rightbeams[minrightbeam]]
	rightdiff=min(data$dire[rightbeams[-minrightbeam]]-minrightdire)
	minrightdire=minrightdire-rightdiff/2
	
	phi=c(minleftdire,minrightdire)
	theta=c(pi,0)
	
	vesselpos=rbind(data$psxv,data$psyv,data$pszv+data$psze)
	
	# Define the along ship lines marking the outer extent of the sampling volume (discard the vessel position by [-1,], since this will be added afterwards, avioding it to be defined twice):
	angleft=as.matrix(expand.grid(c(-pi-data$rtzv,NA),-minleftdire+pi/2)[,2:1])
	angright=as.matrix(expand.grid(c(-data$rtzv,NA),-minrightdire+pi/2)[,2:1])
	
	corners=rotate3D(cbind(c(seq(0,maxR,bowdens),maxR),0,0),by="yz",ang=rbind(angleft,angright))
	corners=aperm(corners,c(3,1,2))
	corners[,,1]=corners[,,1]+c(data$psxv,NA)
	corners[,,2]=corners[,,2]+c(data$psyv,NA)
	corners[,,3]=corners[,,3]+c(data$pszv+data$psze,NA)
	dim(corners)=c(prod(dim(corners)[1:2]),dim(corners)[3])
	
	# Move through the time steps and plot the frame for the time steps given by 'every':
	for(i in every){
		
		### Define edges: ###
		# Define points along the edges of the beams:
		edges=rotate3D(cbind(c(R,NA),0,0),by="yz",ang=cbind(-phi+pi/2,-theta-data$rtzv[i]))
		# Add sonar position in (V) and vessel position:
		edges[,1,]=edges[,1,]+data$psxv[i]
		edges[,2,]=edges[,2,]+data$psyv[i]
		edges[,3,]=edges[,3,]+data$pszv[i]+data$psze
		### Define bows: ###
		Rbows=c(seq_len(maxR/bowdens)*bowdens,maxR)
					bows=NAs(length_bows+1,3,length(Rbows))
		# Define angles for the points on the bows:
		nleft=round(length_bows*length(leftbeams)/length(data$dire))
		nright=round(length_bows*length(rightbeams)/length(data$dire))
		#length_bows=nleft+nright
		thetas=c(rep(pi,nleft), rep(0,nright))  
		phis=c(seq(phi[1],pi,length.out=nleft),seq(pi,phi[2],length.out=nright))
		# Add vessel position:
		for(d in seq_along(Rbows)){
			bows[seq_len(dim(bows)[1]-1),,d]=matrix(c(data$psxv[i],data$psyv[i],data$pszv[i]+data$psze),ncol=3,nrow=length_bows,byrow=TRUE) + rotate3D(sph2car(cbind(Rbows[d],thetas,phis)),by="z",ang=-data$rtzv[i])
			}
		
		### Restrict edges: ###
		# Select the points of the edges that are inside the plotting volume:
		if(!any(is.null(xyzlim),is.na(xyzlim)[1])){
			inside=xyzlim[1,1]<=edges[,1,] & xyzlim[2,1]>=edges[,1,] & xyzlim[1,2]<=edges[,2,] & xyzlim[2,2]>=edges[,2,] & xyzlim[1,3]<=edges[,3,] & xyzlim[2,3]>=edges[,3,]
			edges[,1,][!inside]=NA
			edges[,2,][!inside]=NA
			edges[,3,][!inside]=NA
			}
		edges=aperm(edges,c(1,3,2))
		dim(edges)=c(prod(dim(edges)[1:2]),dim(edges)[3])
		### Restrict bows: ###
		# Select the points of the bows that are inside the plotting volume:
		if(!any(is.null(xyzlim),is.na(xyzlim)[1])){
			inside=xyzlim[1,1]<=bows[,1,] & xyzlim[2,1]>=bows[,1,] & xyzlim[1,2]<=bows[,2,] & xyzlim[2,2]>=bows[,2,] & xyzlim[1,3]<=bows[,3,] & xyzlim[2,3]>=bows[,3,]
			bows[,1,][!inside]=NA
			bows[,2,][!inside]=NA
			bows[,3,][!inside]=NA
			}	
					bows=aperm(bows,c(1,3,2))
					dim(bows)=c(prod(dim(bows)[1:2]),dim(bows)[3])
		
		### Plot edges: ###
		if(!is.na(xyzlim)[1]){
			# Only plot if the bow has more than one non-NA point:
			if(sum(!apply(edges,1,function(x) any(is.na(x))))>1){
				thisl=list(x=edges,col=col,alpha=1)
				otherl=ll[setdiff(names(ll),names(thisl))]
				do.call("lines3d",c(thisl,otherl))
				}
			}
		### Plot bows: ###
		if(!is.na(xyzlim)[1]){
			# Only plot if the bow has more than one non-NA point:
			if(sum(!apply(bows,1,function(x) any(is.na(x))))>1){
				thisl=list(x=bows,col=col,alpha=1)
				otherl=ll[setdiff(names(ll),names(thisl))]
				do.call("lines3d",c(thisl,otherl))
				}
			}
		
		xyzlim0=matrix(c(range(xyzlim0[,1],edges[,1],bows[,1],na.rm=TRUE),range(xyzlim0[,2],edges[,2],bows[,2],na.rm=TRUE),range(xyzlim0[,3],edges[,3],bows[,3],na.rm=TRUE)),2,3)
		}
	if(is.na(xyzlim)[1]){
		return(matrix(c(range(xyzlim0[,1],corners[,1],na.rm=TRUE),range(xyzlim0[,2],corners[,2],na.rm=TRUE),range(xyzlim0[,3],corners[,3],na.rm=TRUE)),2,3))
		}
		
	# Select the points of the corners that are inside the plotting volume:
	if(!is.null(xyzlim)){
		inside=xyzlim[1,1]<=corners[,1] & xyzlim[2,1]>=corners[,1] & xyzlim[1,2]<=corners[,2] & xyzlim[2,2]>=corners[,2] & xyzlim[1,3]<=corners[,3] & xyzlim[2,3]>=corners[,3]
		corners[!inside,]=NA
		}
	
	# Plot the corners:
	if(sum(!is.na(corners))>0){
		thisl=list(x=corners,col=col,alpha=1)
		otherl=ll[setdiff(names(ll),names(thisl))]
		do.call("lines3d",c(thisl,otherl))
		}
	##################################################
	##################################################
	}
