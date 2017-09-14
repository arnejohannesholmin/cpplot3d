#*********************************************
#*********************************************
#' Transforms 'xlim', 'ylim', and 'zlim' to a matrix of limits given the 'data' and the type of the inputs.
#'
#' @param xlim  is the x limits (x1, x2) of the plot. 'xlim' can be given as a list containing the elements 'xlim', 'ylim', and 'zlim' (names can be skipped, in which case the first element is interpreted as 'xlim', and so on). 'xlim' can also be given as a matrix cbind(xlim,ylim,zlim). 'xlim' can be used both to enlarge and to shrink the plotting frame (as opposed to plot3d(), in which xlim is only used to enlarge the plotting frame). If 'range' has been used in pplot3d.TSD(), xlim can be used to specify the limits presicely. Note that x1 > x2 is allowed and leads to a ‘reversed axis’. If one of xlim, ylim or zlim is NA, all the data and the entire sonar frame are included in the plot. The individual limits can take one of the following forms:
#' @param ylim  is the same as 'xlim' but for the y-axis, and without the possibility of giving all of 'xlim', 'ylim', and 'zlim'.
#' @param zlim  is the same as 'xlim' but for the z-axis, and without the possibility of giving all of 'xlim', 'ylim', and 'zlim'.
#' @param data  is the list of TSD inputs as returned from read.event(var=c("vbsc","voxels","vessel")).
#' @param t  is the time step used as input to 'xlim', 'ylim' and 'zlim', if these are functions.
#' @param school  is TRUE if the school is to be plotted in pplot3d.TSD().
#' @param cs.pos  indicates the coordinate system of the positions to be plotted, "g" representing the global coordinate system and "v" representing the coordinate system of the vessel.
#' @param cs.xyzlim   indicates the coordinate system of the limits of the plot, "g" representing the global coordinate system and "v" representing the coordinate system of the vessel. cs.xyzlim="v" is particularly useful for setting the limits of the plotting frame to move with the vessel.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all
#'
#' @export
#' @rdname get.xyzlim
#'
get.xyzlim<-function(xlim, ylim, zlim, data=list(), t=1, school=FALSE, cs.pos="g", cs.xyzlim="g"){
		
	############### LOG: ###############
	# Start: 2013-06-19 - Clean version.
	# Last: 2013-09-30 - Fixed bug with voxel positions spanning the range for pplot3d.
	
	##################################################
	##################################################
	##### Preparation #####
	# Extract 'xlim', 'ylim' and 'zlim', if all are stored in 'xlim' in a matrix of three columns or a in list of three elements:
	if(length(xlim) && NCOL(xlim)==3){
		ylim=xlim[,2]
		zlim=xlim[,3]
		xlim=xlim[,1]
		}
	else if(is.list(xlim)){
		names1_3=substr(names(xlim),1,3)
		wherex=grep("x",names1_3)
		wherey=grep("y",names1_3)
		wherez=grep("z",names1_3)
		if(length(xlim)==1 && !any(length(wherex)>0,length(wherey)>0,length(wherez)>0)){
			xlim=xlim[[1]]
			}
		else if(length(xlim)==2 && !any(length(wherex)>0,length(wherey)>0,length(wherez)>0)){
			ylim=xlim[[2]]
			xlim=xlim[[1]]
			}
		else if(length(xlim)>2 && !any(length(wherex)>0,length(wherey)>0,length(wherez)>0)){
			ylim=xlim[[2]]
			zlim=xlim[[3]]
			xlim=xlim[[1]]
			}
		else{
			# Set ylim, if specified:
			if(length(wherey)>0){
				ylim=xlim[[wherey[1]]]
				}
			# Set zlim, if specified:
			if(length(wherez)>0){
				zlim=xlim[[wherez[1]]]
				}
			# Set xlim, if specified:
			if(length(wherex)>0){
				xlim=xlim[[wherex[1]]]
				}
			}
		}
	
	
	##### Execution #####
	# Initiate 'xyzlim' to the default if no points are to be plotted:
	xyzlim=matrix(rep(NA,6),2,3)
	psxnames=c("psxr","psxx","psxf")[seq_len(2+school)]
	psx_available=sapply(data[psxnames],function(x) sum(unlist(dim_all(x)))>0)
	# Remove the voxel positions if the regenerated points 'psxr' are given:
	if(psx_available[1] && psx_available[2]){
		psx_available[2]=!psx_available[2]
		}
	psynames=c("psyr","psyx","psyf")[seq_len(2+school)]
	psy_available=sapply(data[psynames],function(x) sum(unlist(dim_all(x)))>0)
	# Remove the voxel positions if the regenerated points 'psyr' are given:
	if(psy_available[1] && psy_available[2]){
		psy_available[2]=!psy_available[2]
		}
	psznames=c("pszr","pszx","pszf")[seq_len(2+school)]
	psz_available=sapply(data[psznames],function(x) sum(unlist(dim_all(x)))>0)
	# Remove the voxel positions if the regenerated points 'pszr' are given:
	if(psz_available[1] && psz_available[2]){
		psz_available[2]=!psz_available[2]
		}
	# Set the xlim:
	if(identical(xlim,FALSE) || length(xlim)==0){
		for(i in which(psx_available[seq_len(length(psx_available)-school)])){
			xyzlim[1,1]=min(xyzlim[1,1], unlist(data[[psxnames[i]]]), na.rm=TRUE)
			xyzlim[2,1]=max(xyzlim[2,1], unlist(data[[psxnames[i]]]), na.rm=TRUE)
			}
		}
	else{
		if(is.function(xlim)){
			xyzlim[,1]=xlim(t)
			}
		else if(length(xlim)==1){
			xyzlim[,1]=c(-xlim,xlim)
			}
		else{
			xyzlim[,1]=c(min(xlim,na.rm=TRUE),max(xlim,na.rm=TRUE))
			}
		}
	# Set the ylim:
	if(identical(ylim,FALSE) || length(ylim)==0){
		for(i in which(psy_available[seq_len(length(psy_available)-school)])){
			xyzlim[1,2]=min(xyzlim[1,2], unlist(data[[psynames[i]]]), na.rm=TRUE)
			xyzlim[2,2]=max(xyzlim[2,2], unlist(data[[psynames[i]]]), na.rm=TRUE)
			}
		}
	else{
		if(is.function(ylim)){
			xyzlim[,2]=ylim(t)
			}
		else if(length(ylim)==1){
			xyzlim[,2]=c(-ylim,ylim)
			}
		else{
			xyzlim[,2]=c(min(ylim,na.rm=TRUE),max(ylim,na.rm=TRUE))
			}
		}
	# Set the zlim:
	if(identical(zlim,FALSE) || length(zlim)==0){
		for(i in which(psz_available[seq_len(length(psz_available)-school)])){
			xyzlim[1,3]=min(xyzlim[1,3], unlist(data[[psznames[i]]]), na.rm=TRUE)
			xyzlim[2,3]=max(xyzlim[2,3], unlist(data[[psznames[i]]]), na.rm=TRUE)
			}
		}
	else{
		if(is.function(zlim)){
			xyzlim[,3]=zlim(t)
			}
		else if(length(zlim)==1){
			xyzlim[,3]=c(-zlim,zlim)
			}
		else{
			xyzlim[,3]=c(min(zlim,na.rm=TRUE),max(zlim,na.rm=TRUE))
			}
		}
	# Compensate to the coordinate systems specified:
	if(identical(cs.pos,"g") && identical(cs.xyzlim,"v")){
		xyzlim=cbind(xyzlim[,1]+data$psxv,xyzlim[,2]+data$psyv,xyzlim[,3]+data$pszv)
		}
	else if(identical(cs.pos,"v") && identical(cs.xyzlim,"g")){
		xyzlim=cbind(xyzlim[,1]-data$psxv,xyzlim[,2]-data$psyv,xyzlim[,3]-data$pszv)
		}
	if(any(is.infinite(xyzlim))){
		warning("Infinite plotting frame. Either ranges given by 'xyzlim' are invalid, or if xyzlim points to the ranges of one of the pings, this ping is empty, or if the list 'range' has length 3, these ranges are invalid (maybe wrongly named list elements)")
		}
	
	
	##### Output #####
	xyzlim
	##################################################
	##################################################
	}
