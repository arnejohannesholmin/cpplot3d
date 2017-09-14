#*********************************************
#*********************************************
#' A simple but unelegant way of plotting the sonar volume.
#'
#' @param data  is a list of data including the elements named "dira", "dire", "asps", "sint", "lenb", "rtzv", "psxv", "psyv", "pszv" and "psze".
#' @param xyzlim  is eihter a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values, or NA, indicating that the ranges of the sonar grid are to be returned.
#' @param t  is the time step number of which to plot the sonar grid in the case that data$vbsc has a third dimension indicating time. 't' must be given as a single integer in the range seq_along(data$psxv).
#' @param plot  is either "proj", for plotting the projection of the sonar volume onto the surface (the old method) or "frame" for plotting the frame around the sonar volume.
#' @param cs.pos  is a single character representing the coordinate system used for the plotted data (one of "g" for global or "v" for vessel).
#' @param cs.view  is a single character representing the coordinate system used for the view point (one of "g" for global or "v" for vessel).
#' @param length_edges  is the number of points along the bows, which if chosen too low can result in lines appearing not to reach the bounding box.
#' @param length_bows  is the number of points along the edges, which if chosen too low can result in lines appearing not to reach the bounding box.
#' @param bowdens  is the distance in meters between bows. If given as a two element vector, the first is for the bows on the lower end of the sonar volume and the second is for the bows on the horizontal possibly projected end.
#' @param col  is a vector of two color elements (string or integers) representing the color for the plotted frame, and the color of the lines forced onto the top of the bounding box respectively (in case parts of the frame is above the bounding box).
#' @param sides  is a vector of two strings, one for the lines and one for the bows, including the characters "b" for bottom grid, "l" for left grid (as seed from the sonar), "t" for the top grid and "r" for the left grid.
#' @param ...  are inputs passed on to lines3d().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl lines3d
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR extractTimeStep rotate3D
#' @importFrom TSD dim_all labl.TSD NAs sph2car zeros
#' @importFrom stats median
#'
#' @export
#' @rdname plot_volume.MS70
#'
plot_volume.MS70 <- function(data, xyzlim=NULL, t=1, plot=c("proj","frame"), cs.pos="g", cs.view="g", length_edges=200, length_bows=100, bowdens=100, beamdens=1, col=c("orange","cornflowerblue"), sides=c("trbl","trbl"), ends=FALSE, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-01-15 - Clean version (used in animate.event() for ages).
	# Update: 2011-06-15 - Changed the use of lines3d() to be through do.call(), to avoid error when alpha is used in the parent function (alpha set to 1 for this funciton).
	# Update: 2011-08-25 - Added the option 't' specifying for which of the time steps located in 'data' the sonar volume should be plotted.
	# Update: 2011-10-10 - Changed to have an extra NA at the end of the sonar volume, so that the entire sonar grid can be plotted in one turn using lines3d() (no time consuming for loop).
	# Update: 2011-11-23 - Fixing bug in restricting the intersect_edges to the plotting frame.
	# Update: 2011-11-23 - Added the option 'ends' for plotting the ends of the beams.
	# Last: 2015-11-02 - Less CPU time by merging bows and intersect_bows into a matrix.
	########### DESCRIPTION: ###########
	# A simple but unelegant way of plotting the sonar volume.
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---data--- is a list of data including the elements named "dira", "dire", "asps", "sint", "lenb", "rtzv", "psxv", "psyv", "pszv" and "psze".
	# ---xyzlim--- is eihter a three column matrix of two rows, where the first column represents the plotting range of x-values and the second and third for the y- and z-values, or NA, indicating that the ranges of the sonar grid are to be returned.
	# ---t--- is the time step number of which to plot the sonar grid in the case that data$vbsc has a third dimension indicating time. 't' must be given as a single integer in the range seq_along(data$psxv).
	# ---plot--- is either "proj", for plotting the projection of the sonar volume onto the surface (the old method) or "frame" for plotting the frame around the sonar volume.
	# ---cs.pos--- is a single character representing the coordinate system used for the plotted data (one of "g" for global or "v" for vessel).
	# ---cs.view--- is a single character representing the coordinate system used for the view point (one of "g" for global or "v" for vessel).
	# ---length_edges--- is the number of points along the bows, which if chosen too low can result in lines appearing not to reach the bounding box.
	# ---length_bows--- is the number of points along the edges, which if chosen too low can result in lines appearing not to reach the bounding box.
	# ---bowdens--- is the distance in meters between bows. If given as a two element vector, the first is for the bows on the lower end of the sonar volume and the second is for the bows on the horizontal possibly projected end.
	# ---col--- is a vector of two color elements (string or integers) representing the color for the plotted frame, and the color of the lines forced onto the top of the bounding box respectively (in case parts of the frame is above the bounding box).
	# ---sides--- is a vector of two strings, one for the lines and one for the bows, including the characters "b" for bottom grid, "l" for left grid (as seed from the sonar), "t" for the top grid and "r" for the left grid.
	# ---...--- are inputs passed on to lines3d().
	

	##################################################
	##################################################
	##### Preparation, execution and output #####
	# Simple function for generating a sequence with 1 and 'l' as endpoints and approximately 'd' difference between the integers:
	s = function(l,d){
		L = (l-1)/seq_len(l-1)
		valid = which(round(L)==L)
		d = valid[which.min(abs(d-valid))]
		seq(1,l,d)
		}
	
	# Expand bowdens to two elements:
	bowdens = rep(bowdens,length.out=2)
	
	# Select the specified time step:
	data = extractTimeStep(data, t, var=c("psxv","psyv","pszv","rtxv","rtyv","rtzv", labl.TSD("rb")))
	# Check if the required variables are present:
	if(!all(c("dira","dire","asps","sint","lenb","psxv","psyv","pszv","psze","rtzv") %in% names(data))){
		warning("The required variables were not present in 'data' (\"dira\",\"dire\",\"asps\",\"sint\",\"lenb\",\"psxv\",\"psyv\",\"pszv\",\"psze\",\"rtzv\"). No sonar grid plotted")
		return()
		}
	
	# If 'xyzlim' is NULL, assign a matrix of zeros:
	if(length(xyzlim)==0){
		xyzlim = zeros(2,3)
		}
	# Else if 'xyzlim' is NA, do the same but indicate by a fourth unused colum, so that this input is recognized later, when the calucated xyzlim of the full frame is to be returned:
	else if(is.na(xyzlim[1])){
		xyzlim = zeros(2,4)
		xyzlim[2,3] = data$pszv
		}
	# Else check that 'xyzlim' has the correct dimension:
	else if(!all.equal(dim_all(xyzlim),c(2,3))){
		stop("'xyzlim' must be NULL (no pre-specified value), NA (return the limits of the full frame) or a matrix of dimension (2,3)")
		}
	# The list 'll' aids in ensuring that error is avoided when alpha is used in the parent function (alpha set to 1 for this funciton):
	ll = list(...)
	# If the length of the first string in 'plot' is of length 0, terminate the function:
	if(nchar(plot[1])==0){
		return()
		}
	n1 = length(unique(data$dira))
	n2 = length(data$dira)/n1
	# Radial partitioning (as a sequence of distances from r=0 to (lenb-0.5)*dr, where 'lenb' is the maximum length of the beams and -0.5 is to adjust for the fact that the midpoint of the first voxel is at r=0, and we here wish to plot the edges of the voxels):
	R = soundbeam_range(data, pos=length_edges)
	#R = seq(0, data$asps[1]*data$sint[1]/2 * (max(data$lenb)-0.5), l=length_edges)
	dr = diff(R[1:2])
	# Prepare plotting parameters:
	col = rep(col,length.out=2)
	sides = rep(sides,length.out=2)
	sides = strsplit(sides,"")
	# Plot only projection of the sonar volume on the sea surface (original method, expanded for plot=="frame" to plotting the volume itself):
	if(plot[1]=="proj"){
		# Define angles for the points on the edges:
		# Theta (azimuth):
		dtheta = abs(median(diff(unique(data$dira))))
		theta = c(sort(unique(data$dira))-dtheta/2,max(data$dira)+dtheta/2)
		# Add vessel rotation if global coordinate system is used for generating the points:
		if(identical(cs.pos,"g")){
			theta = theta+data$rtzv
			}
		else{
			theta = theta
			}
		# Phi (elevation):
		phi = rep(pi/2,length(theta))
		
		### Define edges: ###
		# Define points along the edges of the beams:
		edges = zeros(length_edges,3,length(theta))
		# Add vessel position if global coordinate system is used for generating the points:
		edges[,1,] = outer(R,cos(theta))
		edges[,2,] = outer(R,sin(theta))
		if(identical(cs.pos,"g")){
			edges[,1,] = edges[,1,]+data$psxv
			edges[,2,] = edges[,2,]+data$psyv
			}
		edges[,3,] = xyzlim[2,3]
		# Apply the beam density:
		#if(length(beamdens)==1){
		#	beamdens=unique(s(length(theta),beamdens))
		#	}
		beamdens = unique(s(length(theta),beamdens))
		edges[,,-beamdens] = NA
		### Define bows: ###
		# Define points along bows at distances separated d by 'bowdens':
		bows = zeros(length_bows,3,ceiling(max(R)/bowdens[1]))
		# Add vessel position if global coordinate system is used for generating the points:
		if(identical(cs.pos,"g")){
			for(i in seq_along(bows[1,1,])){
				bows[,,i] = matrix(c(data$psxv,data$psyv,xyzlim[2,3]),ncol=3,nrow=length_bows,byrow=TRUE) + sph2car(cbind(bowdens[1]*i,seq(min(theta),max(theta),length.out=length_bows),pi/2))
				}
			}
		else{
			for(i in seq_along(bows[1,1,])){
				bows[,,i] = matrix(c(0,0,xyzlim[2,3]),ncol=3,nrow=length_bows,byrow=TRUE) + sph2car(cbind(bowdens[1]*i,seq(min(theta),max(theta),length.out=length_bows),pi/2))
				}
			}
		# Return the limits of the plotting frame is xyzlim==NA:
		if(ncol(xyzlim)==4){
			return(cbind(range(edges[,1,],bows[,1,],na.rm=TRUE),range(edges[,2,],bows[,2,],na.rm=TRUE),range(edges[,3,],bows[,3,],na.rm=TRUE)))
			}
		
		### Restrict edges: ###
		# Select the points inside the plotting volume:
		inside = xyzlim[1,1]<=edges[,1,] & xyzlim[2,1]>=edges[,1,] & xyzlim[1,2]<=edges[,2,] & xyzlim[2,2]>=edges[,2,]
		edges[,1,][!inside] = NA
		edges[,2,][!inside] = NA
		### Restrict bows: ###
		# Select the points inside the plotting volume:
		inside = xyzlim[1,1]<=bows[,1,] & xyzlim[2,1]>=bows[,1,] & xyzlim[1,2]<=bows[,2,] & xyzlim[2,2]>=bows[,2,]
		bows[,1,][!inside] = NA
		bows[,2,][!inside] = NA
				
		### Plot edges: ###
		# Plot the lines:
		for(i in seq_len(dim(edges)[3])){
			# Only plot if the edge has more than one non-NA point:
			if(sum(!apply(edges[,,i],1,function(x) any(is.na(x))))>1){
				#lines3d(edges[,,i],col=col[1],alpha=1,...)
				thisl = list(x=edges[,,i],col=col[1],alpha=1)
				otherl = ll[setdiff(names(ll),names(thisl))]
				do.call("lines3d",c(thisl,otherl))
				}
			}
		### Plot bows: ###
		# Plot the bows:
		for(i in seq_len(dim(bows)[3])){
			# Only plot if the bow has more than one non-NA point:
			if(sum(!apply(bows[,,i],1,function(x) any(is.na(x))))>1){
				#lines3d(bows[,,i],col=col[1],alpha=1,...)
				thisl = list(x=bows[,,i],col=col[1],alpha=1)
				otherl = ll[setdiff(names(ll),names(thisl))]
				do.call("lines3d",c(thisl,otherl))
				}		
			}
		}
	# Plot the sonar volume:
	else if(plot[1]=="frame"){
		
		# Define the indexes of the edges and the points on the bows related to each of the following: "top", "right", "bottom" and "left":
		sidestrings = c("t","r","b","l")
		# Edges:
		sideindexes_edges = c(rep(1,n1),rep(2,n2-1),rep(3,n1+1),rep(4,n2-1),1)
		sides_edges = sideindexes_edges %in% which(sidestrings %in% sides[[1]])
		# Bows:
		sideindexes_bows = rep(1:4,each=length_bows)
		sides_bows = sideindexes_bows %in% which(sidestrings %in% sides[[2]])
		
		# Define the distance from the top plotting frame to the sonar depth:
		h = data$pszv[1]+data$psze[1]-xyzlim[2,3]
		
		
		### Define edges: ###
		# Define angles for the points on the edges:
		# Theta (azimuth):
		dtheta = abs(median(diff(unique(data$dira))))
		theta = c( sort(unique(data$dira))[-1]-dtheta/2, rep(max(data$dira)+dtheta/2,n2+1) , rev(sort(unique(data$dira))[-1]-dtheta/2) , rep(min(data$dira)-dtheta/2,n2+1))
		# Add vessel rotation if global coordinate system is used for generating the points:
		if(identical(cs.pos,"g")){
			theta = theta+data$rtzv
			}
		# Phi (elevation):
		dphi = abs(median(diff(unique(data$dire))))
		phi = c(rep(min(data$dire)-dphi/2,n1), sort(unique(data$dire))[-1]-dphi/2 , rep(max(data$dire)+dphi/2,n1+1), rev(sort(unique(data$dire))[-1]-dphi/2) , min(data$dire)-dphi/2 )
		
		# Define points along the edges of the beams:
		suppressWarnings(edges<-rotate3D(cbind(c(R,NA),0,0),by="yz",ang=cbind(-phi+pi/2,-theta)))
		# Add sonar position in (V):
		edges[,3,] = edges[,3,]+data$psze[1]
		# Add vessel position if global coordinate system is used for generating the points:
		if(identical(cs.pos,"g")){
			edges[,1,] = edges[,1,]+data$psxv
			edges[,2,] = edges[,2,]+data$psyv
			edges[,3,] = edges[,3,]+data$pszv
			}
		
		# Force points above the sea surface onto the surface:
		abovesea = edges[,3,]
		abovesea[abovesea>0] = 0
		edges[,3,] = abovesea
		# Restrict to the sides given by 'sides':
		edges[,,!sides_edges] = NA
		
		# Apply the beam density:
		l = c(n1,length(unique(data$dire)))
		l = c(l,l)
		#if(length(beamdens)==1){
		#	beamdens=unique(c(s(l[1]+1,beamdens),s(l[2]+1,beamdens)+l[1],s(l[3]+1,beamdens)+sum(l[1:2]),s(l[4]+1,beamdens)+sum(l[1:3])))-1
		#	}
		if(length(beamdens)==1){
			beamdens = rep(beamdens,2)
			}
		beamdens_intersect = unique(c(s(l[1]+1,beamdens[2]),s(l[2]+1,beamdens[2])+l[1],s(l[3]+1,beamdens[2])+sum(l[1:2]),s(l[4]+1,beamdens[2])+sum(l[1:3])))-1
		beamdens = unique(c(s(l[1]+1,beamdens[1]),s(l[2]+1,beamdens[1])+l[1],s(l[3]+1,beamdens[1])+sum(l[1:2]),s(l[4]+1,beamdens[1])+sum(l[1:3])))-1
		
		edges[,,-beamdens] = NA
		
		
		### Define intersect_edges: ###
		# Edges intersecting the plotting frame:
		r_min = h/tan((h<0)*min(phi) + (h>=0)*max(phi)-pi/2)
		r_max = max(R)*cos(atan(h/max(R)))
		newR = R
		# Set points on the intersect edges to NA if they are not between r_min-dr and r_max, where the subtraction of 'dr' is for esthetic purposes:
		newR[newR<(r_min-dr) | newR>r_max] = NA
		# Rotate the 'intersect_edges':
		suppressWarnings(intersect_edges<-rotate3D(cbind(c(newR,NA),0,0),by="z",ang=-theta))
		# Add vessel position if global coordinate system is used for generating the points:
		if(identical(cs.pos,"g")){
			intersect_edges[,1,] = intersect_edges[,1,]+data$psxv
			intersect_edges[,2,] = intersect_edges[,2,]+data$psyv
			}
		else{
			intersect_edges[,1,] = intersect_edges[,1,]
			intersect_edges[,2,] = intersect_edges[,2,]
			}
		#intersect_edges[,1,]=intersect_edges[,1,]+data$psxv
		#intersect_edges[,2,]=intersect_edges[,2,]+data$psyv
		intersect_edges[,3,] = xyzlim[2,3]
		# Restrict to the sides given by 'sides':
		intersect_edges[,,!sides_edges] = NA
		intersect_edges[,,-beamdens_intersect] = NA
		
		
		### Define bows: ###
		# Define points along bows at distances separated by 'bowdens':
		bows = zeros(length_bows*4,3,ceiling(max(R)/bowdens[1]))
		for(i in seq_along(bows[1,1,])){
			# Define angles for the points on the bows:
			thetas = c( seq(min(theta),max(theta),length.out=length_bows) , rep(max(theta),length_bows) , seq(max(theta),min(theta),length.out=length_bows) , rep(min(theta),length_bows))
			phis = c( rep(min(phi),length_bows) , seq(min(phi),max(phi),length.out=length_bows) , rep(max(phi),length_bows) , seq(max(phi),min(phi),length.out=length_bows) )
			# Add vessel position if global coordinate system is used for generating the points:
			if(identical(cs.pos,"g")){
				for(i in seq_along(bows[1,1,])){
					bows[,,i] = matrix(c(data$psxv,data$psyv,data$pszv+data$psze),ncol=3,nrow=length_bows*4,byrow=TRUE) + sph2car(cbind(bowdens[1]*i,thetas,phis))
					}
				}
			else{
				for(i in seq_along(bows[1,1,])){
					bows[,,i] = matrix(c(0,0,data$psze),ncol=3,nrow=length_bows*4,byrow=TRUE) + sph2car(cbind(bowdens[1]*i,thetas,phis))
					}
				}
			}
		# Force points above the sea surface onto the surface:
		abovesea = bows[,3,]
		abovesea[abovesea>0] = 0
		bows[,3,] = abovesea
		# Discard bows on sides not specified by 'sides':
		bows[!sides_bows,,] = NA
		
		### Define intersect_bows: ###
		# Define points on bows forced onto the plotting frame:
		intersect_bows = zeros(length_bows, 3, ceiling(max(R)/bowdens[2]))
		phi_bowdens = suppressWarnings(asin( h/(bowdens[2] * seq_len(ceiling(max(R)/bowdens[2])) ))+pi/2)
		for(i in seq_along(intersect_bows[1,1,])){
			# Define angles for the points on the bows:
			thetas = seq(min(theta),max(theta), length.out=length_bows)
			phis = suppressWarnings(rep(asin(h/(bowdens[2]*i))+pi/2, length.out=length_bows))
			# Add vessel position if global coordinate system is used for generating the points:
			if(identical(cs.pos,"g")){
				for(i in seq_along(bows[1,1,])){
					intersect_bows[,,i] = matrix(c(data$psxv,data$psyv,data$pszv+data$psze),ncol=3,nrow=length_bows,byrow=TRUE) + sph2car(cbind(bowdens[2]*i,thetas,phis))
					}
				}
			else{
				for(i in seq_along(bows[1,1,])){
					intersect_bows[,,i] = matrix(c(0,0,data$psze),ncol=3,nrow=length_bows,byrow=TRUE) + sph2car(cbind(bowdens[2]*i,thetas,phis))
					}
				}
			}
		intersect_bows[,3,] = xyzlim[2,3]
		intersect_bows[,,is.na(phi_bowdens)] = NA
		if(h<0){
			too_low_angle = phi_bowdens<min(phi)
			intersect_bows[,,too_low_angle] = NA
			too_high_angle = phi_bowdens>max(phi)
			intersect_bows[,,too_high_angle] = NA
			}
		else{
			too_low_angle = phi_bowdens<min(phi)
			intersect_bows[,,too_low_angle] = NA
			too_high_angle = phi_bowdens>max(phi)
			intersect_bows[,,too_high_angle] = NA
			}
		if(!any(c("t"%in%sides[[1]],"t"%in%sides[[2]]))){
			intersect_bows = NAs(dim_all(intersect_bows))
			intersect_edges = NAs(dim_all(intersect_edges))
			}
		
		
		### Define ends: ###
		if(ends){
			theta = c( sort(unique(data$dira))-dtheta/2, max(data$dira)+dtheta/2 )
			# Add vessel rotation if global coordinate system is used for generating the points:
			if(identical(cs.pos,"g")){
				theta = theta+data$rtzv
				}
			# Phi (elevation):
			phi = c( sort(unique(data$dire))-dphi/2, max(data$dire)+dphi/2 )
		
			# Define points along the ends of the beams:
			ends = zeros((length_bows+1)*(n1+n2+2),3)
			# Define angles for the points on the bows:
			thetas = c( rep(c(seq(min(theta),max(theta),length.out=length_bows),NA),n2+1), rep(theta,each=length_bows+1) )
			phis = c( rep(phi,each=length_bows+1), rep(c(seq(min(phi),max(phi),length.out=length_bows),NA),n1+1) )
		
			# Add vessel position if global coordinate system is used for generating the points:
			if(identical(cs.pos,"g")){
				ends = matrix(c(data$psxv,data$psyv,data$pszv+data$psze),ncol=3,nrow=length(thetas),byrow=TRUE) + sph2car(cbind(max(R),thetas,phis))
				}
			else{
				ends = matrix(c(0,0,data$psze),ncol=3,nrow=length(thetas),byrow=TRUE) + sph2car(cbind(max(R),thetas,phis))
				}
			}
			
			
		
		# Return the limits of the plotting frame is xyzlim==NA:
		if(ncol(xyzlim)==4){
			return(cbind(range(edges[,1,],intersect_edges[,1,],bows[,1,],intersect_bows[,1,],na.rm=TRUE),range(edges[,2,],intersect_edges[,2,],bows[,2,],intersect_bows[,2,],na.rm=TRUE),range(edges[,3,],intersect_edges[,3,],bows[,3,],intersect_bows[,3,],na.rm=TRUE)))
			}
		
		### Restrict edges: ###
		# Select the points of the edges that are inside the plotting volume:
		inside = xyzlim[1,1]<=edges[,1,] & xyzlim[2,1]>=edges[,1,] & xyzlim[1,2]<=edges[,2,] & xyzlim[2,2]>=edges[,2,] & xyzlim[1,3]<=edges[,3,] & xyzlim[2,3]>=edges[,3,]
		edges[,1,][!inside] = NA
		edges[,2,][!inside] = NA
		edges[,3,][!inside] = NA
		### Restrict intersect_edges: ###	
		# Select the points of the intersect_edges that are inside the plotting volume:
		inside_intersect_edges = xyzlim[1,1]<=intersect_edges[,1,] & xyzlim[2,1]>=intersect_edges[,1,] & xyzlim[1,2]<=intersect_edges[,2,] & xyzlim[2,2]>=intersect_edges[,2,] & xyzlim[1,3]<=intersect_edges[,3,] & xyzlim[2,3]>=intersect_edges[,3,]
		intersect_edges[,1,][!inside_intersect_edges] = NA
		intersect_edges[,2,][!inside_intersect_edges] = NA
		intersect_edges[,3,][!inside_intersect_edges] = NA
		### Restrict bows: ###
		# Select the points of the bows that are inside the plotting volume:
		inside = xyzlim[1,1]<=bows[,1,] & xyzlim[2,1]>=bows[,1,] & xyzlim[1,2]<=bows[,2,] & xyzlim[2,2]>=bows[,2,] & xyzlim[1,3]<=bows[,3,] & xyzlim[2,3]>=bows[,3,]
		bows[,1,][!inside] = NA
		bows[,2,][!inside] = NA
		bows[,3,][!inside] = NA
		### Restrict intersect_bows: ###
		# Select the points of the intersect_bows that are inside the plotting volume:
		inside_intersect_bows = xyzlim[1,1]<=intersect_bows[,1,] & xyzlim[2,1]>=intersect_bows[,1,] & xyzlim[1,2]<=intersect_bows[,2,] & xyzlim[2,2]>=intersect_bows[,2,] & xyzlim[1,3]<=intersect_bows[,3,] & xyzlim[2,3]>=intersect_bows[,3,]
		intersect_bows[,1,][!inside_intersect_bows] = NA
		intersect_bows[,2,][!inside_intersect_bows] = NA
		intersect_bows[,3,][!inside_intersect_bows] = NA
		
		
		### Restrict ends: ###
		if(length(ends)>1){
			# Select the points of the bows that are inside the plotting volume:
			inside = xyzlim[1,1]<=ends[,1] & xyzlim[2,1]>=ends[,1] & xyzlim[1,2]<=ends[,2] & xyzlim[2,2]>=ends[,2] & xyzlim[1,3]<=ends[,3] & xyzlim[2,3]>=ends[,3]
			ends[,1][!inside]=NA
			ends[,2][!inside]=NA
			ends[,3][!inside]=NA
			}		
		
		
		
		
		### Plot edges: ###
		anyedges = !any(all(is.na(edges[,1,])),all(is.na(edges[,2,])),all(is.na(edges[,3,])))
		anyintersect_edges = !any(all(is.na(intersect_edges[,1,])),all(is.na(intersect_edges[,2,])),all(is.na(intersect_edges[,3,])))
		anybows = !any(all(is.na(bows[,1,])),all(is.na(bows[,2,])),all(is.na(bows[,3,])))
		anyintersect_bows = !any(all(is.na(intersect_bows[,1,])),all(is.na(intersect_bows[,2,])),all(is.na(intersect_bows[,3,])))
		if(length(ends)>1){
			anyends = !any(all(is.na(ends[,1])),all(is.na(ends[,2])),all(is.na(ends[,3])))
			}
		# Plot the lines:
		if(anyedges){
			thisl = list(x=edges[,1,],y=edges[,2,],z=edges[,3,],col=col[1],alpha=1)
			otherl = ll[setdiff(names(ll),names(thisl))]
			do.call("lines3d",c(thisl,otherl))
			}
		# Plot lines forced onto the plotting frame:
		if(xyzlim[2,3]<0 && anyintersect_edges){
			thisl = list(x=intersect_edges[,1,],y=intersect_edges[,2,],z=intersect_edges[,3,],col=col[2],alpha=1)
			otherl = ll[setdiff(names(ll),names(thisl))]
			do.call("lines3d",c(thisl,otherl))
			}
		### Plot bows: ###
		# Plot the bows:
		if(anybows){
			newbows = NAs(dim(bows)+c(1,0,0))
			newbows[-nrow(newbows),,] = bows
			newbows = aperm(newbows, c(1,3,2))
			dim(newbows) = c(prod(dim(newbows)[1:2]), dim(newbows)[3])
			thisl = list(x=newbows, col=col[1], alpha=1)
			otherl = ll[setdiff(names(ll),names(thisl))]
			do.call("lines3d",c(thisl,otherl))
			#for(i in seq_len(dim(bows)[3])){
			#	# Only plot if the bow has more than one non-NA point:
			#	if(sum(!apply(bows[,,i],1,function(x) any(is.na(x))))>1){
			#		#lines3d(bows[,,i],col=col[1],alpha=1,...)
			#		thisl = list(x=bows[,,i],col=col[1],alpha=1)
			#		otherl = ll[setdiff(names(ll),names(thisl))]
			#		do.call("lines3d",c(thisl,otherl))
			#		}		
			#	}
			}
		# Plot bows forced onto the plotting frame:
		if(xyzlim[2,3]<0 && anyintersect_bows){
			newbintersect_bows = NAs(dim(intersect_bows)+c(1,0,0))
			newbintersect_bows[-nrow(newbintersect_bows),,] = intersect_bows
			newbintersect_bows = aperm(newbintersect_bows, c(1,3,2))
			dim(newbintersect_bows) = c(prod(dim(newbintersect_bows)[1:2]), dim(newbintersect_bows)[3])
			thisl = list(x=newbintersect_bows,col=col[2],alpha=1)
			otherl = ll[setdiff(names(ll),names(thisl))]
			do.call("lines3d",c(thisl,otherl))
			#for(i in seq_len(dim(intersect_bows)[3])){
			#	# Only plot if the bow has more than one non-NA point:
			#	if(sides_bows[i] && sum(!apply(intersect_bows[,,i],1,function(x) any(is.na(x))))>1){
			#		#lines3d(intersect_bows[,,i],col=col[2],alpha=1,...)
			#		thisl = list(x=intersect_bows[,,i],col=col[2],alpha=1)
			#		otherl = ll[setdiff(names(ll),names(thisl))]
			#		do.call("lines3d",c(thisl,otherl))
			#		}
			#	}
			}
		# Plot the endsH:
		if(length(ends)>1){
			if(anyends){
				# Only plot if the bow has more than one non-NA point:
				if(sum(!apply(ends,1,function(x) any(is.na(x))))>1){
					#lines3d(bows[,,i],col=col[1],alpha=1,...)
					thisl = list(x=ends,col=col[1],alpha=1)
					otherl = ll[setdiff(names(ll),names(thisl))]
					do.call("lines3d",c(thisl,otherl))
					}		
				}
			}
		}
	##################################################
	##################################################
	}
