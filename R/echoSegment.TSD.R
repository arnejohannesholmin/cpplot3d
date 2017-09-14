#*********************************************
#*********************************************
#' Extracts a subvolume of the data enclosed in the bounding box or ellipsoid specified in the input, and calculates the total echo energy in the subvolume. Optionally the bounding object is plotted on top of an existing plot.
#'
#' @param data  is a list containing the acoustic data and the vessel data, as returned from read.event(var=c("vbsc","voxels","vessel")).
#' @param object  is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).
#' @param par  is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.
#' @param center  is a vector of three elements representing the centre position of the object.
#' @param angle  is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".
#' @param plot  is TRUE if the object should be plotted as an rgl-object.
#' @param seg.col  is the color of the object in case plot==TRUE.
#' @param alpha  is the transparacy of the object in case plot==TRUE.
#' @param subdivide  is the density of points on the plotted ellipsoid.
#' @param excl.neg  is FALSE if negative mass values are to be included in the calculation of the centers of mass of the school.
#' @param full.out  is TRUE to return voxel positions, volumes and acoustic data inside the segmente, as well as the segmentation mask.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl cube3d ellipse3d plot3d rotate3d scale3d shade3d translate3d
#' @importFrom sonR cm.school
#'
#' @export
#' @rdname echoSegment.TSD
#'
echoSegment.TSD<-function(data=NULL, object=c("ellipsoid","cuboid"), par=c(1,1,1), center=c(0,0,0), angle=0, plot=TRUE, seg.col="green", alpha=0.2, subdivide=3, excl.neg=TRUE, full.out=FALSE,...){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-04-26 - Clean version.
	# Update: 2012-02-07 - Added the distance 'dsts' to the school center.
	# Update: 2013-01-07 - Restructured to accept no data but only plotting.
	# Last: 2013-08-07 - Added 'excl.neg'.
	########### DESCRIPTION: ###########
	# Extracts a subvolume of the data enclosed in the bounding box or ellipsoid specified in the input, and calculates the total echo energy in the subvolume. Optionally the bounding object is plotted on top of an existing plot.
	########## DEPENDENCIES: ###########
	# inside.object3d()
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	# ---data--- is a list containing the acoustic data and the vessel data, as returned from read.event(var=c("vbsc","voxels","vessel")).
	# ---object--- is string representing the type of object to use. Currently implemented are "ellipsoid" and "cuboid" (may be abbreviated).
	# ---par--- is a vector of three elements representing the semi axis lengths for ellipsoid and the x-width, y-depth and z-height of cuboid.
	# ---center--- is a vector of three elements representing the centre position of the object.
	# ---angle--- is the angle of the major axis of the ellipsoid in the x-y-plane, in the case that object=="ellipsoid".
	# ---plot--- is TRUE if the object should be plotted as an rgl-object.
	# ---seg.col--- is the color of the object in case plot==TRUE.
	# ---alpha--- is the transparacy of the object in case plot==TRUE.
	# ---subdivide--- is the density of points on the plotted ellipsoid.
	# ---excl.neg--- is FALSE if negative mass values are to be included in the calculation of the centers of mass of the school.
	# ---full.out--- is TRUE to return voxel positions, volumes and acoustic data inside the segmente, as well as the segmentation mask.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# 'par' may be given as a single numeric to indicate sphere or cube:
	if(length(par)<3){
		par=rep(par,length.out=3)
		}
	# Check wheter 'angle' is given in degrees:
	if(-4*pi>angle || 4*pi<angle){
		angle=angle*pi/180
		warning("'angle' converted to radians")
		}
	# Plot the object:
	if(plot){
		if(tolower(substr(object[1],1,1))=="e"){
			if(length(par)>2){
				size=(diag(3)*par)^2
				ee=rgl::ellipse3d(size,centre=center,t=1,subdivide=subdivide)
				ee=rgl::translate3d(ee,-center[1],-center[2],-center[3])
				ee=rgl::rotate3d(ee,angle,0,0,1)
				ee=rgl::translate3d(ee,center[1],center[2],center[3])
				rgl::shade3d(ee,col=seg.col,alpha=alpha,add=TRUE,...)
				}
			}
		else if(tolower(substr(object[1],1,1))=="c"){
			if(length(par)>2){
				cc=rgl::cube3d()
				cc=rgl::scale3d(cc,par[1],par[2],par[3])
				cc=rgl::rotate3d(cc,angle,0,0,1)
				cc=rgl::translate3d(cc,center[1],center[2],center[3])
				rgl::shade3d(cc,col=seg.col,alpha=alpha,add=TRUE,...)
				}
			}
		else{
			stop("Unknown object")
			}
		}
	
	
	##### Execution and output #####
	# Calculate the total energy and stuff inside the segmentation object only if there are voxels available and the segmentation object has non-zero size:
	if(!any(is.null(data$psxx),is.null(data$psyx),is.null(data$pszx), all(par==0))){
		# Locate the data inside the object:
		inside=inside.object3d(x=cbind(c(data$psxx),c(data$psyx),c(data$pszx)), object=object, par=par, center=center, angle=angle, logical=FALSE)
		# Calculate the total echo energy and the total volume of the voxels inside the object, the center of mass of the segmentation mask, the angle of incidence to the segmentation mask, and the number of voxels included in the segmentation mask:
		tvol=sum(data$volx[inside],na.rm=TRUE)
		dsts=sqrt((data$psxx[inside]-center[1])^2 + (data$psyx[inside]-center[2])^2 + (data$pszx[inside]-center[3])^2)
		# Calculate the center of mass of the segmented school:
		cm=cm.school(data, subset=inside, excl.neg=excl.neg)
		# Define the segmentation information to return:
		aniS=atan2(data$psyv-cm[2],data$psxv-cm[1])
		anio=atan2(data$psyv-center[2],data$psxv-center[1])
		nseg=sum(as.numeric(inside))
		
		# Return the total echo energy and the segment indexes, along wth the sv, volume, coordinates and distance to the center of the segmentation object for the segmented voxels, and the angle of incidence to the segmentation mask, and the number of voxels included in the segmentation mask:
		if(!is.null(data$vbsc)){
			tvbs=sum(data$vbsc[inside] * data$volx[inside], na.rm=TRUE)
			vbss=data$vbsc[inside]
			}
		else{
			tvbs=NA
			vbss=NA
			}
		if(full.out){
			list(tvbs=tvbs, tvol=tvol, aniS=aniS, psxS=cm[1], psyS=cm[2], pszS=cm[3], nseg=nseg, sgmt=inside, psxs=data$psxx[inside], psys=data$psyx[inside], pszs=data$pszx[inside], vbss=vbss, vols=data$volx[inside], dsts=dsts, anio=anio, psxo=center[1], psyo=center[2], pszo=center[3], szxo=par[1], szyo=par[2], szzo=par[3], ango=angle, typo=c("ellipsoid","cuboid")[substring(object[1],1,1)==c("e","c")])
			}
		else{
			list(tvbs=tvbs, tvol=tvol, aniS=aniS, psxS=cm[1], psyS=cm[2], pszS=cm[3], nseg=nseg, anio=anio, psxo=center[1], psyo=center[2], pszo=center[3], szxo=par[1], szyo=par[2], szzo=par[3], ango=angle, typo=c("ellipsoid","cuboid")[substring(object[1],1,1)==c("e","c")])
			}
		}
	else{
		return(list())
		}
	##################################################
	##################################################
	}
