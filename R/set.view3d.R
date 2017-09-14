#*********************************************
#*********************************************
#' Function for extracting the view point of an rgl plot.
#'
#' @param view  defines the view point of the user and has 4 possible values:
#' @param cs.pos  defines the coordinate system to use when generating the points. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel. If given as a three element vector, 'cs.pos' is interpreted as c(cs.pos,cs.view).
#' @param cs.view  defines the coordinate system to use when specifying the view point. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
#' @param rtzv  is the heading of the vessel.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d
#' @importFrom sonR rotx rotz
#'
#' @export
#' @rdname set.view3d
#'
set.view3d = function(view=c("free","top","bottom","south","west","north","east"), cs.pos="g", cs.view="g", rtzv=0){
			
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-03-05 - Clean version, extracted from pplot3d.TSD().
	########### DESCRIPTION: ###########
	# Function for extracting the view point of an rgl plot.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	########### DESCRIPTION: ###########
	# ---view--- defines the view point of the user and has 4 possible values:
	# 		(1) FALSE, NULL or "free", for interactive selection of the view point. If view=="free" the user can continusly interactively adjust the plotting frame.
	#		(2) A 4x4 matrix representing the 'userMatrix' used in rgl.viewpoint() in the "rgl" package.
	#		(3) A string representing one of a set of predefined viewpoints:
	#			"t" = Plot seen from above with north upwards in the plot.
	#			"b" = Plot seen from below with north downwards in the plot.
	#			"s" = Plot seen from the south.
	#			"w" = Plot seen from the west.
	#			"n" = Plot seen from the north.
	#			"e" = Plot seen from the east.
	#		(4) A vector of two elements (theta,phi) representing azimuth angle (theta) and elevation angle (phi) of the view point.
	# ---cs.pos--- defines the coordinate system to use when generating the points. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel. If given as a three element vector, 'cs.pos' is interpreted as c(cs.pos,cs.view).
	# ---cs.view--- defines the coordinate system to use when specifying the view point. Given as one of "g" or "v" representing the global coordinate and the coordinate system of the vessel.
	# ---rtzv--- is the heading of the vessel.
	
	
	##################################################
	##################################################
	# If 'cs.pos' has length>1, it specifies all of 'cs.pos', 'cs.view', in that order:
	if(length(cs.pos)>1){
		cs.view=cs.pos[2]
		cs.pos=cs.pos[1]
		}
	# Instruct the user to adjust the plotting volume, unless parameters to rgl.viewpoint() are given as the list 'view':
	if(length(view)==0 || identical(view[1],FALSE) || identical(view[1],"free") || identical(view[1],"f")){
		view=par3d()$userMatrix
		}
	else if(is.list(view)){
		warning("'view' cannot be a list. Use a vector of two elements c(theta=azimuth angle, phi=elevation angle)")
		view=par3d()$userMatrix
		}
	else if(!identical(as.double(dim(view)),c(4,4))){
		small_change=0.00001
		if(is.character(view[1])){
			view=substr(view[1],1,1)
			charview=c("t","b","s","w","n","e")
			if(!(view %in% charview)){
				warning(paste("Invalid character for 'view' (",view,"), set to \"free\". Must be one of \"t\" (top view), \"b\" (bottom view), \"s\" (view from south), \"w\" (view from west), \"n\" (view from north), \"e\" (view from east)",sep=""))
				return(par3d()$userMatrix)
				}
			
			thetaphi=cbind(c(0,0),c(0,180),c(0,90),c(-90,90),c(180,90),c(90,90))
			
			if(identical(cs.pos,"g") && identical(cs.view,"v")){
				thetaphi[1,]=thetaphi[1,]+rtzv*180/pi
				}
			else if(identical(cs.pos,"v") && identical(cs.view,"g")){
				thetaphi[1,]=thetaphi[1,]-rtzv*180/pi
				}
			theta=thetaphi[1,view==charview]
			phi=thetaphi[2,view==charview]
			}
		else if(is.numeric(view)){
			thetaphi=rep(view,length.out=2)
			theta=thetaphi[1]+90
			phi=thetaphi[2]
			if(identical(cs.pos,"g") && identical(cs.view,"v")){
				theta=theta+rtzv*180/pi
				}
			else if(identical(cs.pos,"v") && identical(cs.view,"g")){
				theta=theta-rtzv*180/pi
				}
			}
		theta=theta-small_change
		phi=phi-small_change
		view=rbind(cbind(matrix(rotx(phi,FALSE) %*% rotz(theta,FALSE),ncol=3,nrow=3),0),c(0,0,0,1))
		}
	
		
	##### Output #####
	view
	##################################################
	##################################################
	}
