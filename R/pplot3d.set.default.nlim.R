#*********************************************
#*********************************************
#' Sets the default 'nlim' used in pplot3d-functions.
#'
#' @param nlim  is the maximum number of points in a voxel, before the number of points is recalculated to n*fact.
#' @param var  is a string specifying the variable to plot. Currently supported are "vbsc", for volume backscattering data, "sgsc"/"sgs0"/"sgsE", for thresholded segmentation data, "pr0s" for unhtresholded not-school probability data, and "tlns" for pure estimated noise.
#' @param esnm  is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR is.sonar
#' @importFrom TSD labl.TSD
#'
#' @export
#' @rdname pplot3d.set.default.nlim
#'
pplot3d.set.default.nlim<-function(nlim=NULL,var=c("vbsc","sgsc","pr0s","sgs0","sgsE","tlns"),esnm="MS70"){
			
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-05-09 - Clean version.

	
	##################################################
	##################################################
	if(length(nlim)==0){
		# If segmentation data are used, set nlim to Inf:
		if(var[1] %in% labl.TSD(var="sg")){
			Inf
			}
		else if(!is.sonar(esnm)){
			30^(1:5)
			}
		else{
		 	50^(1:4)
		 	}
		}
	else if(is.infinite(nlim) || identical(nlim,0)){
		nlim=Inf
		}
	else{
		nlim
		}
	##################################################
	##################################################
	}
