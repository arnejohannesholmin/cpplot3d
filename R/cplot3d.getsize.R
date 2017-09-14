#*********************************************
#*********************************************
#' Returns a function of one signel numeric, returning the sizes used when plotting voxels by cplot3d.TSD(). The input to the retunred function is the number of breaks of the color plot. The inputs to cplot3d.size() are the type of size function, the range of the size values and the parameters of the size function.
#'
#' @param size Either a numeric, function, or character representing a pre-defined function (such as SU90 or MS70).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR sonR_implemented
#'
#' @export
#' @rdname cplot3d.getsize
#'
cplot3d.getsize<-function(size){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-01-06 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a function of one signel numeric, returning the sizes used when plotting voxels by cplot3d.TSD(). The input to the retunred function is the number of breaks of the color plot. The inputs to cplot3d.size() are the type of size function, the range of the size values and the parameters of the size function.
	########## DEPENDENCIES: ###########
	# setrange()
	############ VARIABLES: ############
	# ---size--- is a three character string representing the type of function of the size. Current valid values are 
	
	
	##################################################
	##################################################
	if(is.numeric(size) || is.function(size)){
		size
		}
	else if(is.character(size)){
		# Allow for sonar and echosounder presets:
		if(sonR_implemented(size, c("MBS"))){
			size = list(type="pow", y=20, par=4)
			}
		else if(sonR_implemented(size, c("MBS","OFS"))){
			size = list(type="pow", y=20, par=1.5)
			}
		do.call("cplot3d.size", size)
		}
	else if(is.list(size)){
		do.call("cplot3d.size", size)
		}
	else{
		warning("Wrong value of 'size'. Should be a numeric, function, list or character.")
		}
	##################################################
	##################################################
	}
