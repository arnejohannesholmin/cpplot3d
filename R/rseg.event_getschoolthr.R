#*********************************************
#*********************************************
#' Function for converting the school threshold into a list of functions/single values.
#'
#' @param schoolthr  is a function of size and mean volume backscattering strength (S_V) defining the threshold level below the 90-percentile of the school using the initial above-noise-threshold. Can also be is a vector of values, where one segmentation file is written for each value of 'schoolthr'. 
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname rseg.event_getschoolthr
#'
rseg.event_getschoolthr<-function(schoolthr=function(S, SBR) 4.4 + 0.013*S + 0.1*SBR){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-06-04 - Extracted from rseg.event().
	########### DESCRIPTION: ###########
	# Function for converting the school threshold into a list of functions/single values.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---schoolthr--- is a function of size and mean volume backscattering strength (S_V) defining the threshold level below the 90-percentile of the school using the initial above-noise-threshold. Can also be is a vector of values, where one segmentation file is written for each value of 'schoolthr'. 
	
	
	##################################################
	##################################################
	##### Preparation #####
	# If the length of 'schoolthr' is zero, apply the default function found by Pena, Holmin and Ona in october 2014:
	if(length(schoolthr)>0){
		# If 'schoolthr' is not a function, create a list:
		if(is.function(schoolthr)){
			schoolthr=list(schoolthr)
			}			
		else if(!is.list(schoolthr)){
			# If the length of 'schoolthr' is 1, create a function that returns the value:
			if(length(schoolthr)==1){
				schoolthr=list(schoolthr)
				}
			else{
				schoolthr=as.list(schoolthr)
				}
			}
		# Return:
		schoolthr
		}
	else{
		# Return  default function:
		#list(function(S) 2.444 + 0.0212*S)
		#list(function(S, SBR) 2.57 - 32.0/S + 0.526*SBR)
		list()
		}
	##################################################
	##################################################
	}
