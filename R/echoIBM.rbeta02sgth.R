#*********************************************
#*********************************************
#' Returns segmentation threshold values 'c' corresponding to the input realtive lower schooling threshold values 'rbeta0'. The function only treats one time step, which implies that 'misM' must have length 1 if given (one school of one time step).
#'
#' @param rbeta0  is an array of relative lower schooling threshold values.
#' @param misM  is the mean sv of the school at the time step, given if the input is given in lower schooling threshold (not relative lower schooling threshold).
#' @param pow  is a value to raise the segmentation threshold values to the power of, which if pow<1 eases the strictness of the segmentations, suitable for bottom detection (pow=1/4 might be good):
#' @param table.out  is TRUE to return only the table.
#' @param ...  are parameters used in approx().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all read.TSD
#' @importFrom stats approx
#'
#' @export
#' @rdname echoIBM.rbeta02sgth
#'
echoIBM.rbeta02sgth<-function(rbeta0, misM=NULL, pow=1, table.out=FALSE, ...){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-10-22 - Clean version.
	########### DESCRIPTION: ###########
	# Returns segmentation threshold values 'c' corresponding to the input realtive lower schooling threshold values 'rbeta0'. The function only treats one time step, which implies that 'misM' must have length 1 if given (one school of one time step).
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---rbeta0--- is an array of relative lower schooling threshold values.
	# ---misM--- is the mean sv of the school at the time step, given if the input is given in lower schooling threshold (not relative lower schooling threshold).
	# ---pow--- is a value to raise the segmentation threshold values to the power of, which if pow<1 eases the strictness of the segmentations, suitable for bottom detection (pow=1/4 might be good):
	# ---table.out--- is TRUE to return only the table.
	# ---...--- are parameters used in approx().
	
	
	##################################################
	##################################################
	##### Preparation #####
	#echoIBM_datadir_ = file.path(echoIBM_frameworks, "R", "Functions", "echoIBM Main", "Utilities")
	#filebasename = "beta0-c-table.TSD"
	#file = file.path(echoIBM_datadir_, filebasename)
	#rbeta0_c_matrix = read.TSD(file)
	rbeta0_c_matrix = read.TSD(system.file("extdata", "beta0-c-table.TSD", package="echoIBM"))
	# Raise the segmentation threshold values to the power of 'pow', which if pow<1 eases the strictness of the segmentations, suitable for bottom detection (pow=1/4 might be good):
	if(pow!=1){
		rbeta0_c_matrix$b0cJ[,2] = rbeta0_c_matrix$b0cJ[,2]^pow
		}
	
	
	##### Execution and output #####
	if(table.out){
		rbeta0_c_matrix$b0cJ
		}
	else{
		dim_all(rbeta0)
		if(length(misM[1])==0){
			approx(rbeta0_c_matrix$b0cJ[,1:2], xout=rbeta0, ...)
			}
		else{
			approx(rbeta0_c_matrix$b0cJ[,1:2], xout=rbeta0/misM[1], ...)
			}
		}
	##################################################
	##################################################
	}
