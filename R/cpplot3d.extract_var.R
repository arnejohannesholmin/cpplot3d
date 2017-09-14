#*********************************************
#*********************************************
#' Extracts the variable (acoustic or segmentation) specified by 'var' from the 'data'. Only one time step must be given in 'data'.
#'
#' @param data  is a list containing the data to plot, as returned from read.event(...,var=c("vbsc","voxels","ctd","beams","vessel","time")). May include school positions data$psxf, data$psyf and data$pszf. If non-school voxel probabilities are given in an array 'pr0s', or thresholded segmentation values 'sgsc'/'sgs0' are given, these can be plotted using the input 'var'.
#' @param var  is a string or a vector for two strings, specifying the variable to extract. If var[2] starts with "+" or "-", and the rest of the string is the four character name of a segmentation variable, the variable specified by var[1] is extracted but only ("+") or everything but ("-") the data in the segmentation mask. Adding a "-" at the end of var[2], so that this has 6 characters expands the subtraction to everythinb past the first segmentation index of the beam.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD extractIndSubset ind2arr.ind labl.TSD strff zeros
#'
#' @export
#' @rdname cpplot3d.extract_var
#'
cpplot3d.extract_var <- function(data, var){
			
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-09-06 - Clean version.
	# Last: 2013-09-30 - Added the option of discarding all voxels past and including the first voxel included in the segmentation mask for each beam, (demands 10 % more time).
	########### DESCRIPTION: ###########
	# Extracts the variable (acoustic or segmentation) specified by 'var' from the 'data'. Only one time step must be given in 'data'.
	########## DEPENDENCIES: ###########
	# labl.TSD(), extractIndSubset()
	############ VARIABLES: ############
	##########################
	##### Main variable: #####
	##########################
	# ---data--- is a list containing the data to plot, as returned from read.event(...,var=c("vbsc","voxels","ctd","beams","vessel","time")). May include school positions data$psxf, data$psyf and data$pszf. If non-school voxel probabilities are given in an array 'pr0s', or thresholded segmentation values 'sgsc'/'sgs0' are given, these can be plotted using the input 'var'.
	# ---var--- is a string or a vector for two strings, specifying the variable to extract. If var[2] starts with "+" or "-", and the rest of the string is the four character name of a segmentation variable, only ("+") or everything but ("-") the data in the segmentation mask  is extracted from the variable specified by var[1].
	
		
	##################################################
	##################################################
	##### Preparation and execution #####
	# Function to expand to all indices along beams up to the first voxel included in the segmentation mask:
	#expandToStart <- function(s, lenb, numb){
	#	s = ind2arr.ind(s, c(max(data$lenb),data$numb))
	#	s = by(s[,1], s[,2], min)
	#	unlist(lapply(s, seq_len)) + rep((as.numeric(names(s))-1)*max(data$lenb),c(s))
	#	}
	expandToStart <- function(s, lenb, numb){
		s = ind2arr.ind(s, c(lenb, numb))
		s = by(s[,1], s[,2], min)
		unlist(lapply(s, seq_len)) + rep((as.numeric(names(s))-1)*lenb, c(s))
		}
	# Plot segmentation data if reuqired:
	if((var[1] %in% labl.TSD("sd")) && all(length(data[[var[1]]])>0, length(data$lenb)>0, length(data$numb)>0)){
		data$vbsc = zeros(max(data$lenb), data$numb)
		if(is.list(data[[var[1]]])){
			warning("Segmentation data given for multiple parameter settings. The first used")
			data$vbsc[data[[var[1]]][[1]]] = 1
			if(length(data$psis)>0){
				data$vbsc[data[[var[1]]][[1]]] = data$psis[[1]]
				}
			}
		else{
			data$vbsc[data[[var[1]]]] = 1
			if(length(data$psis)>0){
				data$vbsc[data[[var[1]]]] = data$psis
				}
			}
		}
	else if((var[1] %in% labl.TSD("sg")) && length(data[[var[1]]])==0){
		warning(paste("Segmentation data", data[[var[1]]], "non-existent. Volume backscattering coefficient 'vbsc' used"))
		}
	if(var[1]=="pr0s" && length(data$pr0s)>0){
		data$vbsc = data$pr0s
		}
	else if(var[1]=="pr0s" && length(data$pr0s)==0){
		warning("Probability data 'pr0s' non-existent. Volume backscattering coefficient 'vbsc' used")
		}
	# Plot noise if required:
	if(var[1]=="tlns"){
		if(length(data$tlns)==0){
			warning("Noise not specified. Use 'bgns', 'pdns', 'nrns', and 'hins' to specify the noise. Volume backscattering data plotted")
			}
		else{
			data$vbsc = data$tlns
			}
		}
	
	# If there is a second string in 'var' starting with "+" or "-", plot only ("+") or everything but ("-") the data in the segmentation mask given by the rest of the string:
	pm = c("+","-")
	if(length(var)>1 && any(pm %in% substr(var[2],1,1)) && substr(var[2],2,5) %in% labl.TSD("sd")){
		segname = substr(var[2],2,5)
		thispm = substr(var[2],1,1)
		if(strff("-", thispm)){
			return.all=TRUE
			# If there is an additional "-" at the end of var[2], remove all voxels past the first voxel included in the segmentation mask for each beam:
			if(is.list(data[[segname]])){
				# Get the non-empty elements:
				nonempty = sapply(data[[segname]],length)>0
				if(any(nonempty)){
					if(nchar(var[2])>5 && substr(var[2],6,6)=="-"){
						# Convert to array indices:
						data[[segname]][nonempty] = lapply(data[[segname]][nonempty], expandToStart, lenb=max(data$lenb), numb=data$numb[1])
						}
					else{
						data[[segname]] = lapply(data[[segname]],function(x) if(length(x)>0) -x)
						}
					}
				}
			else{
				if(length(data[[segname]])>0){
					if(nchar(var[2])>5 && substr(var[2],6,6)=="-"){
						data[[segname]] = expandToStart(data[[segname]], lenb=max(data$lenb), numb=data$numb[1])
						}
					else{
						data[[segname]] = -data[[segname]]
						}
					}
				}
			}
		else{
			return.all=FALSE
			}
			
		data$vbsc = extractIndSubset(data$vbsc,subset=data[[segname]],insert.NA=TRUE,return.all=return.all)
		}
	
		
	##### Output #####
	data$vbsc
	##################################################
	##################################################
	}
