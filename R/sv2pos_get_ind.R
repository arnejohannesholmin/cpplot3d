#*********************************************
#*********************************************
#' Extracts a subset of TSD data according to the array subset 'ind' and/or the cartesian subset 'range' and/or the logical/numeric vector of subscripts 'subset'. 
#'
#' @param data  is a list of elements named according to the TSD file format.
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range  is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep (as used in subset()). Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param strict  is TRUE if strict inequality is to be used when subsetting according to 'range'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD arr.ind2ind dim_all ind.expand
#'
#' @export
#' @rdname sv2pos_get_ind
#'
sv2pos_get_ind<-function(data,ind=list(),range=list(),subset=NULL,strict=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-03-23 - Clean version.
	# Last: 2010-08-08 - Added support for 'range' of acoustic data.
	########### DESCRIPTION: ###########
	# Extracts a subset of TSD data according to the array subset 'ind' and/or the cartesian subset 'range' and/or the logical/numeric vector of subscripts 'subset'. 
	########## DEPENDENCIES: ###########
	# extract()
	############ VARIABLES: ############
	# ---data--- is a list of elements named according to the TSD file format.
	# ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
	# ---range--- is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
	# ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep (as used in subset()). Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
	# ---strict--- is TRUE if strict inequality is to be used when subsetting according to 'range'.
		
	
	##################################################
	##################################################
	##### Preparation #####
	# Do nothing if 'ind', 'range' and 'subset' are not specified:
	if(all(length(ind)==0,length(range)==0,length(subset)==0)){
		dimx=dim_all(data$vbsc)
		ind=ind.expand(ind,dimx)
		ind=arr.ind2ind(ind,dimx)
		return(ind)
		}
	
	# 'strict' is used when subsetting according to 'range':
	if(strict){
		ineq="<"
		}
	else{
		ineq="<="
		}
	
	# Input 'data' needs to be a list:
	if(!is.list(data)){
		stop("Input 'data' must be a list of elements named according to the TSD file format")
		}
	
	# Input 'ind' needs to be a list:
	if(!is.list(ind)){
		ind=list(ind)
		}
	# If the length of 'ind' is 1, a warning is issued:
	if(length(ind)==1 && any(sapply(data,function(x) length(dim_all(x)))>1)){
		warning("'ind' contains only one vector. All arrays of one or more dimensions will be subsetted")
		}
	
		
	##### Execution #####
	### 1 ###
	# If 'subset' is not given:
	if((identical(subset,0) || length(subset)==0)){
		dimx=dim_all(data$vbsc)
		ind=ind.expand(ind,dimx)
		ind=arr.ind2ind(ind,dimx)
		}
		
	### 2 ###
	# If only 'subset' is given:
	else if(length(ind)==0 && !(identical(subset,0) || length(subset)==0)){
		if(is.logical(subset)){
			ind=seq_along(data[[1]])[subset]
			}
		else{
			ind=subset[1<subset & subset<length(data[[1]])]
			}
		}
	
	### 3 ###
	# If both 'ind' and 'subset' are given, transform 'ind' to indexes, and combine with 'subset':
	else if(length(ind)>0 && !(identical(subset,0) || length(subset)==0)){
		if(is.logical(subset)){
			subset=seq_along(data[[1]])[subset]
			}
		else{
			subset=subset[1<subset & subset<length(data[[1]])]
			}
		
		dimx=dim_all(data$vbsc)
		ind=ind.expand(ind,dimx)
		ind=arr.ind2ind(ind,dimx)
		# Intersect the indexes of 'ind' and 'subset':
		if(!(identical(subset,0) || length(subset)==0)){
			ind=intersect(ind,subset)
			}
		}
			
	### The following code is copied from extract.range.TSD() and modified to only affect the indeces 'ind': ###
	# Get the names of 'range' and separate out the names concerinig voxels ('namesposx'):
	# If the mean volume backscattering strength (Sv) is to be subsetted, transform to volume backscattering coefficient (linear, sv):
	if("mvbs" %in% names(range)){
		range$vbsc=10^(range$mvbs/10)
		range$mvbs=NULL
		}
	namesrange=intersect(names(range),names(data))
	namesposx=intersect(namesrange,c("psxx","psyx","pszx"))
	namesacoustic=intersect(namesrange,c("vbsc"))
		
	# Positions of the voxels, simultaneously subsetted:
	if(length(namesposx)>0){
		thisind=TRUE
		for(i in seq_along(namesposx)){
			thisind=thisind & do.call(ineq,list(min(range[[i]]),data[[namesposx[i]]][ind])) & do.call(ineq,list(data[[namesposx[i]]][ind],max(range[[i]])))
			}
		ind=ind[thisind]
		}	
	# Acoustic data, simultaneously subsetted along with positions of the voxels:
	if(length(namesacoustic)>0){
		thisind=TRUE
		# Only one step in the for loop, but keeping the same structure as the above for loops for convenience:
		for(i in seq_along(namesacoustic)){
			thisind=thisind & do.call(ineq,list(min(range[[i]]),data[[namesacoustic[i]]][ind])) & do.call(ineq,list(data[[namesacoustic[i]]][ind],max(range[[i]])))
			}
		ind=ind[thisind]
		}	
	
	
	##### Output #####
	ind
	##################################################
	##################################################
	}
