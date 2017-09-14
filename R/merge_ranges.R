#*********************************************
#*********************************************
#' Finds new ranges from matrices of ranges along the columns.
#'
#' @param ...  are matrices of range values along the columns. The matrices must have dimension [2 x p], where p is the number of ranges.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname merge_ranges
#'
merge_ranges<-function(...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-09-30 - Clean version.
	########### DESCRIPTION: ###########
	# Finds new ranges from matrices of ranges along the columns.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---...--- are matrices of range values along the columns. The matrices must have dimension [2 x p], where p is the number of ranges.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	ll=list(...)
	dimll=lapply(ll,dim)
	nrowll=sapply(ll,nrow)
	ncolll=sapply(ll,nrow)
	lengthll=sapply(ll,length)
	if(all(sapply(dimll,is.null)) && all(lengthll==2)){
		ll=lapply(ll,function(x) array(x[1:2],dim=c(2,1)))
		}
	if(any(sapply(dimll,length)>2)){
		stop("Only matrices accepted")
		}
	if(!all(ncolll==ncolll[1])){
		stop("Matrices must have the same number of columns")
		}
	if(!all(nrowll==2)){
		stop("Matrices must have ranges along the columns, and must thus have two rows")
		}
	

	########## Execution and output ##########
	ranges=ll[[1]]
	if(length(ll)>1){
		for(i in seq_len(length(ll)-1)+1){
			ranges=matrix(c(min(ranges[1,1],ll[[i]][1,1],na.rm=TRUE), max(ranges[2,1],ll[[i]][2,1],na.rm=TRUE), min(ranges[1,2],ll[[i]][1,2],na.rm=TRUE), max(ranges[2,2],ll[[i]][2,2],na.rm=TRUE), min(ranges[1,3],ll[[i]][1,3],na.rm=TRUE), max(ranges[2,3],ll[[i]][2,3],na.rm=TRUE)),2,3)
			}
		}
	ranges
	##################################################
	##################################################
	}
