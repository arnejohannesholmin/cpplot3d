#*********************************************
#*********************************************
#' Locates the path of an event specified by numeric or sting for both 'event' and 'cruise' (used in read.event()), and generates segmentation file names continuing from the extisting numbered segmentation files.
#'
#' @param n  is an integer giving the sequence number to append to the file name. If n = 3 and there are 4 segmentation files present, the numbers 5, 6, and 7 are added to the file names.
#' @param t  is the time step index to append to the file name.
#' @param nchart  is the number of characters of the time point information, defaulted to the maximum number of characters of 't'.
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param cruise  is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' @param esnm  is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#' @param dir.data  is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#' @param add  is a string to add to the file name.
#' @param startn  is the start number of the files.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR event.path
#' @importFrom TSD zeropad
#' @importFrom tools file_ext
#'
#' @export
#' @rdname echoIBM.get.segfilename
#'
echoIBM.get.segfilename<-function(n=1, t=0, nchart=NULL, event=1, cruise=2009116, esnm="MS70", dir.data=NULL, add="", startn=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-07-07 - Clean version.
	# Last: 2013-10-22 - Major change, from specifying parameter names and values in the file names, to simply adding a numbering index in the file names.

	
	##################################################
	##################################################
	########## Preparation ##########
	# Get the directory to which to write the files:
	dir.out = event.path(event=event, cruise=cruise, esnm=esnm, dir.data=dir.data)
	
	### THIS DID NOT MAKE SENSE: ###
	# If more than one segmentation threshold value is given, return one file name for each value:
	#thresholds = grep("sgt", substr(names(par), 1, 3))
	#if(length(thresholds)>0){
	#	nfiles = length(par[[thresholds[1]]])
	#	}
	#else{
	#	nfiles = 1
	#	}
	
	
	########## Execution ##########
	# Read the existing segmentation file names, and continue in the numbered list:
	filelist = list.files(dir.out$event, recursive = TRUE)
	ext = tolower(file_ext(filelist))
	segfiles = filelist[ext == "seg"]
	if(length(startn)>0){
		thesesfnr = seq_len(n) + startn - 1
		}
	else{
		thesesfnr = seq_len(n) + length(segfiles)
		}
	
	sfnr = zeropad(thesesfnr, 4)
	
	
	########## Output ##########
	# Zeropad 't':
	if(is.numeric(t)){
		#t = zeropad(t, n = if(length(nchart) =  = 0) max(nchar(t)) else nchart)
		t = zeropad(t, n = max(nchar(t)))
		}
	list(sfnm = file.path(dir.out[1], paste(dir.out[2], "_T", t[1], if(nchar(add)>0) "_", add, "_sfnr_", sfnr, ".seg", sep = "")), sfnr = as.numeric(sfnr))
	##################################################
	##################################################
	}
