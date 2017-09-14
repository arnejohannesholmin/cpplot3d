#*********************************************
#*********************************************
#' Creates a new event from segmentation files. The directory of the event must be created in advance, to ensure secure generation of the event.
#'
#' @param event  is the identifier of the event holding the segmentation files, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param pingsdir  is the path to the new event.
#' @param t  is the time points to process.
#' @param filesize  is the maximum size of the files.
#' @param type  is one of "sgsc", representing estimated segmentation data, and "sgs0", representing theoretical segmentation data.
#' @param segpar  is a list of elements named "bwGp", "lsth"/"rlst", "usth"/"rust", or "sgth"/"sgt0" specifying the parameters of the segmentation data to read.
#' @param ow  is TRUE if the user wish to overwrite existing files.
#' @param cruise  is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
#' @param esnm  is the name of the acoustical instrument, given as a four character string. Currently implemented are "EK60", "ME70", "MS70" and "SH80"/"SX80"/"SH90"/"SX90" (may be given in lover case)
#' @param dir.data  is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR event.path list.files_caseInsensitive read.event
#' @importFrom TSD read.TSD write.TSD zeros
#' @importFrom tools file_ext
#'
#' @export
#' @rdname echoIBM.sesg2event
#'
echoIBM.sesg2event<-function(event,pingsdir,t=1,filesize=3e8,type="scsg",segpar=NULL,ow=TRUE,cruise=2009116,esnm="MS70",dir.data=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-10-17 - Clean version.
	########### DESCRIPTION: ###########
	# Creates a new event from segmentation files. The directory of the event must be created in advance, to ensure secure generation of the event.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the identifier of the event holding the segmentation files, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# ---pingsdir--- is the path to the new event.
	# ---t--- is the time points to process.
	# ---filesize--- is the maximum size of the files.
	# ---type--- is one of "sgsc", representing estimated segmentation data, and "sgs0", representing theoretical segmentation data.
	# ---segpar--- is a list of elements named "bwGp", "lsth"/"rlst", "usth"/"rust", or "sgth"/"sgt0" specifying the parameters of the segmentation data to read.
	# ---ow--- is TRUE if the user wish to overwrite existing files.
	# ---cruise--- is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event to be read.
	# ---esnm--- is the name of the acoustical instrument, given as a four character string. Currently implemented are "EK60", "ME70", "MS70" and "SH80"/"SX80"/"SH90"/"SX90" (may be given in lover case)
	# ---dir.data--- is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
	
	
	##################################################
	##################################################
	########## Preparation ##########
	if(!type %in% c("sgsc","sgs0")){
		stop("The value of 'type' must be one of \"sgsc\" or \"sgs0\"")
		}
	
	# Locate the event:
	event=event.path(event=event,cruise=cruise,esnm=esnm,dir.data=dir.data)$event
	# Locate the new event:
	pingsdir=event.path(event=pingsdir,cruise=cruise,esnm=esnm,dir.data=dir.data)$event
	
	# Get the time indexes and the formated time strings corresponding to 't':
	time=read.event(event=event,cruise=cruise,var=c("indt","ftim","mtim"),t=t)
	t=time$indt
	
	# The list of files in the tsd-directory inside the case:
	filelist=list.files_caseInsensitive(event,full.names=TRUE,recursive=TRUE)
	# Selecting only files with valid extensions:
	ext=file_ext(filelist)
	beamsfilesind=which(ext=="beams")
	ctdfilesind=which(ext=="ctd")
	vesselfilesind=which(ext=="vessel")
	# Identify the original beams-file:
	# Read the variables of the beams files:
	beamsvars=vector("list",length(beamsfilesind))
	for(i in seq_along(beamsfilesind)){
		beamsvars[[i]]=read.TSD(filelist[beamsfilesind[i]],var="",header=TRUE)$labl
		}
	#beamsvar=c("esnm","numb","lenb","dirl","dirt","freq","absr","psze","sint","bwtl","bwtt","eqba","gain","tpow","sacr")
	beamsvar=c("esnm","numb","lenb","freq","absr","psze","sint","bwtl","bwtt","eqba","gain","tpow","sacr")
	beamsfile=filelist[beamsfilesind[sapply(beamsvars,function(x) all(beamsvar %in% x))]]
	if(length(beamsfile)==0){
		stop(paste("No original beams file located in the event",event))
		}
	else if(length(beamsfile)>1){
		warning(paste("Multiple beams file containing the original beams variables located in the event",event))
		}
	# Copy the beams file to the new directory:
	file.copy(beamsfile[1],pingsdir,overwrite=ow)
	# Identify the original ctd-file as the first ctd-file (normally only one ctd file should be given):
	ctdfile=filelist[ctdfilesind]
	if(length(ctdfile)>1){
		warning(paste("Multiple ctd-files found(",length(ctdfile),"). The first chosen"),sep="")
		}
	# Copy the ctd file to the new directory:
	file.copy(ctdfile[1],file.path(pingsdir,basename(ctdfile[1])),overwrite=ow)
	# Identify the original vessel-file as the first vessel-file (normally only one ctd file should be given):
	vesselfile=filelist[vesselfilesind]
	if(length(vesselfile)>1){
		warning(paste("Multiple vessel-files found(",length(vesselfile),"). The first chosen"),sep="")
		}
	# Copy the vessel file to the new directory:
	file.copy(vesselfile[1],file.path(pingsdir,basename(vesselfile[1])),overwrite=ow)
	
	
	########## Execution and output ##########
	# Read the original beams data for use in the pings files:
	beams=read.event(event=event,var="beams")
	
	eventname=rev(strsplit(event,"/",fixed=TRUE)[[1]])
	if(tolower(eventname[1])!="tsd"){
		echoIBM.warnings_warninglist=c(echoIBM.warnings_warninglist,"The path to the event specified by 'event' does not end with \"tsd\". Naming of the .pings files may be unexpected")
		}
	eventname=eventname[3]
	
	totallength=0
	newfile=TRUE
	for(i in t){
		cat("Writing pings-files for ping",i,"\n")
		# Read vessel data for ping i:
		vessel=read.event(event=event,var="vessel",t=i)
		# Read segmentation data for ping i:
		sgsc=read.event(event=event,var=type,t=i,segpar=segpar)[[type]]
		if(is.list(sgsc)){
			stop("Unique segmentation parameter values must be given through 'segpar'. See read.event() for explaination")
			}
		vbsc=zeros(max(beams$lenb),beams$numb)
		vbsc[sgsc]=1
		
		# Write ping i:
		x=c( list(indt=as.double(i),utim=time$utim[i],vbsc=vbsc),vessel, beams )
		# Assure that no duplicated elements names of 'x' are present when writing to file (Favouring the first of duplicatly named elements):
		x=x[!duplicated(names(x))]
		
		if(newfile){
			pingfile=paste(pingsdir,"/",eventname,"_",beams$esnm,"_T",paste(c(zeros(5-nchar(i)),i),collapse=""),".pings",sep="")
			# Write the data to file:
			bytes=write.TSD(con=pingfile,x=x,numt=1,ow=ow)
			}
		else{
			# Write (append) the data to file:
			bytes=write.TSD(con=pingfile,x=x,numt=1,append=TRUE,ow=ow)
			}
		totallength=totallength+bytes
		if(totallength>filesize){
			newfile=TRUE
			totallength=0
			}
		else{
			newfile=FALSE
			}		
		}
	##################################################
	##################################################
	}
