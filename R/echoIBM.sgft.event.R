#*********************************************
#*********************************************
#' Calculates various segmentation success measures of segmented data versus theoretical segmented data.
#'
#' @param event  is the path to the directory holding the segmentation data.
#' @param event0  is the path to the directory holding the theoretical segmentation data.
#' @param pamkpar  is a list of parameters ('krange', 'criterion' and 'alpha') used pamk() when clustering the segmented voxels 'sgsc'. The voxels 'sgsc' are also ordered so that the voxels belonging to the largest cluster lead and the second to largest cluster follows.
#' @param ind  is a list of indexes, as typed into the [] of an array extracting a subset of the segmentation data.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR event.path list.files_caseInsensitive read.event sgPM
#' @importFrom TSD arr.ind2ind ind.expand ind2arr.ind NAs read.TSD write.TSD
#' @importFrom tools file_ext
#'
#' @export
#' @rdname echoIBM.sgft.event
#'
echoIBM.sgft.event<-function(event,event0,cruise=2009116,pamkpar=list(),ind=list(-(1:500),NULL),esnm="MS70",dir.data=NULL){
#echoIBM.sgft.event<-function(event,event0,cruise=2009116,segpar=list(sgt0=4.6e-1),pamkpar=list(),ind=list(-(1:500),NULL),esnm="MS70",dir.data=NULL){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-10-19 - Clean version.
	# Last: 2011-10-19 - Fixed bug by importing 'indt'.
	########### DESCRIPTION: ###########
	# Calculates various segmentation success measures of segmented data versus theoretical segmented data.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the path to the directory holding the segmentation data.
	# ---event0--- is the path to the directory holding the theoretical segmentation data.
	# <discontinued> ---segpar--- is a list of elements named "bwGp", "lsth"/"rlst", "usth"/"rust", or "sgth"/"sgt0" specifying the parameters of the segmentation data to read.
	# ---pamkpar--- is a list of parameters ('krange', 'criterion' and 'alpha') used pamk() when clustering the segmented voxels 'sgsc'. The voxels 'sgsc' are also ordered so that the voxels belonging to the largest cluster lead and the second to largest cluster follows.
	# ---ind--- is a list of indexes, as typed into the [] of an array extracting a subset of the segmentation data.
	
	
	##################################################
	##################################################
	##### Preparation #####
	intersect_sgsc=function(x,ind,dimb){
		x=ind2arr.ind(x,dimb)
		x=x[x[,1]%in%ind[[1]] & x[,2]%in%ind[[2]],]
		x=arr.ind2ind(x,dimb)
		}
	# Locate the event:
	event=event.path(event=event[1],cruise=cruise,esnm=esnm,dir.data=dir.data)$event
	
	
	b=read.event(event=event,var=c("numb","lenb"))
	dimb=c(max(b$lenb),b$numb)
	ind=ind.expand(ind,dimb)
	
	
	##### Execution #####
	# Get the theoretically segmented voxels, using the default value of 'sgt0' given in Paper III of the PhD of Holmin:
	default_sgt0=4.6e-1
	sgs0=read.event(event0,t="all",var="sgs0",segpar=list(sgt0=default_sgt0))$sgs0
	
	# Read the table of parameter values for the segmentation files:
	sgsc=read.event(event=event,t="all",var="sgsc",segpar=1,drop.out=FALSE)
	time=read.event(event=event,t="all",var=c("indt","utim"))
	
	sgPM=sgsc$sgPM
	numseg=nrow(sgPM)
	numt=length(time$indt)
	# Declare the vector of values for the fit of the segmentation:
	# The number of voxels CoRrectly segmented:
	sgcr=NAs(numseg,numt)
	# The union of the theoretically segmented voxels and the segmented voxels (ALl segmented voxels):
	sgal=NAs(numseg,numt)
	# The number of voxels TheoretiCally segmented:
	sgtc=NAs(numseg,numt)
	# The number of voxels segmented (EStimated):
	sges=NAs(numseg,numt)
	
	# The fraction of the theoretically segmented that are correctly segmented (sgcr/sgtc):
	sgfi=NAs(numseg,numt)
	# The fraction of the segmented voxels that are correctly segmented (sgcr/sges):
	sgfo=NAs(numseg,numt)
	# The measure of segmentation fit (sgfi * sgfo):
	sgft=NAs(numseg,numt)
	# The Jaccard measure of segmentation fit (sgcr / sgal):
	sgJC=NAs(numseg,numt)
	
	filelist=list.files_caseInsensitive(event,full.names=TRUE,recursive=TRUE)
	filelist=filelist[file_ext(filelist)=="seg"]
	
	for(i in seq_len(numseg)){
		cat("Reading segmentation file",i,"\n")
		#segpar=as.list(sgPM[i,c(1,4,5,6,7,8)])
		#names(segpar)=c("bwGp","lsth","usth","rlst","rust","sgth")
		sgsc=read.TSD(filelist[sgPM[i,"ORDR"]],t="all",var=c("indt","sgsc"),dimension=TRUE)
		indt=sgsc$indt
		sgsc=sgsc$sgsc
		#sgsc=read.event(event,t="all",var="sgsc",segpar=sgPM[i,"ORDR"],pamkpar=pamkpar,drop.out=FALSE)$sgsc
		if(length(sgsc)>0){
			if(!is.list(sgsc)){
				sgsc=list(sgsc)
				}
			sgsc=lapply(sgsc, intersect_sgsc, ind=ind, dimb=dimb)
			for(j in seq_along(indt)){
				sgcr[i,indt[j]]=length(intersect(sgs0[[indt[j]]],sgsc[[j]]))
				sgal[i,indt[j]]=length(union(sgs0[[indt[j]]],sgsc[[j]]))
				sgtc[i,indt[j]]=length(sgsc[[j]])
				sges[i,indt[j]]=length(sgs0[[indt[j]]])
				sgfi[i,indt[j]]=sgcr[i,indt[j]]/sgtc[i,indt[j]]
				sgfo[i,indt[j]]=sgcr[i,indt[j]]/sges[i,indt[j]]
				sgft[i,indt[j]]=sgfi[i,indt[j]]*sgfo[i,indt[j]]
				sgJC[i,indt[j]]=sgcr[i,indt[j]]/sgal[i,indt[j]]
				}
			}
		}
	
	
	##### Output #####
	out=list(INDT=time$indt,utim=time$utim,sgJC=sgJC,sgft=sgft,sgfi=sgfi,sgfo=sgfo,sgcr=sgcr,sgal=sgal,sgtc=sgtc,sges=sges,sgPM=sgPM)
	outfile=file.path(event.path(event=event[1],cruise=cruise,esnm=esnm,dir.data=dir.data)$event
,"SegmentationFit.tsd")
	write.TSD(out,con=outfile,dimension=TRUE,numt=1)
	out
	##################################################
	##################################################
	}
