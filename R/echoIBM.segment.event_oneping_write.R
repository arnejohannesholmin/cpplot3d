#*********************************************
#*********************************************
#' Writes one ping of segmented data from the Bayesian segmentation method.
#'
#' @param data  is the list of data including segmentation and acoustic data.
#' @param beams  is a list of beam configuration data.
#' @param time  is a list of time information.
#' @param sgsc  is unbiased segmentatino mask.
#' @param sgsE  is enlarged segmentatino mask.
#' @param segfile  is the segmentation file(s).
#' @param ii  is the current time step index.
#' @param thisi  is the current time step.
#' @param thisa  is the index of current alpha value.
#' @param minsgsc  is the minimum number of segmented voxels required by meanSv.TSD().
#' @param newfile  is locigal deciding wheter to create a new file due to the 'filesize' limit.
#' @param totallength  is a vector of the sizes of the files.
#' @param Xcsz  is the maximum file size.
#' @param pr0s.out  is TRUE if the pobability of beta<beta0 should be returned.
#' @param dir.out  is the directory in which to put the segmentaiton files.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR cm.school meanSv.TSD
#' @importFrom TSD strff write.TSD
#'
#' @export
#' @rdname echoIBM.segment.event_oneping_write
#'
echoIBM.segment.event_oneping_write<-function(data, beams, time, sgsc, sgsE, ii, thisi, block, thisa, alpha, minsgsc, newfile, segfile, totallength, filesize, pr0s.out, dir.out, var, thisbeta0, thisbeta1, beta0, beta1, lst0, input_misM, misM, h, pow, startn, add, numt, factor, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-06-04 - Extracted from echoIBM.segment.event().
	########### DESCRIPTION: ###########
	# Writes one ping of segmented data from the Bayesian segmentation method.
	########## DEPENDENCIES: ###########
	# cm.school(), write.TSD()
	############ VARIABLES: ############
	# ---data--- is the list of data including segmentation and acoustic data.
	# ---beams--- is a list of beam configuration data.
	# ---time--- is a list of time information.
	# ---sgsc--- is unbiased segmentatino mask.
	# ---sgsE--- is enlarged segmentatino mask.
	# ---segfile--- is the segmentation file(s).
	# ---ii--- is the current time step index.
	# ---thisi--- is the current time step.
	# ---thisa--- is the index of current alpha value.
	# ---minsgsc--- is the minimum number of segmented voxels required by meanSv.TSD().
	# ---newfile--- is locigal deciding wheter to create a new file due to the 'filesize' limit.
	# ---totallength--- is a vector of the sizes of the files.
	# ---Xcsz--- is the maximum file size.
	# ---pr0s.out--- is TRUE if the pobability of beta<beta0 should be returned.
	# ---dir.out--- is the directory in which to put the segmentaiton files.
	
	##################################################
	##################################################
	##### Preparation #####
	# Get the indices of the segmented voxels for which the lower schooling threshold was set to the background noise:
	lst0 = intersect(sgsc[[ii]], data$lst0[[ii]])
	
	### General variables, unbiased segmentation mask: ###
	thisadd = prod(dim(data$vbsc)[1:2])*(ii-1)
	# Total volume unbiased:
	Htvl = sum(data$volx[sgsc[[ii]]], na.rm=TRUE)
	# Total backscattering cross section unbiased:
	Htbs = sum(data$vbsc[sgsc[[ii]]+thisadd]*data$volx[sgsc[[ii]]], na.rm=TRUE)
	# Total target strength unbiased:
	HtTS = 10*log10(Htbs)
	
	### General variables, enlarged segmentation mask: ###
	# Total volume enlarged:
	HEvl = sum(data$volx[sgsE[[ii]]], na.rm=TRUE)
	# Total backscattering cross section enlarged:
	HEbs = sum(data$vbsc[sgsE[[ii]]+thisadd]*data$volx[sgsE[[ii]]], na.rm=TRUE)
	# Total target strength enlarged:
	HETS = 10*log10(HEbs)
	
	### Derived variables: ###
	data$vbss = data$vbsc[sgsc[[ii]]+thisadd]
	meanSv = meanSv.TSD(data, list.out=TRUE, minsgsc=minsgsc, ...)
	data$vbss = data$vbsc[sgsE[[ii]]+thisadd]
	EmeanSv = meanSv.TSD(data, list.out=TRUE, minsgsc=minsgsc, enlarged=TRUE, ...)
		
	
	# Mean volume backscattering coefficient:
	Hmsv = HEbs/Htvl
	# Mean volume backscattering strength:
	HmSv = 10*log10(Hmsv)
	
	
	### Position variables: ###
	# Center of acoustic backscatter:
	Hcax = suppressWarnings(cm.school(list(vbsc=data$vbsc[sgsc[[ii]]+thisadd], psxx=data$psxx[sgsc[[ii]]+thisadd], psyx=data$psyx[sgsc[[ii]]+thisadd], pszx=data$pszx[sgsc[[ii]]+thisadd], volx=data$volx[sgsc[[ii]]])))
	Hcay = Hcax[2]
	Hcaz = Hcax[3]
	Hcax = Hcax[1]
	
	# Center of mass:
	Hcmx = suppressWarnings(cm.school(list(vbsc=1, psxx=data$psxx[sgsc[[ii]]+thisadd], psyx=data$psyx[sgsc[[ii]]+thisadd], pszx=data$pszx[sgsc[[ii]]+thisadd], volx=data$volx[sgsc[[ii]]])))
	Hcmy = Hcmx[2]
	Hcmz = Hcmx[3]
	Hcmx = Hcmx[1]
	
	# The horizontal range to the school from the sonar:
	Hhra = sqrt((Hcmx-data$psxv)^2+(Hcmy-data$psyv)^2)
	
	# Define output lists:
	x_time = list(indt=time$indt[thisi], mtim=time$mtim[thisi])
	x_seg = c(list(sgsc=sgsc[[ii]], sgsE=sgsE[[ii]], pr0H=data$pr0s[,,ii][sgsc[[ii]]], if(length(factor)>1) pr0E=data$pr0s[,,ii+length(block)][sgsE[[ii]]], Htvl=Htvl, Htbs=Htbs, HtTS=HtTS, HEvl=HEvl, HEbs=HEbs, HETS=HETS, Hmsv=Hmsv, HmSv=HmSv, Hcmx=Hcmx, Hcmy=Hcmy, Hcmz=Hcmz, Hcax=Hcax, Hcay=Hcay, Hcaz=Hcaz, Hhra=Hhra), meanSv, EmeanSv)
	if(pr0s.out){
		x_seg$pr0s = data$pr0s[,,ii]
		}
	if(strff("s", var[1])){
		names(x_seg)[1:2] = c("sgbt", "sgbE")
		}
	x_par_lsth = list(lsth=thisbeta0[ii], usth=thisbeta1[ii], lst0=lst0)
	# Variables dependent on 'misM' (default: length(misM)>0):
	if(length(misM)>0){
		x_par_rlst = list(rlst=beta0[thisi], rust=beta1[thisi], misM=input_misM[thisi], iisM=misM[thisi])
		}
	else{
		x_par_rlst = list(rlst=NULL, rust=NULL, misM=NULL, iisM=NULL)
		}
	# Variables dependent on 'newfile' (default: newfile = TRUE):
	x_par_sgth = list(bwGp=h)
	if(is.integer(alpha)){
		x_par_bwGp = list(sgth=echoIBM.rbeta02sgth(table.out=TRUE, pow=pow)[, 1:2])
		}
	else{
		x_par_bwGp = list(sgth=alpha[thisa])
		}
	x_beams = beams[c("numb", "lenb", "freq")]
	
	
	##### Execution #####
	# Write the data to file:
	if(newfile[thisa]){
		segfile = echoIBM.get.segfilename(event=dir.out[1], add=paste(add, time$indt[thisi], sep="_"), startn=startn)
		sfnr = segfile[[2]]
		segfile = segfile[[1]]
		x = c(x_time, x_seg, x_par_lsth, x_par_rlst, x_par_sgth, x_par_bwGp, x_beams, segfile[2])
		suppressWarnings( bytes<-write.TSD(con=segfile[[1]][thisa], x=x, numt=1, reserve=numt-1, keep.null=TRUE, ...) )
		}
	# Append the data to file:
	else{
		# Redefine the variables depending on 'newfile':
		x_par_sgth = list(bwGp=NULL)
		x_par_bwGp = list(sgth=NULL)
		x_beams = list(numb=NULL, lenb=NULL, freq=NULL)
		# Write (append) the data to file:
		x = c(x_time, x_seg, x_par_lsth, x_par_rlst, x_par_sgth, x_par_bwGp, x_beams)
		suppressWarnings( bytes<-write.TSD(con=segfile[thisa], x=x, numt=1, append=TRUE, keep.null=TRUE, ...) )
		}
	totallength[thisa] = totallength[thisa]+bytes
	if(totallength[thisa]>filesize){
		newfile[thisa] = TRUE
		totallength[thisa] = 0
		}
	else{
		newfile[thisa] = FALSE
		} # End of if(totallength[thisa]>filesize)
	
	
	##### Output #####
	list(newfile=newfile, totallength=totallength, segfile=segfile)
	##################################################
	##################################################
	}
