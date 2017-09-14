#*********************************************
#*********************************************
#' Segments one ping with the (SX90) unbiased segmentatino method. Used in rseg.event().
#'
#' @param t_seq  is the time step index. One of 1:t.
#' @param t  is the time steps to be segmented.
#' @param utim  is a a vector of UNIX time points corresponding to 't'. 
#' @param noisethr  is the input dB above background threshold value. 
#' @param segfilesdir  is the directory of the segmentation files to be written.
#' @param sfnr  is the segmentatino file number.
#' @param thersholdFromSchool  is the (unbiased) threshold values defined from the singal.
#' @param dBb1, dBb2, dBb3, dBb4, dBbs  are the dB belov the signal, where the first four are parameters in the function f(S, SBR, Sv) with dBb1 being the constant term.
#' @param nseg1, nseg2  are the number of segmentations using backgroun-up and signal-down, respectively.
#' @param Xcsz  is the circular size of the schools, calucated as 2*sqrt(A/pi).
#' @param kern  is the standard deviation in units of the number of voxels used in a Gaussian kernel smoother along the beams (no smoothing if kern == 0 or length(kern) == 0, which is the default). If given as an integer, say 5L, median smoothing is applied instead of Gaussian.
#' @param save.trh  is TRUE to save the threshold values used in the segmentation.
#' @param save.data  is TRUE to save the data included in the segmentation mask.
#' @param wt  is the number of time steps used as the width of the median filter along time steps.
#' @param type  is the function to use across beams (one of "mean" and "median", where "mean" is used in PROFUS up to 2014, while "median" is more robust to the valus of the school).
#' @param esnm  is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#' @param cmpred  is a three column matrix holding the centers of mass of the ellipsoid outside which no samples are discarded from the segmentation mask. This matrix must have one row per time step, starting at the first time step.
#' @param ellipsoid  is an optional vector of up to three elements specifying the semi axes of an ellipsoid centered at the center of mass of a user selected region, outside which voxels will not be included in the segmentation. The ellispoid can be set to move with the center of mass of the segmented voxels.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event subset_TSD
#' @importFrom TSD arr.ind2ind ind.expand prettyIntegers
#' @importFrom utils tail
#'
#' @export
#' @rdname rseg.event_oneping
#'
rseg.event_block <- function(t_seq, t, utim, noise, noisethr, thr, segfilesdir, sfnr, thersholdFromSchool, dBb1, dBb2, dBb3, dBb4, dBbs, nseg1, nseg2, Xcsz, add, event, kern, type, noisesmooth, ind, range, beamend, save.trh, save.data, save.ind, wt, esnm, cmpred, avoid, ellipsoid, nlmp, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-06-04 - Extracted from rseg.event().
	# Last: 2015-10-05 - Added the function rseg.estimate.noise().
	########### DESCRIPTION: ###########
	# Segments one ping with the (SX90) unbiased segmentatino method. Used in rseg.event().
	########## DEPENDENCIES: ###########
	# cm.school(), write.TSD()
	############ VARIABLES: ############
	# ---t_seq--- is the time step index. One of 1:t.
	# ---t--- is the time steps to be segmented.
	# ---utim--- is a a vector of UNIX time points corresponding to 't'. 
	# ---noisethr--- is the input dB above background threshold value. 
	# ---segfilesdir--- is the directory of the segmentation files to be written.
	# ---sfnr--- is the segmentatino file number.
	# ---thersholdFromSchool--- is the (unbiased) threshold values defined from the singal.
	# ---dBb1, dBb2, dBb3, dBb4, dBbs--- are the dB belov the signal, where the first four are parameters in the function f(S, SBR, Sv) with dBb1 being the constant term.
	# ---nseg1, nseg2--- are the number of segmentations using backgroun-up and signal-down, respectively.
	# ---Xcsz--- is the circular size of the schools, calucated as 2*sqrt(A/pi).
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Estimate the background (noise), median filtering over the specified number of pings (2*add + 1, defaulted to 5), and also for only one ping, used when subtracting the background in the biomass estimate:
	# If 'thr' is given, indicating a fixed threshold, the noise is input as NA (see rseg.event()), saving time and setting the noise as missing:
	noiseANDvbsc = rseg.estimate.noise(event, t_seq=t_seq, add=add, t=t, kern=kern, type=type, noisesmooth=noisesmooth, noise=noise, ind=ind)
	# The fixed threshold gan be given as a vector of one element per range or a list of one element per beam (both are expanded so that the last element is repeated with a warning). Here 'nbeams' and 'nranges' are defined for simplicity and not used futher in the function:
	if(length(thr)){
		nbeams = ncol(noiseANDvbsc$vbscmed)
		nranges = nrow(noiseANDvbsc$vbscmed)
		if(is.list(thr)){
			thr = unlist(thr)
			if(length(thr)<nbeams){
				warning(paste0("the last threshold ", tail(thr,1), " repeated for beam(s) ", prettyIntegers(seq(length(thr)+1, nbeams))))
				thr = c(thr, rep(tail(thr,1), nbeams-length(thr)))
				}
			thr = matrix(thr, nrow=nranges, ncol=nbeams, byrow=TRUE)
			}
		else if(length(thr)>1){
			if(length(thr)<nranges){
				warning(paste0("the last threshold ", tail(thr,1), " repeated for range(s) ", prettyIntegers(seq(length(thr)+1, nranges))))
				thr = c(thr, rep(tail(thr,1), nranges-length(thr)))
				}
			thr = matrix(thr, nrow=nranges, ncol=nbeams)
			}
		}
	
	# Get the acoustic data and voxel data of the ping t[t_seq]. Here we read all time steps and use subset_TSD() to select single time steps below:
	data = sonR::read.event(event=event, adds=list(nlmp=nlmp), var=c("vbsc", "volx", "harx", "psxx", "psyx", "pszx"), t=t[t_seq], kern=kern, allow.old=TRUE, try.runmed=TRUE, esnm=esnm, ...)
	vessel = sonR::read.event(event=event, adds=list(nlmp=nlmp), var=c("psxv", "psyv", "pszv"), t=t[t_seq], kern=kern, allow.old=TRUE, try.runmed=TRUE, esnm=esnm, ...)
	
	# Get the dimensions of the data:
	dimvbsc = dim(data$vbsc)[1:2]
	
	# Get the voxel indices, and discard the beam ends using 'beamend':
	valid = TSD::ind.expand(ind, dimvbsc)
	valid[[1]] = setdiff(valid[[1]], tail(seq_len(dimvbsc[1]), beamend))
	valid = TSD::arr.ind2ind(valid, dimvbsc)
	
	
	# Run through the time steps in the block:
	file_thist_seq = t_seq[1]
	for(thist_seq in t_seq){
		# Extract data for the current time step. This is sloppy programming, but it serves the need of reading data in blocks for higher speed:
		thist_seqind = which(thist_seq==t_seq)
		suppressWarnings(d <- c(subset_TSD(data, list(NULL, NULL, thist_seqind)), subset_TSD(vessel, list(thist_seqind))))
		vbscmed = noiseANDvbsc$vbscmed[,,thist_seqind]
		if(length(noiseANDvbsc$Xsb1)){
			Xsb1 = noiseANDvbsc$Xsb1[,thist_seqind]
			Xsbg = noiseANDvbsc$Xsbg[,thist_seqind]
			}
		else{
			Xsb1 = NULL
			Xsbg = NULL
			}
			
		# Extract 'cmpred' and ellipsoid:
		if(length(cmpred)>3){
			if(length(cmpred)<3*t[thist_seq]){
				matrix(rep(cmpred, length=3*t[thist_seq]), byrow=TRUE, ncol=3, nrow=t[thist_seq])
				}
			cmpred = cmpred[t[thist_seq],]
			}
		if(length(ellipsoid)>3){
			if(length(ellipsoid)<3*t[thist_seq]){
				matrix(rep(ellipsoid, length=3*t[thist_seq]), byrow=TRUE, ncol=3, nrow=t[thist_seq])
				}
			ellipsoid = ellipsoid[t[thist_seq],]
			}
		
		# Apply the subset defined by center (cmpred) and dimensions (ellipsoid):
		subset = rseg.event_subsetfun(d, cmpred, ellipsoid)
		valid = intersect(valid, if(is.logical(subset)) which(subset) else subset)
		if(is.list(avoid)){
			if(length(avoid$vessel)>0){
				vesselt = seq(thist_seq-avoid$numt+1, thist_seq)
				vesselt = vesselt[vesselt>0]
				vessel = sonR::read.event(event=event, c("psxv", "psyv"), t=t[vesselt])
				thisavoid = lapply(seq_along(vessel$psxv), function(x) valid[which(sqrt((d$psxx[valid]-vessel$psxv[x])^2 + (d$psyx[valid]-vessel$psyv[x])^2)<avoid$dist)])
				valid = setdiff(valid, unique(unlist(thisavoid)))
				}
			if(length(avoid$points)>0){
				thisavoid = lapply(seq_along(avoid$points[,1]), function(x) valid[which(sqrt((d$psxx[valid]-avoid$points[x,1])^2 + (d$psyx[valid]-avoid$points[x,2])^2)<avoid$dist)])
				valid = setdiff(valid, unique(unlist(thisavoid)))
				}
			if(length(valid)==0){
				warning("All data discarded by 'avoid', which should only be used for sonars.")
				}
			}
		
		# Apply the 'range':
		valid = sonR::subset_TSD(d, subset=valid, range=range, ind.out=TRUE)$subs$psxx
		
	
		##### Execution and output #####
		# Return the last segmentation output:
		lastout=list()
		# If nseg2==0 do only the noise-up segmentations:
		if(nseg2==0){	
			# Get the threshold from the noise:
			if(length(thr)>0){
				thisthr = thr
				}
			else{
				thisthr = Xsbg * 10^(noisethr/10)
				}
			# Apply the threshold:
			thisseg = which(vbscmed > thisthr)
			
			# Discard invalid voxels:
			thisseg = intersect(thisseg, valid)
			
			# Write the segmentation data:
			lastout = rseg.event_oneping_write(d=d, thisseg=thisseg, thist_seq=thist_seq, t=t, utim=utim, valid=valid, subset=subset, dimvbsc=dimvbsc, segfilesdir=segfilesdir, sfnr=sfnr, dBan=noisethr, dBb1=NaN, dBb2=NaN, dBb3=NaN, dBb4=NaN, dBbs=NaN, Xsbg=Xsbg, Xsb1=Xsb1, thisthr=thisthr, thersholdFromSchool=NaN, Xcsz=Xcsz[t[thist_seq]], save.trh=save.trh, save.data=save.data, save.ind=save.ind, kern=kern, wt=wt, type=type, cmpred=cmpred, ellipsoid=ellipsoid, file_thist_seq, reserve=length(t_seq)-1)
			}
		else{
			# Get the threshold from the noise:
			thersholdFromNoise = Xsbg * 10^(noisethr/10)
			# Get the total threshold:
			thisthr = pmax(thersholdFromNoise, thersholdFromSchool[t[thist_seq]])
			# Apply the threshold:
			thisseg = which(vbscmed > thisthr)
			# Discard invalid voxels:
			thisseg = intersect(thisseg, valid)
			lastout = rseg.event_oneping_write(d=d, thisseg=thisseg, thist_seq=thist_seq, t=t, utim=utim, valid=valid, subset=subset, dimvbsc=dimvbsc, segfilesdir=segfilesdir, sfnr=sfnr, dBan=noisethr, dBb1=dBb1, dBb2=dBb2, dBb3=dBb3, dBb4=dBb4, dBbs=dBbs[t[thist_seq]], Xsbg=Xsbg, Xsb1=Xsb1, thisthr=thisthr, thersholdFromSchool=thersholdFromSchool[t[thist_seq]], Xcsz=Xcsz[t[thist_seq]], save.trh=save.trh, save.data=save.data, save.ind=save.ind, kern=kern, wt=wt, type=type, cmpred=cmpred, ellipsoid=ellipsoid, file_thist_seq, reserve=length(t_seq)-1)
			}
		}
	lastout
	##################################################
	##################################################
	}
