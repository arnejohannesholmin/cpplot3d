#*********************************************
#*********************************************
#' Segments SX90 data using noise-up and top-down.
#'
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t  is a vector of the time steps to segment, or a two element vector of ftim values between which rawfiles are extracted from a directory (see the description of 'event').
#' @param cores  is the number of cores to use for parallel processing.
#' @param noise  can be given as an array of background noise values (either of dimension [J], or [J, I], where J is the length of the beams and I is the number of beams), in which case the background will not be calculated for each time step.
#' @param kern  is an integer if the Profus segmentation method is applied, smoothing the data by a median filter of 21 voxels along the beams. If specified as double (not using the "L" behind the number), Gaussian kernel is used along the beams, with a warning.
#' @param wt  is the number of time steps used as the width of the median filter along time steps.
#' @param noisethr  is a vector of segmentation threshold levels above the noise. One segmentation file is written for each value of 'noisethr'.
#' @param schoolthr  is a function of size and mean volume backscattering strength (S_V) defining the threshold level below the 90-percentile of the school using the initial above-noise-threshold. Can also be a vector of values, where one segmentation file is written for each value of 'schoolthr'.
#' @param thr  is the fixed threshold to use, in which case 'noisethr' and 'schoolthr' are ignored.
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes, and is used to discard regions of the sampling volume of the sonar, such as the inner 100 voxels and the 11 beams behind the vessel (default)..
#' @param range  is a list of elements with names matching names of 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param beamend  is the number of voxels at the end of the beams which should be discarded from the segmentation.
#' @param type  is the function to use across beams (one of "mean" and "median", where "mean" is used in PROFUS up to 2014, while "median" is more robust to the valus of the school).
#' @param noisesmooth  is the standard deviation of the Gaussian kernel smoother for the background noise/signal estimate, set by CMR (Christian Michelsen Research) to 100.
#' @param startn  is the file segmentation file number written to the first segmentation file. If given as a vector of two elements, the second element is the segmentation file number of the first file using below-school-threshold.
#' @param fresh  should be set to FALSE if existing noise-up files should be kept. Otherwise, these are overwritten without warning.
#' @param keep.temp  should be set to TRUE to keep all temporary files written by the cores.
#' @param save.trh  should be set to TRUE to save the smoothed background noise/signal estimate and the threshold value, 'lenb' values, for each ping. Requires typically 20 KB per ping and 20 MB per 1000 pings. For a mini survey of 12 hours and one ping per 2 seconds, the two variables require more than 500 MB.
#' @param save.data  is TRUE to save the actual segmented acoustic data.
#' @param rawevent  specifies a directory with raw files which are copied to 'event' and converted to TSD filess before running the segmentation.
#' @param dir.data  is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#' @param owtsd  is FALSE to not overwrite existing data when copying data from 'rawevent'.
#' @param temp  is used in extract_event(), and is TRUE to only return the temporary directory if raw files already exist and toTSD==TRUE, in which case the existing and temporary tsd files are not merged into the existing.
#' @param track  is TRUE to track the school recursively in time, to locate the midpoint of the ellipsoid restricting the segmentation of the schools.
#' @param ellipsoid  is an optional vector of up to three elements specifying the semi axes of an ellipsoid centered at the center of mass of a user selected region, outside which voxels will not be included in the segmentation. The ellispoid can be set to move with the center of mass of the segmented voxels.
#' @param cmpred  is a three column matrix holding the centers of mass of the ellipsoid outside which no samples are discarded from the segmentation mask. This matrix must have one row per time step, starting at the first time step.
#' @param nlmp  is the number of center of mass positions of the school used to predict the next center of mass.
#' @param avoid  defines a region in which segmentation will not be performed, such as a regio around the vessel track, typically avoid=list(numt=50, dist=30, vessel=TRUE), indicating that all points within 30 m of the last 50 vessel positions should be discarded.
#' @param plot  is TRUE to plot the segmentations for each time step, or for each 'plot' time step if 'plot' is numeric.
#' @param ...  parameters passed to merge_TSD(), particularly 'chunksize'.	
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom rgl ellipse3d plot3d points3d selectpoints3d
#' @importFrom sonR extract_event medSmooth1 merge_events read.event
#' @importFrom TSD even merge_TSD prettyIntegers read.TSD splitSeqIntoBlocks strff zeros
#' @importFrom utils tail head
#' @importFrom stats quantile lm predict
#' @importFrom grDevices hsv
#' @importFrom tools file_path_sans_ext
#'
#' @export
#' @rdname rseg.event
#'
rseg.event<-function(event, t=1, cores=1, memsize=1e9, noise=NULL, kern=21L, wt=5, noisethr=8, schoolthr=function(S, SBR) 4.4 + 0.013*S + 0.1*SBR, thr=NULL, ind=list(-(1:300), -(49+(-5:5))), subset=NULL, range=NULL, beamend=kern, type=c("median", "mean"), noisesmooth=100, startn=NULL, fresh=FALSE, keep.temp=FALSE, save.trh=FALSE, save.data=FALSE, save.ind=FALSE, rawevent=NULL, dir.data=NULL, owtsd=TRUE, temp=FALSE, track=FALSE, ellipsoid=NA, cmpred=NA, nlmp=200, avoid=NULL, plot=FALSE, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-02-21 - Clean version.
	# Update: 2014-08-28 - Updated to apply first a threshold set 'noisethr' above the noise, then use the resulting segmentation masks to estimate the upper 90-percentile of the school values, smooth these in time by a spline smoother, and repeat the segmentations with thresholds set 'schoolthr' belov the smoothed school 90-percentile.
	# Update: 2014-09-23 - Implemented the function dBbs = 2.5 + 0.02 * S, where S = 2 * sqrt(Xtha/pi) is the diameter of the school, as if it were circular, estimated by the initial segmentation using the threshold 8 dB above the smoothed estimated noise.
	# Update: 2014-10-23 - Added the c++ implementation of median smoothing and fixed some bugs.
	# Update. 2015-01-15 - Fixed bugs related to reading old noise-up-segmentation data, where now read.event() used instead of rea
	# Update. 2015-05-12 - Added 'beamend' and function(S, SBR) 2.57 - 32.0/S + 0.526*SBR.
	# Update. 2015-06-04 - Extracted the three functions rseg.event_oneping_write(), rseg.event_oneping(), and rseg.event_getschoolthr().
	# Update. 2015-09-29 - Added the option of processing directly from a directory of raw files not in the catalogue structure used for the events.
	# Update: 2015-10-05 - Added the function rseg.estimate.noise() to rseg.event_oneping().
	# Update: 2016-05-03 - Added support for fixed threshold. However, this is less efficient than simple thresholding in batches of hundreds of pings.
	# Last: 2016-06-15 - Added support for segmenting in blocks.
	########### DESCRIPTION: ###########
	# Segments SX90 data using noise-up and top-down.
	########## DEPENDENCIES: ###########
	# read.event(), echoIBM.get.segfilename(), rseg.event_oneping_write(), rseg.event_oneping(), rseg.event_getschoolthr().
	############ VARIABLES: ############
	# ---event--- is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# ---t--- is a vector of the time steps to segment, or a two element vector of ftim values between which rawfiles are extracted from a directory (see the description of 'event').
	# ---cores--- is the number of cores to use for parallel processing.
	# ---noise--- can be given as an array of background noise values (either of dimension [J], or [J, I], where J is the length of the beams and I is the number of beams), in which case the background will not be calculated for each time step.
	# ---kern--- is an integer if the Profus segmentation method is applied, smoothing the data by a median filter of 21 voxels along the beams. If specified as double (not using the "L" behind the number), Gaussian kernel is used along the beams, with a warning.
	# ---wt--- is the number of time steps used as the width of the median filter along time steps.
	# ---noisethr--- is a vector of segmentation threshold levels above the noise. One segmentation file is written for each value of 'noisethr'.
	# ---schoolthr--- is a function of size and mean volume backscattering strength (S_V) defining the threshold level below the 90-percentile of the school using the initial above-noise-threshold. Can also be a vector of values, where one segmentation file is written for each value of 'schoolthr'.
	# ---thr--- is the fixed threshold to use, in which case 'noisethr' and 'schoolthr' are ignored.
	# ---ind--- is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes, and is used to discard regions of the sampling volume of the sonar, such as the inner 100 voxels and the 11 beams behind the vessel (default)..
	# ---range--- is a list of elements with names matching names of 'data', specifying the range of the corresponding elements.
	# ---subset--- is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
	# ---beamend--- is the number of voxels at the end of the beams which should be discarded from the segmentation.
	# ---type--- is the function to use across beams (one of "mean" and "median", where "mean" is used in PROFUS up to 2014, while "median" is more robust to the valus of the school).
	# ---noisesmooth--- is the standard deviation of the Gaussian kernel smoother for the background noise/signal estimate, set by CMR (Christian Michelsen Research) to 100.
	# ---startn--- is the file segmentation file number written to the first segmentation file. If given as a vector of two elements, the second element is the segmentation file number of the first file using below-school-threshold.
	# ---fresh--- should be set to FALSE if existing noise-up files should be kept. Otherwise, these are overwritten without warning.
	# ---keep.temp--- should be set to TRUE to keep all temporary files written by the cores.
	# ---save.trh--- should be set to TRUE to save the smoothed background noise/signal estimate and the threshold value, 'lenb' values, for each ping. Requires typically 20 KB per ping and 20 MB per 1000 pings. For a mini survey of 12 hours and one ping per 2 seconds, the two variables require more than 500 MB.
	# ---save.data--- is TRUE to save the actual segmented acoustic data.
	# ---rawevent--- specifies a directory with raw files which are copied to 'event' and converted to TSD filess before running the segmentation.
	# ---dir.data--- is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
	# ---owtsd--- is FALSE to not overwrite existing data when copying data from 'rawevent'.
	# ---temp--- is used in extract_event(), and is TRUE to only return the temporary directory if raw files already exist and toTSD==TRUE, in which case the existing and temporary tsd files are not merged into the existing.
	# ---track--- is TRUE to track the school recursively in time, to locate the midpoint of the ellipsoid restricting the segmentation of the schools.
	# ---ellipsoid--- is an optional vector of up to three elements specifying the semi axes of an ellipsoid centered at the center of mass of a user selected region, outside which voxels will not be included in the segmentation. The ellispoid can be set to move with the center of mass of the segmented voxels.
	# ---cmpred--- is a three column matrix holding the centers of mass of the ellipsoid outside which no samples are discarded from the segmentation mask. This matrix must have one row per time step, starting at the first time step.
	# ---nlmp--- is the number of center of mass positions of the school used to predict the next center of mass.
	# ---avoid--- defines a region in which segmentation will not be performed, such as a regio around the vessel track, typically avoid=list(numt=50, dist=30, vessel=TRUE), indicating that all points within 30 m of the last 50 vessel positions should be discarded.
	# ---plot--- is TRUE to plot the segmentations for each time step, or for each 'plot' time step if 'plot' is numeric.
	# ---...--- parameters passed to merge_TSD(), particularly 'chunksize'.	
	
	
	##################################################
	##################################################
	##### Preparation #####
	getMovingAverage <- function(x, l=5){
		if(length(x)==0){
			NULL
		}
		else if(length(dim(x))==0){
			x
		}
		else{
			colMeans(tail(x,l))
		}
	}
	
	predictcm <- function(cm, l=nlmp, skip=2, ell=ellipsoid, w=NULL){
		if(length(cm)==0){
			NA
		}
		else{
			if(length(w)>0){
				w <- w/quantile(w,0.9)
				w[w>1] <- 1
			}
			lastcm <- tail(cm,1)
			cm <- head(tail(cm, l+skip), l)
			aa <- seq_len(NROW(cm))
			if(any(!is.na(cm))){
				l <- apply(cm, 2, function(bb) lm(bb~aa, weights=w))
				thiscm <- sapply(l, predict, data.frame(aa=NROW(cm)+1))
			}
			else{
				thiscm <- lastcm
			}
			if(sqrt(sum((thiscm-lastcm)^2, na.rm=TRUE))>mean(ell)){
				lastcm
			}
			else{
				thiscm
			}
		}
	}
		
	mergeSegfiles <- function(segfilesdir, targetdir, numt, filesize=1e9){
		# Allways save in only one file for each combination of 'dBan' and 'dBbs', regardless of the number of time steps. This is done for simplicity, and due to the following calculation of storage: Take a mini survey of 12 hours and one ping per 2 seconds (21600 pings) with 3253 samples along each beam at 600 m max range. Imagine a school covering 10 beams over 1000 samples, approximately 200 m in size, and keep this in the sonar volume at all pings. The total amount of segmentation values is then 21600 * 10 * 1000 * 4e-6 MB = 864 MB, where 4e-6 reflects 4 bytes per long integer. This extreme case only reaches close to 1 GB, which is not scary:
		### mergedfiles <- merge_TSD(segfilesdir, reserve=numt-1, filesize=filesize, ...)$x_merged
		cat("\nMeriging segmentation files...\n")
		mergedfiles <- merge_TSD(segfilesdir, reserve=numt-1, filesize=filesize, pbar=FALSE, ...)
		newmergedfiles <- file.path(targetdir[1], basename(mergedfiles))
		newmergedfiles <- sapply(strsplit(newmergedfiles, "_", fixed=TRUE), function(xx) paste0(paste0(xx[-length(xx)], collapse="_"), ".seg"))
		# Move the merged files to the TSD directory:
		file.copy(mergedfiles, newmergedfiles)
	}
	
	# Get the type of acoustic instrument:
	esnm <- read.event(event=event, var="esnm")$esnm
	
	# Convert the schoolthreshold into a list of functions:
	#schoolthr <- rseg.event_getschoolthr(schoolthr)
	
	# Convert 'type' to a numeric, mean = 1 and median = 2:
	if(strff("mea", type[1])){
		type <- 1
	}
	else if(strff("med", type[1])){
		type <- 2
	}
	else{
		type <- 0
		warnings("Neither mean or median taken across beams. Noise estimate has dimension [length of beams, number of beams]")
	}
	
	if(!is.integer(kern)){
		warning("'kern' not given as an integer (use as.integer() or put \"L\" behind the number to get an integer). Gaussian kernel used along the beams")
	}
	
	# If 'event' is an existing directory or has length 3, generate the event given by 'event' based on the raw files in rawevent. Also 't' needs to be given in ftim format, and will be cross referenced with the time information in the file names:
	tempevent <- NULL
	if(length(event) > 0 && length(rawevent) && file.exists(rawevent[1]) && nchar(t[1])>=13){
		event <- extract_event(event, rawevent, t, ow=owtsd, dir.data=dir.data, temp=temp)
		# Prepare 'rawevent' and 't' to the segmentation:
		t <- event$t
		
		# Switch the existing and temporary events so that the temporary event is segmented:
		if(temp && length(event$tempevent)>0){
			tempevent <- event$event
			event <- event$tempevent
		}
		else{
			event <- event$event
		}
		# Update 't':
		#t <- read.event(event,var="indt")$indt
	}
	
	# Crop 't' to valid values:
	maxt <- read.event(event=event, var="numt", allow.old=TRUE)$numt
	if(length(maxt)==0){
		warnings("Event is empty")
		return(event)
	}
	if(identical(t, "all")){
		t <- seq_len(maxt)
	}
	# Allow for ftim input in 't':
	if(nchar(t[1])>=13){
		t <- read.event(event, t=t, var="indt")$indt
	}
	t <- t[t >= 1 & t <= maxt]
	# Get beam modes, and accept only horizontal mode (0):
	bmmd <- read.event(event=event, var=c("bmmd", "indt"), t="all", allow.old=TRUE)
	if(length(bmmd$bmmd)){
		t <- intersect(t, bmmd$indt[bmmd$bmmd == 0])
	}
	numt <- length(t)
	
	# If an ellipsoid is specified, repeat to matrix numt by 3:
	if(length(ellipsoid)==0){
		ellipsoid <- NA
	}
	ellipsoid <- matrix(rep(ellipsoid, length=3*maxt), byrow=TRUE, ncol=3, nrow=maxt)
	if(length(cmpred)==0){
		cmpred <- NA
	}
	
	if(NROW(cmpred)<maxt){
		cmpred <- matrix(rep(cmpred, length=3*maxt), byrow=TRUE, ncol=3, nrow=maxt)
	}
		
	# Only odd values are valid for 'wt':
	if(even(wt)){
		stop("Only odd values are valid for 'wt'")
	}
	add <- (wt-1)/2
	
	# If 'thr' is given, this is considered as a fixed threshold, and schoolthr and noisethr is ignored:
	if(length(thr)>0){
		# Set the noise and noise threshold to NA to give meaningful output in the saved files, and to avoid estimating noise from the data:
		noisethr <- rep(NA, length(thr))
		noise <- NA
		schoolthr <- NULL
	}
	# The number of segmentations:
	nseg1 <- length(noisethr)
	nseg2 <- length(schoolthr)
	
	# Store the unix time points, the dimenstions of the data, and the total volume of the valid sampling region (discarding beams and ranges not in 'ind')
	utim <- read.event(event=event, var="utim", t="all", allow.old=TRUE)$utim
	
	track <- cores==1 && track
	if(track && all(is.na(cmpred[t[1],]))){
		bmmd <- read.event(event, t=t, var="bmmd")$bmmd
		idx <- pplot3d.event(event, t=if(length(bmmd)>0) t[min(which(bmmd!=2))] else t[1], view="t", ind=ind, subset=subset, range=range, ...)
		cat("Select one point or a rectangle in the pplot\n")
		#selection <- selectpoints3d(idx$idx[1])
		selection <- selectpoints3d()
		cmpred[t[1],] <- array(colMeans(selection, na.rm=TRUE), dim=c(1,3))
		cat("Center of school selected:", paste0(round(cmpred[t[1],], digits=1), sep=", "), "\n")
		ee <- ellipse3d(diag(3)*ellipsoid[t[1],]^2, centre=cmpred[t[1],], t=1)
		plot3d(ee, col="green", alpha=0.2, add=TRUE)		
	}
	
	
	##### Execution and output #####
	# Create directories for the individual segmentation files
	ndigits <- nchar(numt)
	if(length(startn)<2){
		segfiles <- echoIBM.get.segfilename(n=nseg1 + nseg2, t=t, nchart=ndigits, event=event, startn=startn)
		sfnr <- segfiles[[2]]
		segfiles <- segfiles[[1]]
	}
	else if(length(startn)>1){
		segfiles1 <- echoIBM.get.segfilename(n=nseg1, t=t, nchart=ndigits, event=event, startn=startn[1])
		segfiles2 <- echoIBM.get.segfilename(n=nseg2, t=t, nchart=ndigits, event=event, startn=startn[2])
		sfnr <- c(segfiles1[[2]], segfiles2[[2]])
		segfiles <- c(segfiles1[[1]], segfiles2[[1]])
	}
	
	# Convert to directories for the parallel processing (remove the ):
	segfilesdirs <- tools::file_path_sans_ext(segfiles)
	segfilesdirs <- sapply(segfilesdirs, function(xx) file.path(dirname(dirname(xx)), "temp_seg", basename(xx)))
	# Create temporary directories, also for the noise-up segmentations regardless of whether these segmentations exist in the event directory, since temporary files by default are deleted at the end:
	suppressWarnings(lapply(segfilesdirs, dir.create, recursive=TRUE))
	targetdir <- file.path(dirname(dirname(segfilesdirs)), "tsd")[1]
	
	# Look for noise-up file:
	#uu <- UNIX_time("~/Data/echoIBM/SX90_biomassEstimation/Events/SX90_biomassEstimation_E0001_one_school_at_the_time_directional_fish/SX90/tsd", var = "l000")
	#segs <- which(sapply(uu$l000,function(x) any("dBbs" %in% x)))
	#rr <- unlist(read.TSDs(unlist(uu$f000[segs]), var = "dBbs", clean = FALSE, addInfo = FALSE))
	#is.na(rr)
	# Group the time steps by predicted memory size, so that each core treats blocks of time steps instead of single time steps. Use memsize < size1 to force one time step to be processed at the time:
	beams <- read.event(event=event, var="beams")
	nBytes <- 8
	fact <- 4.1 * 3.5 # 4.1 is from vbsc and three position data + volume data at each time step, and 3.5 is from the memory occupied during calculations using these data.
	t_seq <- splitSeqIntoBlocks(t=seq_along(t), size1=beams$numb[1] * beams$lenb[1] * nBytes * fact, size=memsize, blocks=cores)
	
	
	cat("Time steps grouped as follows:\n")
	cat(paste(seq_along(t_seq), sapply(t_seq, function(xx) prettyIntegers(t[xx])), sep=": "), sep="\n")
	cm <- NULL
	
	# Read the data using the 'kern' number of voxels for the median kernel along the beams (indicated by integer type for 'kern'). If integer is not specified for 'kern', Gaussian kernel is used:
	useOldNoiseUpFiles <- !fresh && all(file.exists(segfiles[1]) & !file.info(segfiles[1])$isdir)
	if(useOldNoiseUpFiles){
		cat("Segmentation files based on the smoothed noise/background already exists\n")
	}
	else{
		cat("Processing pings with thershold based on the smoothed noise/background\n")
		if(cores>1){
			# Generate the clusters of time steps:
			cores <- min(cores, detectCores())
			cat("Parallel segmentation on", cores, "cores:\n")
			cl<-makeCluster(cores)
			# Run segmentation on multiple cores:
			out <- pblapply(t_seq, rseg.event_block, t=t, utim=utim, noise=noise, noisethr=noisethr, thr=thr, segfilesdir=segfilesdirs[1], sfnr=sfnr, thersholdFromSchool=-Inf, dBb1=NaN, dBb2=NaN, dBb3=NaN, dBb4=NaN, dBbs=NaN, nseg1=nseg1, nseg2=0, Xcsz=NaN, add=add, event=event, kern=kern, type=type, noisesmooth=noisesmooth, ind=ind, range=range, beamend=beamend, save.trh=save.trh, save.data=save.data, save.ind=save.ind, wt=wt, esnm=esnm, cmpred=cmpred, avoid=avoid, ellipsoid=ellipsoid, nlmp=nlmp, ..., cl=cl)
			# End the parallel bootstrapping:
			stopCluster(cl)
		}
		else{
			# Plot a time bar showing the progress of the reading and plotting:
			infostring <- "Segmenting initial"
			cat(infostring,"\n",sep="")
			totalsteps <- numt
			stepfact <- nchar(infostring)/totalsteps
			oldvalue <- 0
			
			out <- list()
			
			for(t_seq_ind in seq_along(t_seq)){
				# Print a dot if the floor of the new value exceeds the old value in:
				thisvalue <- floor(t_seq_ind*stepfact)
				if(thisvalue > oldvalue){
					cat(rep(".",thisvalue-oldvalue),if(t_seq_ind == totalsteps) "\n", sep="")
					oldvalue <- thisvalue
				}
				# Setting thersholdFromSchool = -Inf works because it is not used in segment.event_oneping():
				thist_seq <- t_seq[[t_seq_ind]]
				out <- rseg.event_block(thist_seq, t=t, utim=utim, noise=noise, noisethr=noisethr, thr=thr, segfilesdir=segfilesdirs[1], sfnr=sfnr, thersholdFromSchool=-Inf, dBb1=NaN, dBb2=NaN, dBb3=NaN, dBb4=NaN, dBbs=NaN, nseg1=nseg1, nseg2=0, Xcsz=NaN, add=add, event=event, kern=kern, type=type, noisesmooth=noisesmooth, ind=ind, range=range, beamend=beamend, save.trh=save.trh, save.data=save.data, save.ind=save.ind, wt=wt, esnm=esnm, cmpred=cmpred, avoid=avoid, ellipsoid=ellipsoid, nlmp=nlmp, ...)
				
				if(track && t_seq_ind<length(t_seq)){
					cm <- rbind(cm, c(out$Xcmx, out$Xcmy, out$Xcmz))
					nextt_seq <- t_seq[[t_seq_ind+1]]
					cmpred[t[nextt_seq],] <- predictcm(cm)
					if(plot && sum(is.na(tail(cm,1)))==0){
						cplot3d.event(event, t=t[thist_seq], view="t", ind=ind, subset=subset, range=range, ...)
						if(!is.na(cmpred[t[thist_seq],])){
							points3d(cmpred[t[thist_seq],,drop=FALSE], col=hsv(thist_seq/numt, 1-thist_seq/numt*0.3, 0.7 + thist_seq/numt*0.3))
							ee <- ellipse3d(diag(3) * ellipsoid[t[thist_seq],]^2, centre=cmpred[t[thist_seq],], t=1)
							plot3d(ee, col="green", alpha=0.2, add=TRUE)
						}
						pplot3d.event(event, t=t[thist_seq], adds=out, var="sgsc", acca=if(tolower(esnm)%in%"MS70") 50 else 500, add=TRUE)
					}
				}
			}
		}
		# If 'cm' was read, set 'ellipsoid' to NULL:
		if(length(cm)>0){
			runEllipsoid <- FALSE
		}
		
		# Merge the noise-up segmentation files:
		mergeSegfiles(segfilesdirs[1], targetdir, numt, filesize=1e9)
	}
	cat("\n")
	if(nseg2>0){
		cat("Processing pings with top-down-thershold from the 90-percentile Sv of the schools segmented with the threshold based on the smoothed noise/background\n")
		
		# Smooth the 90-percentile estimates of the school:
		Xqsv <- zeros(maxt)+1e-20
		Xsbr <- zeros(maxt)
		Xtha <- zeros(maxt)
		Xcsz <- zeros(maxt)
		
		# Read merged segmentation files:
		this <- read.TSD(segfiles[1], t=t, var=c("Xqsv","Xtha","Xsbr"), indt=TRUE,header=FALSE)
		Xqsv[t] <- this$Xqsv
		Xsbr[t] <- this$Xsbr
		Xtha[t] <- this$Xtha
		Xqsv[t] <- medSmooth1(Xqsv[t], w=5, try.runmed=FALSE)
		Xsbr[t] <- medSmooth1(Xsbr[t], w=5, try.runmed=FALSE)
		Xcsz[t] <- medSmooth1(2*sqrt(Xtha[t]/pi), w=5, try.runmed=FALSE)
		
		# Create a column matrix of below-signal-thresholds, and store also the parameters of the funcitons giving 'dBbs':
		dBb1 <- dBb2 <- dBb3 <- dBb4 <- 0
		dBbs <- zeros(maxt, nseg2)
		
		if(is.function(schoolthr)){
			formalsschoolthr <- formals(schoolthr)
			if(length(formalsschoolthr) == 1){
				dBbs[,1] <- schoolthr(S=Xcsz)
				dBb1 <- schoolthr(0)
				dBb2 <- schoolthr(1) - dBb1
			}
			else if("SBR" %in% names(formalsschoolthr)){
				dBbs[,1] <- schoolthr(S=Xcsz, SBR=10*log10(Xsbr))
				dBb1 <- schoolthr(0,0)
				dBb2 <- schoolthr(1,0) - dBb1
				dBb3 <- schoolthr(0,1) - dBb1
			}
			else if("Sv" %in% names(formalsschoolthr)){
				dBbs[,1] <- schoolthr(S=Xcsz, Sv=10*log10(Xqsv))
				dBb1 <- schoolthr(0,0)
				dBb2 <- schoolthr(1,0) - dBb1
				dBb4 <- schoolthr(0,1) - dBb1
			}
			### else{
			### 	dBbs <- schoolthr
			### 	dBb1 <- schoolthr
			### }
		}
		else{
			dBbs <- matrix(schoolthr, byrow=TRUE, ncol=nseg2, nrow=maxt)
			dBb1 <- schoolthr
		}

		# Get the threshold values from the school and down:
		#thersholdFromSchool <- outer(c(Xqsv), 10^(dBbs/10), "/")
		thersholdFromSchool <- Xqsv / (10^(dBbs/10))
		
		if(cores>1){
			# Detect the number of cores and use the minimum of this and the number of requested cores:	
			cores <- min(cores, detectCores())
			cat("Parallel segmentation on",cores,"cores:\n")
			cl<-makeCluster(cores)
			# Run segmentation on multiple cores:
			out <- pblapply(t_seq, rseg.event_block, t=t, utim=utim, noise=noise, noisethr=noisethr, thr=thr, segfilesdir=segfilesdirs[-1], sfnr=sfnr, thersholdFromSchool=thersholdFromSchool, dBb1=dBb1, dBb2=dBb2, dBb3=dBb3, dBb4=dBb4, dBbs=dBbs, nseg1=nseg1, nseg2=nseg2, Xcsz=Xcsz, add=add, event=event, kern=kern, type=type, noisesmooth=noisesmooth, ind=ind, range=range, beamend=beamend, save.trh=save.trh, save.data=save.data, save.ind=save.ind, wt=wt, esnm=esnm, cmpred=cmpred, avoid=avoid, ellipsoid=ellipsoid, nlmp=nlmp, ..., cl=cl)
			# End the parallel bootstrapping:
			stopCluster(cl)
		}
		else{
			# Plot a time bar showing the progress of the reading and plotting:
			infostring <- "Segmenting final"
			cat(infostring,"\n",sep="")
			totalsteps <- length(t)
			stepfact <- nchar(infostring)/totalsteps
			oldvalue <- 0
			
			for(t_seq_ind in seq_along(t_seq)){
				# Print a dot if the floor of the new value exceeds the old value:
				thisvalue <- floor(t_seq_ind*stepfact)
				if(thisvalue > oldvalue){
					cat(rep(".",thisvalue-oldvalue),if(t_seq_ind == totalsteps) "\n", sep="")
					oldvalue <- thisvalue
				}
				
				thist_seq <- t_seq[[t_seq_ind]]
				out <- rseg.event_block(thist_seq, t=t, utim=utim, noise=noise, noisethr=noisethr, thr=thr, segfilesdir=segfilesdirs[-1], sfnr=sfnr, thersholdFromSchool=thersholdFromSchool, dBb1=dBb1, dBb2=dBb2, dBb3=dBb3, dBb4=dBb4, dBbs=dBbs, nseg1=nseg1, nseg2=nseg2, Xcsz=Xcsz, add=add, event=event, kern=kern, type=type, noisesmooth=noisesmooth, ind=ind, range=range, beamend=beamend, save.trh=save.trh, save.data=save.data, save.ind=save.ind, wt=wt, esnm=esnm, cmpred=cmpred, avoid=avoid, ellipsoid=ellipsoid, nlmp=nlmp, ...)
				
				if(plot && sum(is.na(tail(cm,1)))==0){
					cplot3d.event(event, t=t[thist_seq[1]], view="t", ind=ind, subset=subset, range=range, ...)
					if(!is.na(cmpred[t[thist_seq],])){
						points3d(cmpred[t[thist_seq],,drop=FALSE], col=hsv(thist_seq/numt, 1-thist_seq/numt*0.3, 0.7 + thist_seq/numt*0.3))
						ee <- ellipse3d(diag(3)*ellipsoid[t[thist_seq],]^2, centre=cmpred[t[thist_seq],], t=1)
						plot3d(ee, col="green", alpha=0.2, add=TRUE)
					}
					pplot3d.event(event, t=t[thist_seq], adds=out, var="sgsc", acca=if(tolower(esnm)%in%"MS70") 50 else 500, add=TRUE, cols=2)
				}
			}
		}
		# Finally, merge the top-down segmentation files:
		lapply(segfilesdirs[-1], mergeSegfiles, targetdir=targetdir, numt=numt, filesize=1e9)
		#mergeSegfiles(segfilesdirs[2], targetdir, numt, filesize=1e9)
	}
	cat("\n")
	
	# Move to trash:
	if(!keep.temp){
		unlink(dirname(segfilesdirs[1]), recursive=TRUE)
	}
	# If raw files were copied from a directory, but there were raw files already existing in the target directory so that a temporary directory was created, from which TSD-files and segmentation files were generated, AND the user specified to keep this temporary directory, merge the existing and temporary TSD-files here:
	if(temp && length(tempevent)>0){
		merge_events(c(tempevent, event), tempevent, cruise=NULL, esnm=NULL, dir.data=NULL, ctd=1)
		unlink(event, recursive=TRUE)
		# Switch back:
		event <- tempevent
	}
	invisible(event)
	##################################################
	##################################################
}
