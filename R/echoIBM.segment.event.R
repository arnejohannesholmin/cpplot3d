#*********************************************
#*********************************************
#' NA
#'
#' @param event  is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
#' @param t  is either the number of the ping to be treated, as listed from 1 to the number of pings in the event, or the time point given as a string "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF". Only one time step alowed!
#' @param cruise  is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event.
#' @param bgns  is ann optional list of noise estimates (must contain the background noise 'bgns', and may contain the periodic noise estimates 'pns1', 'pns2' and 'harm'). The phase parameter 'pns2' is extracted from the data.
#' @param beta0  is the minimum schooling threshold for the volume backscattering coefficient. For voxels where beta0<noise: beta0=noise.
#' @param beta1  is the maximum schooling threshold for the volume backscattering coefficient, defining the probability distribution of the signal. Should be chosen on the basis of the maximum packing density of the observed species.
#' @param misM  is the mean expected observed volume backscattering coefficient (linear value) used in the segmentation. Else, 'misM' should have length equal to the number of time steps. If misM==NULL the absolute lower and upper schooling threshold 'lsth' and 'usth' are used.
#' @param iter  is TRUE to apply the iterative method where 'misM' is used in an initial segmentation, the result of which is used to re-estimate 'misM' by the funciton meanSv.TSD() which is then used in the final segmentation. If more than one time steps is requested, the previous estimated 'misM' is used as the initial estimate for the next time steps (thus the input value of 'misM' disregarded). If 'turns' is given different from 1, the mean of the currently processed block of pings is used as the initial estimate of 'misM' for all pings of the next block of pings.  The iterative method uses 'misM' as an initial estimate used when segmenting for 5 dB below this value, and then the 'misM' is estimated from the resulting segmentation mask (scaled by a factor determined from simulations), and then segmenting again with the new estimate. 
#' @param factor  is the factor by which the input mean observed volume backscattering strength is multiplied to obtain the enlarged segmentation mask. Lower value normally leads to larger segmentation mask.
#' @param minmsvM  is only used when the length of 'misM' is 1 (iterative estimation of the mean sv), and ensures that 'misM' does not go below 'minmsvM' (preventing surface scattering and other noisy values).
#' @param pow  is a value to raise the segmentation threshold values to the power of, which if pow<1 eases the strictness of the segmentations, suitable for bottom detection (pow=1/4 might be good):
#' @param ind  is a vector or list of indices along the beams, as input to ind.expand(), used to select the subset over which the estimation of background noise is done. Also voxels not included in this subset are assigned p=1.
#' @param nsind  is a vector of indices along the beams, as input to ind.expand(), used to select the subset over which the estimation of the phase of the periodic noise is done. If given as a single numeric, the outermost 'nsind' voxels are used in each beam.
#' @param smind  is a list of indices used to define the subset which is smoothed spatially to accumulate the probabilities. The default avoids smoothing densely packed voxels, which reduces computational time.
#' @param h  is the bandwidth of the spatial Gaussian kernel smoother, given as the standard deviation of the Gaussian distribution. For no smoothing use h=NULL.
#' @param alpha  is the significance level of the hypothesis testing of H0: school not present in the voxel, against H1: school present in the voxel. To return non-thresholded data, use alpha=NULL.
#' @param dir.data  is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
#' @param hins_add  is the number of voxels that should be discarded on both sides of high intensity noise voxels voxels along beams, used for accounting for possible high values that are related to the high intensity noise but not classified as such voxels.
#' @param turns  is the number of time steps treated in each block of time steps, where higher values demand more memory but reduces CPU time.
#' @param phase  is FALSE if any of 'pn3M' (phase for each time step) or 'pns3' (phase equal for all time steps) given in 'bgns' or read from the noise file located by the funciton noise.path.event() should be used, as oposed to estimating the phase from the data for each time step. This is only recommended for simulated data where the phase is constant over all time steps, and saves some CPU time.
#' @param var  is "vbsc" for segmenting volume backscattering data, and "posf" for segmenting original fish position data.
#' @param dens  is the packing density of the school, used when var="posf". These values are attemptedly read in the event, if not given as input to the function.
#' @param segdir  is an optional string giving the name of the folder in which to put the segmentation files. The folder will be put in the directory of the event. segdir==NULL results in segdir="seg0".
#' @param subtractNoise  is TRUE if the original acoustic data should be subtracted noise, which is used when returning the total and mean volume backscatter from the segment.
#' @param pr0s.out  is TRUE if the smoothed probabilities of insuffuciently high true volume backscattering coefficient to imply school should be returned in arrays of the same dimensions as the volume backscattering coefficient 'vbsc'.
#' @param adds  is an optional list of variables overriding the variables in read from the event.
#' @param sim  is a TRUE if smoothing should be done only along the first dimensions, simultaneously over the stages of the last dimension. If 'sim' is an integer larger than 1, the positions 'coords' are used 'sim' times, and the data 'x' should have length 'sim' times the length of one coordinate of 'coords'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR event.path meanSv.TSD read.event rotate3D volx.TSD UNIX_time
#' @importFrom TSD arr.ind2ind read.TSD strff write.TSD zeros
#' @importFrom utils tail head
#'
#' @export
#' @rdname echoIBM.segment.event
#'
echoIBM.segment.event<-function(event=1, t=1, cruise=2009116, bgns=NULL, beta0=NULL, beta1=NULL, misM=NULL, iter=TRUE, factor=10^(-5/10), minmsvM=10^(-40/10), pow=1, ind=list(-(1:30), NULL), nsind=0.75, smind=list(-(1:300)), range=list(), subset=NULL, h=NULL, alpha=NULL, filesize=3e8, hins_add=10, turns=10, phase=TRUE, TVG.exp=2, var=c("vbsc", "sgbt", "posf"), dens=NULL, segdir=NULL, esnm="MS70", subtractNoise=TRUE, pr0s.out=FALSE, adds=list(), sim=TRUE, allow.old=FALSE, TOV=0, startn=1, ...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2012-05-23 - Clean version.
	# Update: 2012-05-23 - Added the parameter 'hins_add' for discarding voxels on both sides along beams of the inentified high intensity noise.
	# Update: 2012-06-08 - Added the parameters 'var' and 'dens', for segmentation of original fish position data.
	# Update: 2012-10-24 - Changed to support the default parameter values derived in Holmin/Korneliussen/Tjøstheim 2013.
	# Update: 2012-11-14 - Substituted the option 'pns3new' by 'phase' with default TRUE, indicating estimation of the phase at each ping.
	# Update: 2012-12-18 - Added 'code'.
	# Update: 2012-12-18 - Added 'iter' and 'ifNA'.
	# Update: 2012-13-06 - Added 'lst0' to the output.
	# Update: 2012-12-18 - Removed 'iter', and 'ifNA', and cleaned up the output.
	# Update: 2013-02-27 - Added the iterative method when the length of the initial estimate 'misM' is 1.
	# Update: 2013-04-03 - Fixed bad code, where the iterative method applied when the length of 'misM' is 1 only thresholded with the updated 'misM', whereas in the fixed code new segmentation masks are generated.
	# Update: 2013-04-03 - Added the enlarged segmentation mask suggested in Paper III of the PhD of Holmin (sgsE), giving also estimates of the mean expected observed volume backscattering coefficient and strength, as calculated in Eq. (8) in Paper III.
	# Update: 2013-08-12 - Merged the calculation of the unbiased and enlarged segmentation masks, saving 30 % of the CPU time.
	# Update: 2013-08-21 - Fixed bug when misM=NULL.
	# Update: 2013-10-04 - Added the option 'pr0s.out', which if TRUE writes the probability data to the file.
	# Update: 2013-10-14 - Added 'iter' again, to cover the case when length(t)==1.
	# Last: 2015-06-03 - Added center of mass in the output.
	########### DESCRIPTION: ###########
	# 
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---event--- is the identifier of the event, either given as the number of the event, a string contained in the name of the event, or the path of the event directory.
	# ---t--- is either the number of the ping to be treated, as listed from 1 to the number of pings in the event, or the time point given as a string "yyyymmddHHMMSS.FFF" or "HHMMSS.FFF". Only one time step alowed!
	# ---cruise--- is either the idenfication number of the cruise, given as specified by the IMR (yyyynnn), or the path to the directory containing the event.
	# ---bgns--- is ann optional list of noise estimates (must contain the background noise 'bgns', and may contain the periodic noise estimates 'pns1', 'pns2' and 'harm'). The phase parameter 'pns2' is extracted from the data.
	# ---beta0--- is the minimum schooling threshold for the volume backscattering coefficient. For voxels where beta0<noise: beta0=noise.
	# ---beta1--- is the maximum schooling threshold for the volume backscattering coefficient, defining the probability distribution of the signal. Should be chosen on the basis of the maximum packing density of the observed species.
	# ---misM--- is the mean expected observed volume backscattering coefficient (linear value) used in the segmentation. Else, 'misM' should have length equal to the number of time steps. If misM==NULL the absolute lower and upper schooling threshold 'lsth' and 'usth' are used.
	# ---iter--- is TRUE to apply the iterative method where 'misM' is used in an initial segmentation, the result of which is used to re-estimate 'misM' by the funciton meanSv.TSD() which is then used in the final segmentation. If more than one time steps is requested, the previous estimated 'misM' is used as the initial estimate for the next time steps (thus the input value of 'misM' disregarded). If 'turns' is given different from 1, the mean of the currently processed block of pings is used as the initial estimate of 'misM' for all pings of the next block of pings.  The iterative method uses 'misM' as an initial estimate used when segmenting for 5 dB below this value, and then the 'misM' is estimated from the resulting segmentation mask (scaled by a factor determined from simulations), and then segmenting again with the new estimate. 
	# ---factor--- is the factor by which the input mean observed volume backscattering strength is multiplied to obtain the enlarged segmentation mask. Lower value normally leads to larger segmentation mask.
	# ---minmsvM--- is only used when the length of 'misM' is 1 (iterative estimation of the mean sv), and ensures that 'misM' does not go below 'minmsvM' (preventing surface scattering and other noisy values).
	# ---pow--- is a value to raise the segmentation threshold values to the power of, which if pow<1 eases the strictness of the segmentations, suitable for bottom detection (pow=1/4 might be good):
	# ---ind--- is a vector or list of indices along the beams, as input to ind.expand(), used to select the subset over which the estimation of background noise is done. Also voxels not included in this subset are assigned p=1.
	# ---nsind--- is a vector of indices along the beams, as input to ind.expand(), used to select the subset over which the estimation of the phase of the periodic noise is done. If given as a single numeric, the outermost 'nsind' voxels are used in each beam.
	# ---smind--- is a list of indices used to define the subset which is smoothed spatially to accumulate the probabilities. The default avoids smoothing densely packed voxels, which reduces computational time.
	# ---h--- is the bandwidth of the spatial Gaussian kernel smoother, given as the standard deviation of the Gaussian distribution. For no smoothing use h=NULL.
	# ---alpha--- is the significance level of the hypothesis testing of H0: school not present in the voxel, against H1: school present in the voxel. To return non-thresholded data, use alpha=NULL.
	# ---dir.data--- is the path to the directory in which the projects are stored, defaulted by the variable Acoustics_datasets_directory().
	# ---hins_add--- is the number of voxels that should be discarded on both sides of high intensity noise voxels voxels along beams, used for accounting for possible high values that are related to the high intensity noise but not classified as such voxels.
	# ---turns--- is the number of time steps treated in each block of time steps, where higher values demand more memory but reduces CPU time.
	# ---phase--- is FALSE if any of 'pn3M' (phase for each time step) or 'pns3' (phase equal for all time steps) given in 'bgns' or read from the noise file located by the funciton noise.path.event() should be used, as oposed to estimating the phase from the data for each time step. This is only recommended for simulated data where the phase is constant over all time steps, and saves some CPU time.
	# ---var--- is "vbsc" for segmenting volume backscattering data, and "posf" for segmenting original fish position data.
	# ---dens--- is the packing density of the school, used when var="posf". These values are attemptedly read in the event, if not given as input to the function.
	# ---segdir--- is an optional string giving the name of the folder in which to put the segmentation files. The folder will be put in the directory of the event. segdir==NULL results in segdir="seg0".
	# ---subtractNoise--- is TRUE if the original acoustic data should be subtracted noise, which is used when returning the total and mean volume backscatter from the segment.
	# ---pr0s.out--- is TRUE if the smoothed probabilities of insuffuciently high true volume backscattering coefficient to imply school should be returned in arrays of the same dimensions as the volume backscattering coefficient 'vbsc'.
	# ---adds--- is an optional list of variables overriding the variables in read from the event.
	# ---sim--- is a TRUE if smoothing should be done only along the first dimensions, simultaneously over the stages of the last dimension. If 'sim' is an integer larger than 1, the positions 'coords' are used 'sim' times, and the data 'x' should have length 'sim' times the length of one coordinate of 'coords'.
	
	
	##################################################
	##################################################
	########## Preparation ##########
	# Get the directory of the event:
	event = event.path(event=event, cruise=cruise)$event
	# Get the time indices and the formated time strings corresponding to 't':
	time = read.event(event=event, var=c("indt", "time"), t=t)
	
	# The number of time steps:
	numt = length(time$indt)
	# Read the beam configuration:
	beams = read.event(event=event, var="beams")
	
	# Read the default parameter values, where the defaults:
	if(any(strff("v", var[1]) , strff("s", var[1]) ) && any(length(beta0)==0, length(beta1)==0, length(h)==0)){
    #echoIBM_datadir_ = file.path(echoIBM_frameworks, "R", "Functions", "echoIBM Main", "Utilities")
	#filebasename = "beta0-c-table.TSD"
	#defaults = file.path(echoIBM_datadir_, filebasename)
	#defaults = read.TSD(defaults)
    defaults = read.TSD(system.file("extdata", "beta0-c-table.TSD", package="echoIBM"))
		if(length(beta0)==0){
			if(length(misM)==0){
				beta0 = defaults$lsth
				}
			else{
				beta0 = defaults$rlst
				}
			}
		if(length(beta1)==0){
			if(length(misM)==0){
				beta1 = defaults$usth
				}
			else{
				beta1 = defaults$rust
				}
			}
		if(length(h)==0){
			h = defaults$bwGp
			}
		if(length(alpha)==0){
			alpha = "table"
			}
		}
	# 'totallength' limits the size of the files:
	totallength = zeros(length(alpha))
	# Define the header used when writing the data:
	newfile =! logical(length(alpha))
	
	# Create the directory to put the segmentation files into:
	if(length(segdir)==0){
		dir.out = file.path(event, c("seg", "seg0")[strff("p", var[1])+1])
		}
	else{
		dir.out = file.path(event, segdir)
		}
	if(!file.exists(dir.out)){
		suppressWarnings(dir.create(dir.out))
		}
	
	if(strff("s", var[1])){
		add = "_bottom"
		}
	else{
		add = ""
		}

	
	########## Execution and output ##########
	### (1) var=="posf" ###
	# If the 'var' is "posf", segment the original fish positions as the voxels that contain at least the fraction 'alpha' of the expected number of fish, calculated as the volume of the voxels times the packing density:
	if(strff("p", var[1])){
		
		# Define the name of the segmentation file:
		header = list(dtyp=c("floa", "doub", "floa", "floa", "floa", "floa", "doub", "floa"))
		
		# Read the packing density if not given as input to the function:
		if(length(dens)==0){
			dens = read.event(event=event, t=time$indt, var="scpd")$scpd
			if(length(dens)==0){
				stop("The packing density must be given or present in a TSD file as a variable named \"scpd\"")
				}
			}
		# Get the radial, azimuth, and elevation edges in the spherical coordinate system with the sonar at the origin:
		J = max(beams$lenb)
		I2 = length(unique(beams$freq))
		I1 = length(beams$freq)/I2
		numb = length(beams$lenb)
		# Define the dimensions of the conical voxels (in the spherical coordinate system), and use these to define the vectors used in findInterval():
		r = soundbeam_range(beams)
		#dr = beams$asps*beams$sint/2
		#r = c(0, seq_len(max(beams$lenb))*dr-dr/2)
		#utheta = beams$dira[1:I1]
		#utheta = sort(unique(beams$dira))
		utheta = unique(beams$dira)
		upptheta = TRUE
		if(utheta[1]>utheta[2]){
			upptheta = FALSE
			}
		utheta = sort(utheta)
		dtheta = diff(utheta)[1]
		theta = c(utheta[1]-dtheta/2, utheta+dtheta/2)
		#uphi = beams$dire[seq(1, numb, I1)]
		#uphi = sort(unique(beams$dire))
		uphi = unique(beams$dire)
		uppphi = TRUE
		if(uphi[1]>uphi[2]){
			uppphi = FALSE
			}
		uphi = sort(uphi)
		
		dphi = diff(uphi)[1]
		phi = c(uphi[1]-dphi/2, uphi+dphi/2)
		# Get the voxel volumes:
		volx = volx.TSD(beams)
						
		# Read the vessel dynamics:
		vessel = read.event(event=event, var="vessel", t=time$indt, TOV=TOV)
			
		cat("Segmenting original fish positions for time steps:\n")
		# Run through the time steps:
		for(i in seq_len(numt)){
			cat(time$indt[i], " of ", numt, "\n", sep="")
			# Read the fish position data:
			data = read.event(event=event, var=c("psxf", "psyf", "pszf", "indt", "utim"), t=time$indt[i], cs="v")
			
			# Rotate into the coordinate system of the sonar:
			pos_sph = rotate3D(cbind(data$psxf-vessel$psxv[i], data$psyf-vessel$psyv[i], data$pszf-vessel$pszv[i]-beams$psze), by="z", ang=vessel$rtzv[i], sph.out=TRUE)
			# Identify which voxels the fish are located in:
			fishr = findInterval(pos_sph[, 1], r)
			if(upptheta){
				fishtheta = findInterval(pos_sph[, 2] %% (2*pi), theta)
				}
			else{
				fishtheta = length(theta)-findInterval(pos_sph[, 2] %% (2*pi), theta)
				}
			if(uppphi){
				fishphi = findInterval(pos_sph[, 3], phi)
				}
			else{
				fishphi = length(phi)-findInterval(pos_sph[, 3], phi)
				}
			vox = arr.ind2ind(cbind(fishr, fishtheta, fishphi), c(J, I1, I2))
			
			# Calculate the expected number of fish in ecah voxel
			expected = dens[i]*volx
			# Get the number of fish in each voxel:
			nfish = table(vox)
			nfish = cbind(as.numeric(names(nfish)), nfish)
			
			for(thisa in seq_along(alpha)){
				thissgs0 = as.integer(nfish[nfish[, 2]>alpha[thisa]*expected[nfish[, 1]], 1])
				if(newfile[thisa]){
					segfile = echoIBM.get.segfilename(n=length(thisa), event=dir.out[1], add=paste(add, time$indt[i], sep="_"), startn=startn)
					sfnr = segfile[[2]]
					segfile = segfile[[1]]
					# Write the data to file, adding the parameters of the segmentation, the bandwidth of the Gaussian kernel smoothing of the probability "bwGp" ('h'), the lower schooling threshold value "lsth" ('beta0'), the upper schooling threshold value of the prior distribution "usth" ('beta1'), and the segmentation threshold value "sgth" ('alpha'):
					suppressWarnings(bytes<-write.TSD(con=segfile[[1]], header=header, x=c(list(indt=time$indt[i], mtim=time$mtim[i]), beams[c("numb", "lenb", "freq")], list(sgs0=thissgs0, Ttvl=sum(volx[thissgs0]), sgt0=alpha[thisa]), segfile[2]), numt=1, reserve=numt-1, keep.null=TRUE, ...))
					}
				else{
					# Write (append) the data to file:
					suppressWarnings(bytes<-write.TSD(con=segfile, header=header, x=list(indt=time$indt[i], mtim=time$mtim[i], numb=NULL, lenb=NULL, freq=NULL, sgs0=thissgs0, Ttvl=sum(volx[thissgs0]), sgt0=NULL), numt=1, append=TRUE, keep.null=TRUE, ...))
					
					}
				totallength[thisa] = totallength[thisa]+bytes
				if(totallength[thisa]>filesize){
					newfile[thisa] = TRUE
					totallength[thisa] = 0
					}
				else{
					newfile[thisa] = FALSE
					}
				}
			}
		}
	
	
	### (2) var=="vbsc" ###
	else{
		# 'minsgsc' sets a minimum number of segmented voxels required to apply meanSv.TSD() to estimate the mean expected observed volume backscattering strength:
		minsgsc = 5
		# Add 1 at the beginning of the vector 'factor':
		factor = c(1,factor)
		
		# Define the scaling factors for 'misM':
		# 5 dB below the initial estimate leads to a median value 1.35 too low estimated misM on the simulated schools (estimated in "Paper002-Segmentation002_4shapes3sizes4densities3depths_low_sidelobe_segmentation_new.R"):
		factor_iter = 10^(-5/10)
		scale = 1.35
		# Run through the time steps in blocks and segment the school and write to file:
		blocks = split(seq_len(numt), ceiling(seq_len(numt)/turns))
			
		# 'beta0' and 'beta1' should be of the same length as the number of time steps:
		if(length(beta0)!=numt){
			beta0 = rep(beta0, length.out=numt)
			}
		if(length(beta1)!=numt){
			beta1 = rep(beta1, length.out=numt)
			}
		# Expand the 'misM' if not of length 'numt':
		if(length(misM)!=numt){
			misM = rep(misM, length.out=numt)
			#warning("'misM' should have the same length as the number of time steps")
			}
		# Store the input 'misM':
		input_misM = misM
		
		##### Run through the blocks of time steps: #####
		cat("Segmenting time steps:\n")
		for(i in seq_along(blocks)){
			# Print the progression on screen:
			cat(time$indt[head(blocks[[i]], 1)], " - ", time$indt[tail(blocks[[i]], 1)], " (", head(blocks[[i]], 1), " - ", tail(blocks[[i]], 1), " of ", numt, ")\n", sep="")
			
			# Reserve space for the segmentation masks:
			# Unbiased:
			sgsc = vector("list", length(blocks[[i]]))
			# Enlarged:
			sgsE = vector("list", length(blocks[[i]]))
				
			# If the length of 'misM' in the input is 1, the iterative estimation method for 'misM' is applied:
			if(iter){
				# Define the vectors for 'beta0' and 'beta1', modified to be beta0=beta0*misM and beta1=beta1*misM if misM has positive length. With iter=TRUE, the segmentation is done for a lower value of the mean expected observed volume backscattering strength misM[thisi]*factor_iter (factor_iter=10^(-0.5)). This is done to subsequently multiply the resulting misM with the value scale=1.35, obtained by simulations:
				thismsvM = misM[blocks[[i]]]*factor_iter
				thisbeta0 = beta0[blocks[[i]]]*thismsvM
				thisbeta1 = beta1[blocks[[i]]]*thismsvM
				# Get the probability of not school:
				cat("Initial enlarged segmentation:\n")
				data<-suppressWarnings(echoIBM.vbsc2p.event(event=event, t=time$indt[blocks[[i]]], bgns=bgns, beta0=thisbeta0, beta1=thisbeta1, factor=factor, ind=ind, nsind=nsind, smind=smind, range=range, subset=subset, h=h, alpha=NULL, hins_add=hins_add, phase=phase, TVG.exp=TVG.exp, esnm=esnm, subtractNoise=subtractNoise, adds=adds, sim=sim, allow.old=allow.old, TOV=TOV))
				
				# 
				# Get the initial segmentation masks (indices of the non-school probability values exceeding the threshold 'alpha'):
				for(ii in seq_along(blocks[[i]])){
					thisi = blocks[[i]][ii]
					# Generate the segmentation threshold value, one for each voxel, as extracted from the table generated in paper 2 of the PhD of Holmin:
					sgth = echoIBM.rbeta02sgth(data$lsth[,,ii], rule=2, misM=thismsvM, pow=pow)$y
					sgsc[[ii]] = as.integer(which(data$pr0s[,,ii]<sgth))
					# Re-estimate the misM, multiplying with 'scale':
					if(length(sgsc[[ii]]) >= minsgsc){
						thisadd = prod(dim(data$vbsc)[1:2])*(ii-1)
						suppressWarnings(misM[thisi] <- meanSv.TSD(list(vbsc=data$vbsc[sgsc[[ii]]+thisadd]), allow.vbsc=TRUE)["Hhsv"] * scale)
						}
					}
				
				# Ensure that 'misM' is larger than minmsvM:
				misM[misM<=minmsvM] = minmsvM
				
				# Re-define misM for the next block:
				if(i<length(blocks)){
					misM[blocks[[i+1]]] = mean(misM[blocks[[i]]])
					}
				}
			
			# Re-define the betas:
			if(length(misM)>0){
				thismsvM = misM[blocks[[i]]]
				}	
			else{
				thismsvM = 1
				}		
			thisbeta0 = beta0[blocks[[i]]]*thismsvM
			thisbeta1 = beta1[blocks[[i]]]*thismsvM
			
			cat("Final unbiased segmentation:\n")
			data<-suppressWarnings(echoIBM.vbsc2p.event(event=event, t=time$indt[blocks[[i]]], bgns=bgns, beta0=thisbeta0, beta1=thisbeta1, factor=factor, ind=ind, nsind=nsind, smind=smind, range=range, subset=subset, h=h, alpha=NULL, hins_add=hins_add, phase=phase, TVG.exp=TVG.exp, esnm=esnm, subtractNoise=subtractNoise, adds=adds, sim=sim, allow.old=allow.old, TOV=TOV))
					
			# Get the segmentation masks (indices of the non-school probability values exceeding the threshold 'alpha'):
			for(ii in seq_along(blocks[[i]])){
				thisi = blocks[[i]][ii]
				# Generate the segmentation threshold value, one for each voxel, as extracted from the table generated in paper 2 of the PhD of Holmin:
				# For the unbiased segmentation masks:
				sgth = echoIBM.rbeta02sgth(data$lsth[,,ii], rule=2, misM=misM[thisi], pow=pow)$y
				sgsc[[ii]] = as.integer(which(data$pr0s[,,ii]<sgth))
				# For the enlarged segmentation masks:
				sgth = echoIBM.rbeta02sgth(data$lsth[,,ii+length(blocks[[i]])], rule=2, misM=misM[thisi], pow=pow)$y
				sgsE[[ii]] = as.integer(which(data$pr0s[,,ii+length(blocks[[i]])]<sgth))
				}
						
			# Write segmented voxels (indices of the non-school probability values exceeding the threshold 'alpha') to file:
			cat("Write segmentation data:\n")
			for(ii in seq_along(blocks[[i]])){
				thisi = blocks[[i]][ii]
				if(!is.numeric(alpha) || length(alpha)==0){
					alpha = 1L
					}
				
				# Add the voxel positions in the global cocorinate system, used for calculating the center of mass of the schools:
				#temp = read.event(event=event, var=c("psxx", "psyx", "pszx"), t=time$indt[thisi])
				#data[names(temp)] = temp
				# Run through the values of the segmentation threshold and write segmentation data:
				for(thisa in seq_along(alpha)){
					temp = echoIBM.segment.event_oneping_write(data, beams, time, sgsc, sgsE, ii, thisi, blocks[[i]], thisa, alpha, minsgsc, newfile, segfile, totallength, filesize, pr0s.out, dir.out, var, thisbeta0, thisbeta1, beta0, beta1, input_misM, misM, h, pow, startn, add, numt, factor, ...)
					newfile = temp$newfile
					segfile = temp$segfile
					totallength = temp$totallength
					}		
				} # End of for(ii in seq_along(blocks[[i]]))	
			} # End of for(i in seq_along(blocks))
		} # End of if(strff("p", var[1]))
	# Update the UNIX_time file, since time steps has been appended to the segmentation file(s):
	UNIX_time(event=event, file=TRUE, fresh=TRUE)
	
	invisible(totallength)
	##################################################
	##################################################
	}
