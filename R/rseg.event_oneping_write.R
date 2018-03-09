#*********************************************
#*********************************************
#' Writes one ping of segmented data from the (SX90) unbiased segmentatino method. Used in rseg.event().
#'
#' @param d  is the list of data including segmentation and acoustic data.
#' @param thisseg  is the segmentatino mask.
#' @param thist  is the time step index. One of 1:t.
#' @param t  is all the time steps to be segmented.
#' @param utim  is a a vector of UNIX time points corresponding to 't'. 
#' @param valid  is indices of the valid region of the sonar volume.
#' @param subset  is the subset related to excusion of vessel realted noise such as wake.
#' @param dimvbsc  are the dimenstions of the acoustic data.
#' @param segfilesdir  is the directory of the segmentation files to be written.
#' @param sfnr  is the segmentatino file number.
#' @param dBan  is the dB above background.
#' @param dBb1, dBb2, dBb3, dBb4, dBbs  are the dB belov the signal, where the first four are parameters in the function f(S, SBR, Sv) with dBb1 being the constant term.
#' @param Xsbg, Xsb1  are vectors of the smoothed background estimate (noise is the median over consecutive pings and Gaussian kernel smoothed along beams, whereas Xsb1 is not).
#' @param thisthr  is a vector of the above background-thresholds.
#' @param thersholdFromSchool  is the (unbiased) threshold values defined from the singal.
#' @param Xcsz  is the circular size of the schools, calucated as 2*sqrt(A/pi).
#' @param save.trh  is TRUE to save the threshold values used in the segmentation.
#' @param save.data  is TRUE to save the data included in the segmentation mask.
#' @param kern  is the standard deviation in units of the number of voxels used in a Gaussian kernel smoother along the beams (no smoothing if kern == 0 or length(kern) == 0, which is the default). If given as an integer, say 5L, median smoothing is applied instead of Gaussian.
#' @param wt  is the number of time steps used as the width of the median filter along time steps.
#' @param type  is the function to use across beams (one of "mean" and "median", where "mean" is used in PROFUS up to 2014, while "median" is more robust to the valus of the school).
#' @param cmpred  is a three column matrix holding the centers of mass of the ellipsoid outside which no samples are discarded from the segmentation mask. This matrix must have one row per time step, starting at the first time step.
#' @param ellipsoid  is an optional vector of up to three elements specifying the semi axes of an ellipsoid centered at the center of mass of a user selected region, outside which voxels will not be included in the segmentation. The ellispoid can be set to move with the center of mass of the segmented voxels.
#' @param file_thist  is the time index in the block. If this is equal to 'thist', a new file will be written. Otherwise data will be appended to the file with name given by 'file_thist'.
#' @param reserve  is used in write.TSD() to reserve time steps for the whole block of time steps.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR cm.school
#' @importFrom TSD ind2arr.ind write.TSD zeropad
#' @importFrom stats quantile
#'
#' @export
#' @rdname rseg.event_oneping_write
#'
rseg.event_oneping_write<-function(d, thisseg, thist, t, utim, valid, subset, dimvbsc, segfilesdir, sfnr, dBan, dBb1, dBb2, dBb3, dBb4, dBbs, Xsbg, Xsb1, thisthr, thersholdFromSchool, Xcsz, save.trh, save.data, save.ind, kern, wt, type, cmpred, ellipsoid, file_thist, reserve){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-06-04 - Extracted from rseg.event().
	########### DESCRIPTION: ###########
	# Writes one ping of segmented data from the (SX90) unbiased segmentatino method. Used in rseg.event().
	########## DEPENDENCIES: ###########
	# cm.school(), write.TSD()
	############ VARIABLES: ############
	# ---d--- is the list of data including segmentation and acoustic data.
	# ---thisseg--- is the segmentatino mask.
	# ---thist--- is the time step index. One of 1:t.
	# ---t--- is the time steps to be segmented.
	# ---utim--- is a a vector of UNIX time points corresponding to 't'. 
	# ---valid--- is indices of the valid region of the sonar volume.
	# ---dimvbsc--- are the dimenstions of the acoustic data.
	# ---segfilesdir--- is the directory of the segmentation files to be written.
	# ---sfnr--- is the segmentatino file number.
	# ---dBan--- is the dB above background.
	# ---dBb1, dBb2, dBb3, dBb4, dBbs--- are the dB belov the signal, where the first four are parameters in the function f(S, SBR, Sv) with dBb1 being the constant term.
	# ---thisthr--- is a vector of the above background-thresholds.
	# ---Xcsz--- is the circular size of the schools, calucated as 2*sqrt(A/pi).
	
	##################################################
	##################################################
	##### Preparation #####
	D = list()
	if(length(thisseg)>0){
		# Get the backscatter, background (noise), volumes, areas of the school:
		if(length(dim(Xsbg))==2){
			XsbgS = Xsbg[thisseg]
			}
		else{
			XsbgS = Xsbg[ind2arr.ind(thisseg, dimvbsc)[, 1]]
			}
		if(length(dim(Xsb1))==2){
			Xsb1S = Xsb1[thisseg]
			}
		else{
			Xsb1S = Xsb1[ind2arr.ind(thisseg, dimvbsc)[, 1]]
			}
		# Crop to the length of the data:
		vbscS = d$vbsc[thisseg]
		volxS = d$volx[thisseg]
		
		### Get all required variables of the school: ###
		# (1) The total horizontal area of the school:
		D$Xtha = if(length(d$harx)>0) sum(d$harx[thisseg], na.rm=TRUE) else NaN
		# (2) The total volume of the school:
		D$Xtvl = sum(volxS, na.rm=TRUE)
		
		# (3) The total estimated background (noise) of the school:
		D$Xtbr = sum(XsbgS * volxS, na.rm=TRUE) # Total Backscatter used as Reference in the initial segmentation 
		# (4) The total estimated background (noise) of the school:
		D$Xtbb = sum(Xsb1S * volxS, na.rm=TRUE) # Total Backscatter of the Background (noise)
		# (5) The total backscatter of the school including background (noise):
		D$Xtbt = sum(vbscS * volxS, na.rm=TRUE) # Total Backscatter of the total data (school + background)
		# (6) The total backscatter of the school:
		D$Xtbs = D$Xtbt - D$Xtbb # Total Backscatter of the school
		
		# (7) The total estimated background (noise) of the school in decibel (Target Strength):
		D$XtTR = 10*log10(D$Xtbr)
		# (8) The total estimated background (noise) of the school in decibel (Target Strength):
		D$XtTB = 10*log10(D$Xtbb)
		# (9) The total backscatter of the school including background (noise) in decibel (Target Strength):
		D$XtTT = 10*log10(D$Xtbt)
		# (10) The total backscatter of the school in decibel (Target Strength):
		D$XtTS = 10*log10(D$Xtbs)
		
		# (11) Get the signal to background (noise) ratio:
		D$Xsbr = D$Xtbs/D$Xtbb
		# (12) And in decibel:
		D$XSBR = 10*log10(D$Xsbr)
		
		# (13) The volume of the valid part of the sonar volume, discarding typically the 100 closest voxels of each beam and 11 beams to the rear:
		D$Xvvl = sum(d$volx[valid])
		# (14) The horizontal area of the valid part of the sonar volume, discarding typically the 100 closest voxels of each beam and 11 beams to the rear:
		D$Xvha = if(length(d$harx)>0) sum(d$harx[valid]) else NaN
		
		# (15) The mean volume backscattering coefficient of the school:
		D$Xmsv = D$Xtbs / D$Xtvl
		# (16) The mean volume backscattering strength (decibel) of the school:
		D$XmSv = 10*log10(D$Xmsv)
		# (17) The average volume backscattering coefficient of the school:
		D$Xasv = mean(vbscS)
		# (18) The average volume backscattering strength (decibel) of the school:
		D$XaSv = 10*log10(D$Xasv)
		# (19) The maximum volume backscattering coefficient of the school:
		D$Xxsv = max(vbscS)
		# (20) The maximum volume backscattering strength (decibel) of the school:
		D$XxSv = 10*log10(D$Xasv)
		# (21) The upper 90 % quantile of the volume backscattering coefficient of the school:
		D$Xqsv = quantile(vbscS, 0.90, na.rm=TRUE)
		# (22) The upper 90 % quantile of the volume backscattering strength (decibel) of the school:
		D$XqSv = 10*log10(D$Xqsv)
		# (23) The Gumbel semi-parametric mean volume backscattering coefficient of the ping:
		D$Xpsv = NaN
		# (24) The Gumbel semi-parametric mean volume backscattering strength (decibel) of the ping:
		D$XpSv = NaN
		
		# (25) The average volume backscattering coefficient from the valid segment of the ping:
		D$Xvsv = (D$Xasv * D$Xtvl) / D$Xvvl
		# (26) The average volume backscattering strength (decibel) from the valid segment of the ping:
		D$XvSv = 10*log10(D$Xvsv)
		# (27) The average area backscattering coefficient from the valid segment of the ping:
		D$Xvsa = (D$Xasv * D$Xtha) / D$Xvha
		# (28) The average area backscattering strength (decibel) from the valid segment of the ping:
		D$XvSa = 10*log10(D$Xvsa)
		
		# (29 - 31) The center of mass of the school in the global coordinate system (depth is less accurate due to only one fan of horizontally aligned beams):
		temp = cm.school(d, subset=thisseg)
		D$Xcmx = temp[1]
		D$Xcmy = temp[2]
		D$Xcmz = temp[3]
		# (32) The horizontal range to the school from the sonar:
		D$Xhra = sqrt((D$Xcmx-d$psxv)^2 + (D$Xcmy-d$psyv)^2)
		# (33 - 35) The predicted center of mass of the school in the global coordinate system , given by linear regression of the past 'nlmp' available values of 'Xcmx', 'Xcmy', 'Xcmz' (missing values included):
		D$Xcex = cmpred[1]
		D$Xcey = cmpred[2]
		D$Xcez = cmpred[3]
		D$nlmp = d$nlmp
		# (36 - 37) The total backacatter inside the ellipsoid, including noise/background:
		D$Xebt = sum(d$vbsc[subset] * d$volx[subset], na.rm=TRUE) 
		D$XeBT = 10*log10(D$Xebt)
		# (38) The angle of incidence to the segmented school:
		D$anis = atan2(d$psyv - D$Xcmy, d$psxv - D$Xcmx)
		# (39) The angle of incidence to the ellipsoid enclosing the school:
		D$anio = atan2(d$psyv - D$Xcey, d$psxv - D$Xcex)
		# (40 - 42) The ellipsoid used to restrict the segmentation to, and which is used in 'Xebt':
		D$Xelx = ellipsoid[1]
		D$Xely = ellipsoid[2]
		D$Xelz = ellipsoid[3]
		}
	else{
		thisseg = NULL
		# (1) The total horizontal area of the school:
		D$Xtha = NaN
		# (2) The total volume of the school:
		D$Xtvl = NaN
		
		# (3) The total estimated background (noise) of the school:
		D$Xtbr = NaN
		# (4) The total estimated background (noise) of the school:
		D$Xtbb = NaN
		# (5) The total backscatter of the school including background (noise):
		D$Xtbt = NaN
		# (6) The total backscatter of the school:
		D$Xtbs = NaN
		
		# (7) The total estimated background (noise) of the school in decibel (Target Strength):
		D$XtTR = NaN
		# (8) The total estimated background (noise) of the school in decibel (Target Strength):
		D$XtTB = NaN
		# (9) The total backscatter of the school including background (noise) in decibel (Target Strength):
		D$XtTT = NaN
		# (10) The total backscatter of the school in decibel (Target Strength):
		D$XtTS = NaN
		
		# (11) Get the signal to background (noise) ratio:
		D$Xsbr = NaN
		# (12) And in decibel:
		D$XSBR = NaN
		
		# (13) The volume of the valid part of the sonar volume, discarding typically the 100 closest voxels of each beam and 11 beams to the rear:
		D$Xvvl = NaN
		# (14) The horizontal area of the valid part of the sonar volume, discarding typically the 100 closest voxels of each beam and 11 beams to the rear:
		D$Xvha = NaN
		
		# (15) The mean volume backscattering coefficient of the school:
		D$Xmsv = NaN
		# (16) The mean volume backscattering strength (decibel) of the school:
		D$XmSv = NaN
		# (17) The average volume backscattering coefficient of the school:
		D$Xasv = NaN
		# (18) The average volume backscattering strength (decibel) of the school:
		D$XaSv = NaN
		# (19) The maximum volume backscattering coefficient of the school:
		D$Xxsv = NaN
		# (20) The maximum volume backscattering strength (decibel) of the school:
		D$XxSv = NaN
		# (21) The upper 90 % quantile of the volume backscattering coefficient of the school:
		D$Xqsv = NaN
		# (22) The upper 90 % quantile of the volume backscattering strength (decibel) of the school:
		D$XqSv = NaN
		# (23) The Gumbel semi-parametric mean volume backscattering coefficient of the ping:
		D$Xpsv = NaN
		# (24) The Gumbel semi-parametric mean volume backscattering strength (decibel) of the ping:
		D$XpSv = NaN
		
		# (25) The average volume backscattering coefficient from the valid segment of the ping:
		D$Xvsv = NaN
		# (26) The average volume backscattering strength (decibel) from the valid segment of the ping:
		D$XvSv = NaN
		# (27) The average area backscattering coefficient from the valid segment of the ping:
		D$Xvsa = NaN
		# (28) The average area backscattering strength (decibel) from the valid segment of the ping:
		D$XvSa = NaN
		
		# (29 - 31) The center of mass of the school in the global coordinate system (depth is less accurate due to only one fan of horizontally aligned beams):
		D$Xcmx = NaN
		D$Xcmy = NaN
		D$Xcmz = NaN
		D$Xcmx = NaN
		# (32) The horizontal range to the school from the sonar:
		D$Xhra = NaN
		# (33 - 35) The predicted center of mass of the school in the global coordinate system , given by linear regression of the past 'nlmp' available values of 'Xcmx', 'Xcmy', 'Xcmz' (missing values included):
		D$Xcex = cmpred[1]
		D$Xcey = cmpred[2]
		D$Xcez = cmpred[3]
		D$nlmp = NaN
		# (36 - 37) The total backacatter inside the ellipsoid, including noise/background:
		D$Xebt = NaN
		D$XeBT = NaN
		# (38) The angle of incidence to the segmented school:
		D$anis = NaN
		# (39) The angle of incidence to the ellipsoid enclosing the school:
		D$anio = atan2(d$psyv - D$Xcey, d$psxv - D$Xcex)
		# (40 - 42) The ellipsoid used to restrict the segmentation to, and which is used in 'Xebt':
		D$Xelx = ellipsoid[1]
		D$Xely = ellipsoid[2]
		D$Xelz = ellipsoid[3]
		}
	# Create simple file names, since the files are merged at the end:
	file = file.path(segfilesdir, paste0(basename(segfilesdir), "_", zeropad(t[file_thist], nchar(max(t))), ".tsd"))
	
	
	##### Execution and output #####
	ind = list()
	if(save.ind){
		ind = ind2arr.ind(valid, dimvbsc)
		ind = split(ind, rep(seq_len(ncol(ind)), each=nrow(ind)))
		names(ind) = c("ind1", "ind2")
		}
	seg = list()
	if(save.data){
		seg = list(sgvb=d$vbsc[thisseg], sgvl=d$volx[thisseg], sgpx=d$psxx[thisseg], sgpy=d$psyx[thisseg], sgpz=d$pszx[thisseg])
		}
	thisthr = list()
	if(save.trh){
		thisthr = list(Xsb1=Xsb1, Xsbg=Xsbg)
		}
	# The output list:
	out = c(
		list(utim=utim[t[thist]], indt=t[thist], sgsc=thisseg), 
		seg,
		ind,
		D, 
		list(mdf1=kern, mdf3=wt, dBan=dBan, dBb1=dBb1, dBb2=dBb2, dBb3=dBb3, dBb4=dBb4, dBbs=dBbs, dBaF=10*log10(thersholdFromSchool/(D$Xtbb/D$Xtvl)), smty=type, sfnr=sfnr, Xcsz=Xcsz), 
		thisthr)
	
	# Write the data:
	write.TSD(out, file, numt=1, keep.null=TRUE, header=list(dtyp=list(sgsc="long")), append=file_thist!=thist, reserve=reserve)
	out
	##################################################
	##################################################
	}
