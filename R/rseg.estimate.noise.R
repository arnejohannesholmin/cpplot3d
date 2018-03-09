#*********************************************
#*********************************************
#' Estimate the background (noise) level as specified by the Robust segmentation method in Profos.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR kernSmooth1 median3d read.event
#' @importFrom TSD arr.ind2ind ind.expand zeros
#'
#' @export
#' @rdname rseg.estimate.noise
#'
rseg.estimate.noise <- function(event, t_seq=1, add=2, t=1, kern=21L, type=2, noisesmooth=100, noise=NULL, ind=NULL){
	# Function for estimating the background (noise):
	Xsbg_fun <- function(vbscmed, ind, noisesmooth=NULL){
		noiseest = vbscmed
		noiseest[-ind] = NA
		# Get the noise estimate as the mean or median across beams, giving a [lenb] vector:
		if(type==1){
			noiseest = rowMeans(noiseest) # [J]
			}
		else if(type==2){
			noiseest = median3d(noiseest, 2) # [J]
			}
		else{
			noiseest = noiseest
			}
		# Smooth the noise estimate by a Gaussian filter along the beam:
		if(length(noisesmooth)>0){
			kernSmooth1(noiseest, kern=noisesmooth, nsd=3) # [J]. This can be used to multiply with any [J, I, K] array.
			}
		else{
			noiseest
			}
		}
	
	# To estimate noise, read the data in a time step window around 't_seq':
	readt = lapply(t_seq, "+", -add:add)
	readt = lapply(readt, function(x) x[x>=1 & x<=length(t)])
	cleant = unique(unlist(readt))
	current = match(t_seq, cleant)
	readt_invbsc = lapply(readt, match, cleant)
	# Read the data in a window of time steps:
	d = read.event(event=event, var="vbsc", t=t[cleant], kern=kern, allow.old=TRUE, drop.out=FALSE, try.runmed=TRUE) # [J, I, K]
	
	# Get the dimensions of the data at time step:
	dimvbsc = dim(d$vbsc)[1:2]
	# Get the voxel indices:
	ind = ind.expand(ind, dimvbsc)
	ind = arr.ind2ind(ind, dimvbsc)
	
	# Median smooth across pings, giving a [lenb, numb] matrix:
	# Estimate the noise:
	vbscmed = zeros(dimvbsc, length(t_seq))
	for(i in seq_along(readt)){
		vbscmed[,,i] = median3d(d$vbsc[,,readt_invbsc[[i]], drop=FALSE], 3) # [J, I]
		}
	
	if(length(noise)==0){
		Xsbg = zeros(dimvbsc[1], length(t_seq))
		Xsb1 = Xsbg
		for(i in seq_along(readt)){
			Xsbg[,i] = Xsbg_fun(vbscmed[,,i], ind, noisesmooth)
			Xsb1[,i] = Xsbg_fun(d$vbsc[,,current[i]], ind)
			}
		}
	else{
		Xsb1 = NULL
		Xsbg = NULL
		}
	#vbscmed = median3d(d$vbsc, 3) # [J, I]
	
	# Estimate the noise:
	#noise1 = NULL
	#if(length(noise)==0){
	#	# Get the dimensions of the data at time step:
	#	dimvbsc = dim(d$vbsc)[1:2]
	#	# Get the voxel indices:
	#	ind = ind.expand(ind, dimvbsc)
	#	ind = arr.ind2ind(ind, dimvbsc)
	#	noise = Xsbg(vbscmed, ind, noisesmooth)
	#	noise1 = Xsbg(d$vbsc[,,current], ind)
	#	}
	list(Xsbg=drop(Xsbg), Xsb1=drop(Xsb1), vbscmed=drop(vbscmed), t_seq=t_seq, t=t, readt=readt, cleant=cleant)
	}
