#*********************************************
#*********************************************
#' Returns a list of three elements, where the first element is a three column matrix of regenerated points of acoustic size 'acca', the second element is a three column matrix of regenerated points of acoustic size 'acca' where the number of points in each voxel is no larger than 'nlim', and the third element is a three column matrix of regenerated points of acoustic size acca/fact.
#' !!!Only one time step is treated!!!!
#'
#' @param data  is the list of TSD inputs as returned from read.event(var=c("vbsc","voxels","vessel")). Should only contain one ping/timestep.
#' @param N  is the approximate number of points plotted.
#' @param acca  is the acoustic scattering cross sectional area (assuming omnidirectional targets). Overriding 'N' if given.
#' @param fun  is the name of the funciton used for obtaining the number of points in each voxel by "rounding off" to integers. The string given in 'fun' must be the name of a function taking 'x' as the input, such as "floor", "round", "ceiling" and "mod", where the last function is defined in sv2pos() and draws between floor(x) and ceiling(x) with probability P(ceiling(x))=x\%\%1. Also, there is the option of adding a numeric to "mod", such as "mod3" or "mod4.2", to indicate that all voxels with number of points below the given numeric, are set to zero.
#' @param t  is a single integer giving the time step number of which to regenerate points in the case that data$vbsc has a third dimension indicating time. 't' must be given as a single integer in the range seq_len(dim(data$vbsc)[3]).
#' @param esnm  is the name of the acoustical instrument, given as a four character string. See sonR_implemented() for the implemented systems. May be given in 'data', and in lower case.
#' @param var  is a string specifying the variable to plot. Currently supported are "vbsc", for volume backscattering data, "sgsc"/"sgs0"/"sgsE"/"sgsi"/"sgsI", for thresholded segmentation data, "pr0s" or "psis" for unhtresholded not-school probability data, and "tlns" for pure estimated noise.
#' @param ind  is a list of indexes, as typed into the [] of an array, where 0 and NULL denotes all indexes.
#' @param range  is a list of elements with names matching names if 'data', specifying the range of the corresponding elements.
#' @param subset  is a numeric or logical vector/expression indicating elements or rows to keep. Missing values are taken as false, and subset=0 or subset=NULL indicates no subsetting.
#' @param beamstypes  is either a numeric or a string giving the variable to extract from the output from voxel.edge.TSD(), where "rect" denotes rectangular beams and "circ" denotes circular beams. Default is to select the first in the list, which is usually the rectangular beams if both rectangular and circualr are returned.
#' @param cs  is either "g" or "v" representing the global coordinate and the coordinate system of the vessel, in which the data are plotted.
#' @param ideal  is TRUE to represent the simple case where the speed of sound 'data$asps' is invariant of depth.
#' @param allert  is the limit of the number of points inducing a warning asking the user to continue. Default is NULL, indicating no check for the number of points to be generated.
#' @param plot  TRUE if the data should be plotted.
#' @param seabed  is the depth of the seabed, at which the beams are reflected when calculating the midpoints of voxels.
#' @param rot  see soundbeam.TSD().
#' @param compensation  is a vector of string giving which rotation values that are compensated for in the sonar. Only c("pitch","roll") is available for the current version. Used in soundbeam.TSD.
#' @param nlim  is the maximum number of points in a voxel, before the number of points is recalculated to n*fact.
#' @param fact  is either NULL, indicating that points in voxels where the number of points is larger than 'nlim' are to be returned as they are, or the factor by which to scale the number of points in voxels exceeding the value of 'nlim'.
#' @param cols  is a two element vector specifying the color of the normal points and the points that are generated in voxels in which n>nlim.
#' @param stretch  is used to stretch the voxels of the ME70 multibeam echosounder in the direction of motion, so that space in between voxels is smoothed out.
#' @param scale.voxels  is a factor by which to enlarge the extent of each spherical element inside which points are drawn. If scale.voxels=2, the radial positions are drawn from -0.5 to 1.5 of the extent of the radial boundaries, so that point extend into the previous and next radial interval. The same applies to all three directions.
#' @param rand.gen  is either a string naming the random generating function to use when drawing positions inside the spherical segments, or a (symetrical) function. If rand.gen[1]=="beta", the symmetrical beta function is used (shape1=shape2=2).
#' @param total.out  is TRUE if ONLY the total backscatter (calculated as the sum of the products of volume backscattering coefficient and volume) should be returned, and the function terminated before regeneration of points.
#' @param ...  are inputs passed on to plot3d().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl aspect3d plot3d
#' @importFrom sonR extractTimeStep get.specs.esnm rotate3D runif.sph voxel.edge.TSD
#' @importFrom TSD dim_all ind.expand labl.TSD mod
#' @importFrom stats rbinom
#'
#' @export
#' @rdname pplot3d_sv2pos.TSD
#'
pplot3d_sv2pos.TSD<-function(data, N=1e5, acca=NULL, fun="mod", t=1, esnm="MS70", var=c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns"), ind=list(-(1:100),NULL), range=list(), subset=NULL, beamstypes=1, cs="g", ideal=TRUE, allert=NULL, plot=TRUE, seabed=-12000, rot=1, compensation=c("pitch","roll"), nlim=NULL, fact=NULL, cols=c("black","navyblue","magenta4","red3","darkorange2","yellow"), stretch=1, scale.voxels=1, rand.gen=c("unif","beta","norm"), total.out=FALSE, ...){
		
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2010-05-08 - Clean version.
	# Update: 2010-12-08 - Added the option of regenerating points for larger objects in voxels where n>nlim.
	# Update: 2010-08-25 - Changed the subsetting of the data to include voxels, vessel and vbsc.
	# Update: 2011-09-29 - Fixed bug when using 'rect' too much.
	# Update: 2012-05-16 - Changed from using 'bigcol' for the color of the points representing many fish, to 'cols', which is a two element color vector holding both the color of the normal points and the big points.
	# Update: 2012-06-02 - Added the parameter 'N', which if given defines the approximate number of points to plot. A suited value is 1e5.
	# Update: 2013-01-05 - Added the option var="tlns".
	# Update: 2013-02-28 - Added several levels of points, to be plotted with different colores.
	# Update: 2013-03-11 - Added fun="mod3" and "mod2.7" and so on.
	# Update: 2013-08-08 - Simplified.
	# Update: 2013-08-08 - Restructured the calculation of 'n' and the separation into levels, and fixed the treatment of 'acca' so that if only 'N' is given, 'acca' is now calculated more accurately.
	# Update: 2013-09-12 - Added the parameters 'scale.voxels' and 'rand.gen'.
	# Update: 2014-11-12 - Added remaining points from higher levels.
	# Last: 2015-04-24 - Added the function extractTimeStep().


	
	##################################################
	##################################################
	##### Preparation #####
	# Function used for distributing voxels into classes defined by the number of points in each voxel:
	getn = function(data,nlim,acca,fact,fun){
		# Get the number of points in each voxel:
		n = do.call(fun, list(x=data$vbsc*data$volx/acca))
		# Declare the outputs:
		finaln = vector("list",length(nlim))
		finallevelat = vector("list",length(nlim))
		finalN = 0
		# Loop thorugh the intervals defined by 'nlim' and split 'n' into levels ('nlim' has been added 0 at the beginning):
		for(i in rev(seq_along(finaln))){
			# Get the indices of the voxels that have ist highest level as the current level:
			finallevelat[[i]] = which(n>=nlim[i])
			if(length(finallevelat[[i]])){
				# If we have not reached the lowest level:
				if(i>1){
					# Get the rest of the points to bring to the next and lower level:
					rest = n[finallevelat[[i]]] %% nlim[i]
					# Get the number of points to plot in the current level, and add the sum of these to the total number of points:
					finaln[[i]] = do.call(fun, list(x=(n[finallevelat[[i]]] - rest) * fact[i]))      
					finalN = finalN + sum(finaln[[i]],na.rm=TRUE)
					# Update 'n' with the rest of the points:
					n[finallevelat[[i]]] = rest
					}
				# If we do have reached the lowest level, close the loop by assigning the remaining points to the voxels that has fewer points than the lowest limit:
				else{
					finaln[[i]] = n[finallevelat[[i]]]
					finalN = finalN + sum(finaln[[i]],na.rm=TRUE)
					}
				}
			}
		list(n=finaln, levelat=finallevelat, N=finalN)
		}
	
	# Default 'nlim':
	nlim = pplot3d.set.default.nlim(nlim=nlim,var=var,esnm=esnm)
	# Default is to reduce the number of points by factors inverse to the square root of 'nlim':
	if(is.function(fact)){
		fact = fact(nlim)
		}
	if(length(fact)==0){
		fact = 1/sqrt(nlim)
		}
	if(length(fact)!=length(nlim)){
		if(length(fact)<length(nlim)){
			fact = c(fact[1]^(seq_len(length(nlim)-1)))
			warning(paste("The scaling factors for high values, 'fact', too short. Set to",paste(fact,collapse=", ")))
			}
		else{
			fact = c(fact[seq_len(length(nlim)-1)])
			#warning(paste("The scaling factors for high values, 'fact', too long, and was cropped to", paste(fact,collapse=", ")))
			}
		}
	if(any(is.infinite(fact))){
		warning("Some values of the scaling factors for high values, 'fact', are infinite, and are set to 0 (no points plotted for the corresponding level)")
		fact[is.infinite(fact)] = 0
		}
	if(any(fact>1)){
		stop("The scaling factors for high values, 'fact', must be smaller than 1")
		}
	
	# Add 0 to 'nlim' to ease the code:
	nlim = c(0,nlim)
	fact = c(1,fact)
	# If the color vector is shorter than 'nlim' and 'fact', let it define the number of levels of the plot:
	if(length(cols)<length(fact)){
		#warning(paste("The color vector'cols' (",length(cols),") is shorter than 'fact' (",length(fact),"), causing 'fact' and 'nlim' to be cropped to the length of 'cols'",sep=""))
		nlim = nlim[seq_along(cols)]
		fact = fact[seq_along(cols)]
		}
	
	# Read the name of the acoustic device:
	if(!is.null(data$esnm)){
		esnm = data$esnm
		}
	# Get the number of beams if missing:
	if(length(data$numb)==0 && any(length(data$dira)>0,length(data$dire)>0)){
		data$numb = max(length(data$dira),length(data$dire))
		}
	# Assure that the required variables are present:
	requiredVariables = c("psxv","psyv","pszv","rtzv","psze","volx","esnm","dira","dire","lenb","eqba")
	nullOutput = list(regen=list(), acca=NA, n=list(), N=0, nlim=nlim, fact=fact)
	if(!(all(requiredVariables %in% names(data)) & any(labl.TSD(var="as") %in% names(data)))){
		warning(paste("Vessel positions 'psxv', 'psyv', 'pszv', 'rtzv', voxel volumes 'volx', beam configuration variables 'esnm', 'dira', 'dire', 'lenb', 'eqba',  and at least one of volume backscattering coefficient 'vbsc', mean volume backscattering coefficient 'mvbs', or segmentation data 'sgsc'/'sgs0'/'sgsE', must be present in 'data'. Present variables are: ",paste(sort(intersect(names(data),c(requiredVariables,labl.TSD(var="as")))),collapse=", ") ,sep=""))
		return(nullOutput)
		}
	# Remove unnecessary information:
	if(!"psxx" %in% names(range)){
		data$psxx = NULL
		}
	if(!"psyx" %in% names(range)){
		data$psyx = NULL
		}
	if(!"pszx" %in% names(range)){
		data$pszx = NULL
		}
	data$nrns = NULL
	
	# Select the specified time step:
	data = extractTimeStep(data, t, var=c("vbsc","mvbs",  "psxv","psyv","pszv","rtzv",  "lenb","dira","dire","eqba","bmmd","bwtx","bwty",  "asps","sint","numb","esnm",  labl.TSD(var="sd")))
			
	# Get the edges of the voxels (rectangular or circular) before subsetting the data:
	#names(formals(voxel.edge.TSD))
	suppressWarnings(rthetaphi<-voxel.edge.TSD(data, esnm=esnm, seabed=seabed, rot=rot, compensation=compensation, ideal=ideal, stretch=stretch, ...)[[beamstypes]])
	## Restrict voxels to not extend above the sea surface:
	#rthetaphi[,5][rthetaphi[,5]<pi/2] = pi/2
	
	# Extract the specified variable to plot:
	data$vbsc = cpplot3d.extract_var(data, var)
	vbscpresent = length(data$vbsc)>0
	
	# Expand 'ind' to fit the dimensions of the acoustic data:
	ind = ind.expand(ind, dim(data$vbsc), ...)
	specs = get.specs.esnm(data,ind=ind,beamstypes=beamstypes)[c("ind","offset")]
	ind = specs$ind
	
	# Intersect 'ind' with 'range' and 'subset', creating a single vector of indices:
	ind_flat = sv2pos_get_ind(data,ind=ind,range=range,subset=subset)
	
	# Apply the subsets:
	if("vbsc" %in% names(data)){
		data$vbsc = data$vbsc[ind_flat]
		}
	if("mvbs" %in% names(data)){
		data$mvbs = data$mvbs[ind_flat]
		}
	if("volx" %in% names(data)){
		data$volx = data$volx[ind_flat]
		}
	rthetaphi = rthetaphi[ind_flat,,drop=FALSE]
	# Abort with a warning if no 'vbsc' remain in the data:
	if(length(data$vbsc)==0){
		if(vbscpresent){
			warning("No data plotted, possibly due to empty segmentation mask")
			return(nullOutput)
			}
		else{
			warning("No data plotted, possibly due to full exclusion of the data using 'range', 'ind', or 'subset'")
			return(nullOutput)
			}
		}
	
	# Linearize afterwards:
	if(length(data$vbsc)==0 && length(data$mvbs)>0){
		data$vbsc = 10^(data$mvbs/10)
		}
	# If data$vbsc is complex, take the Mod():
	if(is.complex(data$vbsc)){
		data$vbsc = array(Mod(data$vbsc),dim=dim(data$vbsc))
		}
	# If data$vbsc is logical, transform to numeric:
	if(is.logical(data$vbsc)){
		data$vbsc = as.numeric(data$vbsc)
		}
	
	# Return volume of the segmented school:
	if(var[1] %in% c("sgsc","sgs0","sgsE")){
		cat("Volume of segmented school:",sum(data$vbsc * data$volx, na.rm=TRUE),"\n")
		}
	# Return the total echo if requested:
	if(total.out){
		return(sum(c(data$vbsc)*c(data$volx),na.rm=TRUE))
		}
	
	
	##### Execution #####
	# Define the function mod(), drawing between floor(x) and ceiling(x) with probability P(ceiling(x))=x%%1:
	mod = function(x){
		floor(x)+suppressWarnings(rbinom(length(x),1,x %% 1))
		}
	# A thresholded version of mod():
	modTrh = function(x,thr){
		x[x<thr] = 0
		floor(x)+suppressWarnings(rbinom(length(x),1,x %% 1))
		}
	if(length(grep("mod",fun))>0){
		thr = as.numeric(substring(fun,4))
		if(!is.na(thr)){
			fun = function(x) modTrh(x,thr)
			}
		}
		
	# Set the value of acca if missing, so that it approximately corresponds to the value of N:
	if(length(acca)==0){
		acca = 0.1
		this = getn(data,nlim,acca,fact,fun)
		# If the value of 'acca' is too low to return any points, reduce the value by 1e10:
		if(this$N==0){
			acca = acca*1e-10
			this = getn(data,nlim,acca,fact,fun)
			}
		# Divide by (log10(10+sum(fact*nlim,na.rm=TRUE))/10) to make the end result of 'acca' more close to the value that will produce the desired number 'N' of points. This was found by testing to result in the desired N for moderate steps of 'nlim' (nlim=30^(1:10)), and underestimating for shorter steps and overestimating for larger steps, and will also be dependent on the data.
		acca = acca*this$N/N / (log10(10+sum(fact*nlim,na.rm=TRUE))/10)
		# Recalculate for the new 'acca':
		this = getn(data,nlim,acca,fact,fun)
		acca = acca*this$N/N
		}
		
	# Get the number of points in each voxel and divide into levels:
	n = getn(data,nlim,acca,fact,fun)
	# The total number of points, with a chech for memory:
	if(length(allert)>0 && n$N>allert){
		ask = identical("y",tolower(readline(paste(n$N," positions will be generated. Continue? ('y')"))))
		if(!ask){
			return()
			}
		}
	
	# Draw the points:
	regen = vector("list",length(nlim))
	for(i in rev(seq_along(nlim))){
		if(length(n$n[[i]])>0){
			regen[[i]]<-runif.sph(n$n[[i]], r=rthetaphi[n$levelat[[i]],1:2], theta=rthetaphi[n$levelat[[i]],3:4], phi=rthetaphi[n$levelat[[i]],5:6], sph.out=FALSE, offset=specs$offset, scale.voxels=scale.voxels, rand.gen=rand.gen, reverse=TRUE)
			}
		}
		
	# Drop levels with no points if there are no levels with points above:
	finalNatlevels = unlist(lapply(dim_all(regen),sum))
	if(any(finalNatlevels>0)){
		lastnonemptylevel = max(which(finalNatlevels>0))
		regen = regen[seq_len(lastnonemptylevel)]
		}
	
	# Transform to the global coordinate system if requested:
	if(tolower(cs)=="g"){
		# (1) All points or (2) Points in voxels where n<=nlim:
		for(i in rev(seq_along(regen))){
			if(length(regen[[i]])>0){
				regen[[i]] = rotate3D(regen[[i]], by="z", ang=-data$rtzv[1], drop.out=FALSE)
				regen[[i]][,1,] = regen[[i]][,1,]+data$psxv[1]
				regen[[i]][,2,] = regen[[i]][,2,]+data$psyv[1]
				regen[[i]][,3,] = regen[[i]][,3,]+(data$pszv[1]+data$psze[1])
				# Restrict voxels to not extend above the sea surface:
				regen[[i]][,3,][regen[[i]][,3,]>0] = 0
				# Remove the last dimension:
				if(length(regen[[i]])>0){
					dim(regen[[i]]) = dim(regen[[i]])[-3]
					}
				}
			}
		}
	# Add the sonar position in the coordinate system of the vessel:
	else if(tolower(cs)=="v"){
		for(i in rev(seq_along(regen))){
			if(length(regen[[i]])>0){
				regen[[i]][,3] = regen[[i]][,3]+data$psze[1]
				# Restrict voxels to not extend above the sea surface:
				regen[[i]][,3][regen[[i]][,3]>0] = 0
				}
			}
		}
	else{
		warning("'cs' should be one of \"g\" (global) or \"v\" (vessel). Defaulted to global")
		}
		
	
	##### Output #####
	# Plot in 3D if required:
	if(plot){
		ll = list(...)
		for(i in rev(seq_along(regen))){
			if(length(regen[[i]])>0){
				thisl = list(x=regen[[i]],xlab="x",ylab="y",zlab="z",col=cols[i])
				otherl = ll[setdiff(names(ll),names(thisl))]
				do.call("plot3d",c(thisl,otherl))
				}
			}
		aspect3d("iso")
		}
		
	invisible(list(regen=regen,acca=acca,n=n$n,N=n$N,nlim=nlim,fact=fact))
	##################################################
	##################################################
	}
