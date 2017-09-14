#*********************************************
#*********************************************
#' 2-D echogram.
#'
#' @param data  is a list containing the data to plot. Must contain the sv field 'vbsc', or the Sv field 'mvbs', the sampling interval field 'sint' and the average speed of sound 'asps'.
#' @param breaks  has two possible inputs: (1) the number of breaks of the scale on which the data 'z' are arranged, equally spaced between min(z) and max(z) (or logarithmically equaly spaced when log=TRUE). (2) a vector of values for the breaks given in dB values (volume backscattering strength Sv).
#' @param col  is either the color vector if length(col)>1, or the color function to generate the colors from. Currently the color function name must be one of "rainbow", "grey", "heat.colors", "terrain.colors", "topo.colors" and "cm.colors", but other color functions may be inplemented in the future. Set color.bar=NULL to supress plotting the color bar.
#' @param colpar  is a list of parameters used in colscale().
#' @param log  is TRUE if log transformed data are to be plotted. If so, all values must be positive.
#' @param zlim  is is the depth range (negative values).
#' @param tlim  is the time range.
#' @param z  is the z-position of the echosounder (negative when below the sea surface, 0 when ).
#' @param up  should be set to TRUE for upwards oriented echosounders.
#' @param freq  is the frequency to plot.
#' @param endcol  is the color to use for values above the highest break value.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom fields image.plot
#' @importFrom grid grid.raster
#' @importFrom SimradRaw soundbeam_range
#' @importFrom sonR compr.TSD get.specs.esnm is.sonar medSmooth1 rotate3D subset_TSD
#' @importFrom TSD ftim2list ftim2utim ind.expand strff utim.TSD utim2ftim do.callUnique
#' @importFrom utils tail
#' @importFrom stats approx
#' @importFrom graphics axis par segments image title
#'
#' @export
#' @rdname cplot2d.TSD
#'
cplot2d.TSD<-function(
	# Main variable:
	data, t="all", 
	# Used in cplot2d.TSD():
	breaks=40, col="combined", colpar=list(start=0,end=0.8,flip=TRUE), null.value=NA, beamstypes=1, grid=TRUE, adds=NULL, xlim=NULL, ylim=NULL, zlim=NULL, tlim=NULL, up=FALSE,  freq=1, wb=1, rmar=5, xaxis=c("time", "dist", "pings", "compr", "sparse"), gap=median, gapthr=10, tol=0.1, heave=c("interp", "pixel", "ignore"), x0=NULL, unit=NULL, date=c("unique", "all", "none"), nticksx=10, 
	# Used in cplot3d.plot.color.bar():
	white=0, log=TRUE, endcol=c("white", ""), 
	# Used in subset_TSD and elsewhere:
	esnm="MS70", var=c("vbsc","sgsc","pr0s","sgs0","sgsE","sgsi","sgsI","psis","tlns"), ind=list(), range=list(), subset=NULL, plot=TRUE, 
	# Used when plotting date and time:
	clock=NULL, cex.clock=1, format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, col.clock=4, 
	# Passed on to image():
	...){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-08-26 - Clean version.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Funciton used for adding an as large as possible value to identical values, in order to obtain a strictly increasing vector. This work is not finished, but works fairly good. The problem arrises when there are gaps, where the last artificial ping inserted to generate the void of data is positioned too soon, causing very long pixles at the end of a void!!!!!!!!!!!!!!!!!!!!!!!!!!!
	addToEqual <- function(x){
		numOfEach <- table(x)
		#diffUnique <- diff(unique(x))
		#diffUnique <- c(diffUnique, tail(diffUnique, 1))
		#diffUnique <- diffUnique / numOfEach
		#add <- rep(diffUnique, numOfEach)
		add <- min(diff(unique(x))) / max(numOfEach)
		x <- x + (sequence(numOfEach) - 1) * add
		names(x) <- NULL
		x
	}
	# Function used to check if the data to be used on the x axis are gridded, at least to some extent:
	is.gridded <- function(x, pos=median, tol=0.1, gridthr=0.99, discardHeadTail=TRUE, useAbs=TRUE, ...){
		x <- diff(x)
		if(discardHeadTail){
			x <- x[-c(1, length(x))]
		}
		if(length(x)==0){
			return(TRUE)
		}
		else if(any(x==0)){
			return(FALSE)
		}
		ref <- pos(x, ...)[1]
		if(useAbs){
			out <- mean(abs(x - ref) / abs(ref) < tol) > gridthr
			if(!out){
				cat("Maximum absolue fractional deviation from the ", deparse(substitute(pos)), " of the diffs of the variable on the x axis: ", max(abs(x - ref) / abs(ref)), "\n")
			}
		}
		else{
			out <- mean((x - ref) / ref < tol) > gridthr
			if(!out){
				cat("Maximum fractional deviation from the ", deparse(substitute(pos)), " of the diffs of the variable on the x axis: ", max(abs(x - ref) / abs(ref)), "\n")
			}
		}
		return(out)
	}
	
	# These functions fills in data in new arrays of NAs, so that gaps in time are filled with NAs:
	
	############# step is unfinished. Aslo the line #(out[atFillStartEnd[,2]] - out[atFillStartEnd[,1]]) / (atFillStartEnd[,2]- atFillStartEnd[,1]) is an attempt to fill the gaps perfectly ##########
	fillOne <- function(x, atData, n, step, what=c(NA, "last", "interp")){
		if(length(atData)==n){
			return(x)
			}
		dims <- if(is.list(x)) length(x) else dim_all(x)
		dimUpToLast <- dims[-length(dims)]
		dimLast <- tail(dims, 1)
		if(length(x)==0){
			return(x)
			}
		else if(dimLast < length(atData)){
			return(x)
			}
		out <- NAs(c(dimUpToLast, n))
		##########
		if(length(dims)==0){
			out <- x
			}
		else if(length(dims)==1){
			out[atData] <- x
			}
		else if(length(dims)==2){
			out[,atData] <- x
			}
		else if(length(dims)==3){
			out[,,atData] <- x
			}
		else{
			stop("Funciton fillOne() not designed for more than 3-way arrays.")
			}
		##########
		
		
		startEnd <- function(x, step=1){
			d <- diff(x)
			jump <- which(d>step)
			start <- c(1, jump + 1)
			end <- c(jump, length(x))
			cbind(start=start, end=end)
		}
		
		if(any(c("last", "interp") %in% what[1])){
			atFill <- seq_len(n)[-atData]
			atFillStartEnd <- startEnd(atFill)
			# For pings starting with NA, use the first non-NA:
			atFill2 <- suppressWarnings(sapply(atFill, function(xx) max(min(atData), max(atData[atData<xx]))))
			fillWidths <- atFill - atFill2
			if(identical("last", what[1])){
				if(length(dims)==1){
					out[atFill] <- out[atFill2]
					}
				else if(length(dims)==2){
					out[,atFill] <- out[,atFill2]
					}
				else if(length(dims)==3){
					out[,,atFill] <- out[,,atFill2]
					}
				}
			if(identical("interp", what[1])){
				if(is.numeric(x) && length(atData)>1){
					if(length(dims)==1){
						step <- (out[atData[2]] - out[atData[1]]) / (atData[2]-atData[1])
						
						#(out[atFillStartEnd[,2]] - out[atFillStartEnd[,1]]) / (atFillStartEnd[,2]- atFillStartEnd[,1])
						
						out[atFill] <- out[atFill2] + step * fillWidths
						}
					else if(length(dims)==2){
						step <- (out[,atData[2]] - out[,atData[1]]) / (atData[2]-atData[1])
						out[,atFill] <- out[,atFill2] + outer(step, fillWidths)
						}
					else if(length(dims)==3){
						step <- (out[,,atData[2]] - out[,,atData[1]]) / (atData[2]-atData[1])
						out[,,atFill] <- out[,,atFill2] + outer(step, fillWidths)
						}
					}
				}
			}
		out
	}
	fill <- function(x, atData, n, step, what=c(NA, "last", "interp")){
		if(is.list(x)){
			lapply(x, fillOne, atData=atData, n=n, step=step, what=what)
		}
		else{
			fillOne(x, atData=atData, n=n, step=step, what=what)
		}
	}
	detectAndFill <- function(data, var=NULL, what=c(NA, "last", "interp"), nd=2, gap=median, gapthr=10, xaxis="p", minthr=1e-6){
		if(length(var)==0){
			var <- names(data)
			}
		
		##### Sailed distance along x-axis:
		if(any(strff(c("x", "d"), xaxis[1]))){
			if(length(data$sadv)==0){
				warning("Sailed distance (vessel log) missing. Use xaxis = \"pings\" or \"time\" instead.")
				return(data)
			}
			x <- data$sadv
		}
		##### Time along x-axis:
		else if(strff("t", xaxis[1])){
			x <- utim.TSD(data)
		}
		##### Pings along x-axis:
		else if(any(strff(c("p", "c"), xaxis[1]))){
			x <- data$indt
		}
		
		# Get the median x diff (rounded to nd=3 digits), and check that at least 'gapthr' of the diffs are equal to the median:
		if(length(x)==1){
			return(data)
			}
		#diffx <- round(diff(x), digits=nd)
		diffx <- diff(x)
		
		if(is.function(gap)){
			gap <- gap(diffx[diffx>minthr])
		}
		if(gap==0){
			return(data)
		}
		diffx <- c(gap, diffx)
		
		# Get the gaps:
		#gaps <- pmax(0, round(diffx/gap - 1))
		gaps <- pmax(0, diffx/gap - 1)
		gaps[gaps < (gapthr-1)] <- 0
		gaps <- round(gaps)
		
		sumgaps <- sum(gaps)
		if(sumgaps>0){
			numtNew <- length(x) + sumgaps
			atData <- cumsum(gaps+1)
			data[var] <- fill(data[var], atData=atData, n=numtNew, step=gap, what=what)
			}
		
		#if(mean(diffx==gap) < gapthr){
		#	# Get the gaps:
		#	gaps <- ceiling(diffx/gap - 1)
		#	#atGaps <- which(gaps>0)
		#	sumgaps <- sum(gaps)
		#	numtNew <- length(x) + sumgaps
		#	atData <- cumsum(gaps+1)
		#	data[var] <- fill(data[var], atData=atData, n=numtNew, what=what)
		#	}
		data
	}
	
	getUnits <- function(xaxis, unit=NULL, x=NULL){
		# If distance is along the x axis:
		if(any(strff(c("x", "d"), xaxis[1]))){
			# Define valid units:
			validUnits <- c("m", "km", "nmi")
			validUnitsWithParentheses <- paste0(" (", validUnits, ")")
			validUnitsNum <- c(1, 1e3, 1852)
			
			# Select the valid unit:
			selected <- NULL
			if(length(unit)){
				if(any(validUnits %in% unit)){
					selected <- which(validUnits %in% unit)
				}
				else{
					warning(paste0("'unit' not matching any of the valid units (", validUnits, ")"))
				}
			}
			# To use the defalut unit, get the maximum absolute value, and use this to determine the default as km if this exceeds 2000 and m otherwise:
			if(length(selected)==0){
				mx <- max(diff(x))
				selected <- 1
				if(mx > 2*validUnitsNum[2]){
					selected <- 2
				}
			}
			return(list(unit=validUnits[selected], unitPrint=validUnitsWithParentheses[selected], unitNum=validUnitsNum[selected]))
		}
		else if(strff("t", xaxis[1])){
			return(list(unit=if(length(unit) && length(grep("%", unit, fixed=TRUE))) unit else "%Y-%m-%d\n%H:%M:%S", unitPrint="", unitNum=NULL))
		}
		else if(any(strff(c("p", "c"), xaxis[1]))){
			return(list(unit="", unitPrint="", unitNum=NULL))
		}
	}
	
	plotxaxis <- function(xaxis, unit=NULL, x=NULL, x0=NULL, date=c("unique", "all", "none"), ...){
		unit <- getUnits(xaxis=xaxis, unit=unit, x=x)
		# If distance is along the x axis:
		if(any(strff(c("x", "d"), xaxis[1]))){
			if(length(x0)==0){
				x0 <- 0
			}
			else if(any(strff(c("s", "f"), x0))){
				x0 <- x[1] / unit$unitNum
			}
			
			xnew <- x - x0 * unit$unitNum
			
			# Add axis at the pretty units:
			x_unit <- xnew / unit$unitNum
			labels <- pretty(x_unit, n=nticksx)
			at <- labels * unit$unitNum + x0 * unit$unitNum
			axis(1, at=at, labels=labels, ...)
		}
		else if(strff("t", xaxis[1])){
			if(length(x0)>0){
				warning("x0 not yet implemented for time on x axis")
			}
			x_unit <- as.POSIXlt(x, origin="1970-01-01", tz="GMT")
			labels <- pretty(x_unit, n=nticksx)
			at <- unclass(labels)
			# Discard those outside of the ploting region:
			outside <- at < min(x) | at > max(x)
			labels <- labels[!outside]
			at <- at[!outside]
			
			# If date=="unique", strip the labels of duplicated dates:
			if(any(nchar(as.character(labels))>15)){
				removeDate <- function(x, dateind){
					atH <- regexpr("%H", x[!dateind])
					atM <- regexpr("%M", x[!dateind])
					atS <- regexpr("%S", x[!dateind])
					atH[atH==1] <- Inf
					atM[atM==1] <- Inf
					atS[atS==1] <- Inf
					atH <- pmin(atH, atM, atS)
					x[!dateind] <- substring(x[!dateind], atH)
					trimws(x)
				}
				
				dateind <- logical(length(labels))
				if(strff("a", date[1])){
					dateind <- seq_along(labels)
				}
				else if(strff("u", date[1])){
					dateind <- !duplicated(format(labels, format="%Y-%m-%d"))
				}
				unit$unit <- removeDate(rep(unit$unit, length.out=length(labels)), dateind=dateind)
			}
			if(any(nchar(as.character(labels))>15) && !"padj" %in% names(list(...))){
				padj <- 0.5
				}
			else{
				padj <- NA
				}
			axis(1, at=at, labels=format(labels, format=unit$unit), padj=padj, ...)
		}
		else if(any(strff(c("p", "c"), xaxis[1]))){
			at <- pretty(x, n=nticksx)
			#axis(1, n=nticksx, ...)
			axis(1, at=at, ...)
		}
	}
	
		
	# Function used for plotting image using grid.raster() which is faster than image():
	#image_grid.raster <- function(data, colpar, colvec, endcol, breaks, x, z, xlab="Sailed distance (nmi)", ylab="Depth", xlim=NULL, ylim=NULL, ...){
	image_grid.raster <- function(data, colpar, colvec, endcol, breaks, x, z, xlim=NULL, ylim=NULL, interpolate=FALSE, ...){
		cat("Using grid.raster...\n")
		# Zoom into the data:
		validx <- which(x >= min(xlim) & x <= max(xlim))
		validz <- which(z >= min(zlim) & z <= max(zlim))
		x <- x[validx]
		z <- z[validz]
		data$vbsc <- data$vbsc[validz, validx]

		# Find the breaks of the data:
		imvbsc <- findInterval(data$vbsc, breaks, rightmost.closed=TRUE)
		imvbsc <- colvec[imvbsc]
		dim(imvbsc) <- dim(data$vbsc)
		
		# Flip the data due to how images are plotted with grid.raster():
		imvbsc <- imvbsc[seq(nrow(imvbsc), 1),]
		
		# Add a small portion to xlim and ylim to get the start and end positions to fith with the limits when plotting (since grid.raster() plots at the center of each pixel):
		rangex <- range(x, na.rm=TRUE)
		addx <- diff(rangex)/(length(x)-1)/2
		rangex <- rangex + c(-1,1)*addx
		rangez <- range(z, na.rm=TRUE)
		addz <- diff(rangez)/(length(z)-1)/2
		rangez <- rangez + c(-1,1)*addz
		
		# Set the limits to the ranges if differing from the ranges by less or equal to addx/addz:
		diffXlim2range_x <- xlim - rangex
		diffXlim2range_z <- zlim - rangez
		if(diffXlim2range_x[1] > 0 && diffXlim2range_x[1] <= addx*1.1){
		   xlim[1] <- rangex[1]
		   }
		if(diffXlim2range_x[2] < 0 && diffXlim2range_x[2] >= -addx*1.1){
		   xlim[2] <- rangex[2]
		   }
		if(diffXlim2range_z[1] > 0 && diffXlim2range_z[1] <= addz*1.1){
		   zlim[1] <- rangez[1]
		   }
		if(diffXlim2range_z[2] < 0 && diffXlim2range_z[2] >= -addz*1.1){
			zlim[2] <- rangez[2]
		}
		
		# Plot the bounding box:
		plot(NULL, xlim=xlim, ylim=zlim, xaxs="i", yaxs="i",  ...)
		# Get the plotting region:
		plt <- par()$plt
		fig <- par()$fig
		# Adjust if there e.g. are several plots in the window:
		plt <- c(fig[1] + diff(fig[1:2]) * plt[1:2], fig[3] + diff(fig[3:4]) * plt[3:4])
		
		# Compensate for limits exceeding plotted points (add space around echogram). The min and max restrict to the figure region::
		width <- diff(plt[1:2])
		height <- diff(plt[3:4])
		plt[1] <- max(plt[1] + width * (rangex[1]-xlim[1])/diff(xlim), fig[1])
		plt[2] <- min(plt[2] + width * (rangex[2]-xlim[2])/diff(xlim), fig[2])
		plt[3] <- max(plt[3] + height * (rangez[1]-ylim[1])/diff(ylim), fig[3])
		plt[4] <- min(plt[4] + height * (rangez[2]-ylim[2])/diff(ylim), fig[4])
		width <- diff(plt[1:2])
		height <- diff(plt[3:4])
		x <- mean(plt[1:2])
		y <- mean(plt[3:4])
		
		# Plot:
		grid.raster(imvbsc, width=width, height=height, x=x, y=y, interpolate=interpolate)
		return(list(imvbsc, width=width, height=height, x=x, y=y))
	}
	
	# Plot either the echogram, the color bar, or both:
	if(isTRUE(plot)){
		plot <- c("echogram", "colorbar")
	}
	else if(identical(plot[1], FALSE)){
		plot <- NULL
	}
	
	# Merge the 'adds' and the data:
	if(length(adds)>0){
		data[names(adds)] <- adds
		}
	
	
	########## Execution ##########
	##### Preparations for plotting: #####
	# Extract the specified variable to plot:
	data$vbsc <- cpplot3d.extract_var(data,var)
	vbscpresent <- length(data$vbsc)>0
	
	# Expand 'ind' to fit the dimension of the acoustic data:
	ind <- ind.expand(ind, dim(data$vbsc), ...)
	ind <- get.specs.esnm(data, ind=ind, beamstypes=beamstypes, var="ind")$ind
	
	
	# Subset the data:
	data <- subset_TSD(data, ind=ind, range=range, subset=subset, ind.out=TRUE, drop=FALSE, insert.NA=TRUE)
	
	# Add a column of NAs to allow for a subset of 
	# Linearize:
	if(length(data$vbsc)==0 && length(data$mvbs)>0){
		data$vbsc <- 10^(data$mvbs/10)
		}
	
	# If data$vbsc is complex, take the Mod():
	if(is.complex(data$vbsc)){
		data$vbsc <- array(Mod(data$vbsc),dim=dim(data$vbsc))
		}
	
	# If data$vbsc is logical, transform to numeric:
	if(is.logical(data$vbsc)){
		data$vbsc <- as.numeric(data$vbsc)
		}
	
	# Abort with a warning if no 'vbsc' remain in the data:
	if(length(data$vbsc)==0){
		if(vbscpresent){
			warning("No data plotted, possibly due to empty segmentation mask")
			return(list())
			}
		else{
			warning("No data plotted, possibly due to full exclusion of the data using 'range', 'ind', or 'subset'")
			return(list())
			}
		}
	
	###
	# Log transfomation may enhace the appearence of the plot (set to the default on 2011-01-11):
	if(log){
		if(min(data$vbsc, na.rm=TRUE)<=0){
			# Set all non-postitve values to the minimum of the data (or .Machine$double.neg.eps if the miminum is not available) if null.value=="min.pos", and to 'null.value' otherwise:
			if(identical(null.value, "min.pos")){
				if(sum(data$vbsc>0, na.rm=TRUE)==0){
					null.value <- .Machine$double.neg.eps
					warning("No data in the specified subset and range are positive. All set to .Machine$double.neg.eps for logarithmic plot")
					}
				else{
					null.value <- min(data$vbsc[data$vbsc>0], na.rm=TRUE)
					if(identical(null.value, max(data$vbsc, na.rm=TRUE))){
						null.value <- .Machine$double.neg.eps
						}
					else{
						warning(paste("Non-positive input data set to null.value = ", format(null.value, scientific=TRUE, digits=3), " for logarithmic plot", sep=""))
						}
					}
				}
			data$vbsc[data$vbsc<=0] <- null.value
			}
		data$vbsc <- 10*log10(data$vbsc)
		}
	
	# Get the ranges of the inputs:
	rangevbsc <- c(min(data$vbsc, na.rm=TRUE), max(data$vbsc, na.rm=TRUE))
	
	# Treatment of breaks and color:
	breaks <- cplot3d.col_breaks(col=col, colpar=colpar, breaks=breaks, white=white, log=log, rangevbsc=rangevbsc, endcol=endcol)
	
	colvec <- breaks$col
	endcol <- breaks$endcol
	white <- breaks$white
	breaks <- breaks$breaks
	lbreaks <- length(breaks)-1
	if(length(breaks)>2){
		breaks[1] <- breaks[2]-diff(breaks[2:3])
		breaks[length(breaks)] <- breaks[length(breaks)-1]+diff(breaks[length(breaks)+seq(-2,-1)])
		}
		
	# If only the color bar sould be plotted, do it here anr return:
	if(length(plot)==1 && strff("c", plot)){
		image.plot(NULL, col=colvec, breaks=breaks, legend.only=TRUE, nlevel=length(breaks)*10, ...)
		return(list())
	}
	
	# Make room for the color scale:
	if(par()$mar[4]<rmar){
		par(mar=c(par()$mar[1:3], rmar))
		}
	
	# For the output (updated if echosounder is plotted):
	pszx=NULL
	
	if(strff("e", plot)){
		# Plot a onli sonar image:
		if(is.sonar(data)){
			dimvbsc <- dim(data$vbsc)
			if(!is.na(dim(data$vbsc)[3])){
				warning("Only the first ping plotted")
				data$vbsc <- data$vbsc[,,1]
				data$psxx <- data$psxx[,,1]
				data$psyx <- data$psyx[,,1]
				#dim(data$vbsc) <- dimvbsc[1:2]
				#dim(data$psxx) <- dimvbsc[1:2]
				#dim(data$psyx) <- dimvbsc[1:2]
				}
			
			data$vbsc <- t(data$vbsc)
			# Smooth across beams:
			data$vbsc <- medSmooth1(data$vbsc, w=wb)
			# Define the edge points for the line segments to be plotted:
			x0 <- t(data$psxx)
			beamseq <- seq_len(nrow(x0))
			beamseq1 <- c(beamseq[-1], beamseq[1])
			x1 <- x0[beamseq1,]
			y0 <- t(data$psyx)
			y1 <- y0[beamseq1,]
		
			# Rotate to get the voxels on axis:
			angdiffhalf <- diff(data$dira)[1]/2
			xy0 <- rotate3D(cbind(c(x0), c(y0), 0), by="z", ang=angdiffhalf)
			x0 <- xy0[,1]
			y0 <- xy0[,2]
			xy1 <- rotate3D(cbind(c(x1), c(y1), 0), by="z", ang=angdiffhalf)
			x1 <- xy1[,1]
			y1 <- xy1[,2]
		
			atcol <- findInterval(data$vbsc, breaks, all.inside=TRUE)

			if(length(xlim)==0){
				xlim <- range(c(x0, x1), na.rm=TRUE)
				}
			if(length(xlim)>0 && strff("t", xaxis[1])){
				warning("Specifying 'xlim' not effective when time is plotted in the x axis. Use tlim instead.")
				}
			if(length(ylim)==0){
				ylim <- range(c(y0, y1), na.rm=TRUE)
				}
			plot(NULL, xlim=xlim, ylim=ylim)
			lwd <- max(mean(par("din")) / dimvbsc[1] * 50, 1)
			segments(x0=x0, y0=y0, x1=x1, y1=y1, col=colvec[atcol], lwd=lwd, ...)
			#segments(x0=x0, y0=y0, x1=x1, y1=y1, col=colvec[atcol])
		}
		# Plot an echosounder echogram:
		else{
			cat("Number of pixels:", prod(dim_all(data$vbsc)), "\n")
		
			# Set default ylab:
			ylabDefault <- "Depth (m)"
	
			# Find the frequency:
			nfreq <- length(data$freq)
			freq <- freq[1]
			# This means that whole numbers lower than one tenth of the lowest frequency should be accepted as frequency numbers:
			freq_int_scale <- 10
			freq_index <- (is.numeric(freq) & freq<min(data$freq)/freq_int_scale) | is.integer(freq)
			if(!freq_index){
				if(nfreq>0 && length(freq)>0){
					freq <- which.min(abs(data$freq-freq[1]))
					}
				else if(length(freq)==0 || freq<1 || freq>nfreq){
					freq <- 1
					warning("Invalid frequency. First frequency chosen")
					}
				}
			# Select according to the frequency:
			if(length(data$sint)==nfreq){
				data$sint <- data$sint[freq]
				}
			if(length(data$asps)==nfreq){
				data$asps <- data$asps[freq]
				}
			if(length(data$freq)==nfreq){
				data$freq <- data$freq[freq]
				}
			if(length(dim(data$vbsc))==3){
				data$vbsc <- data$vbsc[,freq,]
				}
			
			# Define beams:
			data$lenb <- nrow(data$vbsc)
			lent <- ncol(data$vbsc)
			#dz <- soundbeam_range(data, pos="res")
			#dz <- data$sint * data$asps / 2
			
			
			# Try using compr.TSD() to distribute the data in depth bins of the same height as the rres variable, but starting from z=0:
			#if(!all(sapply(c("x", "y"), grep, grid))){
			#	zres <- NULL
			#	tres <- NULL
			#	if(!grep("x", grid)){
			#		tres <- 
			#		}
			#	if(!grep("y", grid)){
			#		if(length(data$rres)==0){
			#			# Get range resolution:
			#			data$rres <- data$asps[1] * data$sint[1]/2
			#			}
			#		zres <- data$rres[1]
			#		}
			#	}
			if(!all(data$psze[1]==data$psze)){
				if(length(data$rres)==0){
					# Get range resolution:
					data$rres <- data$asps[1] * data$sint[1]/2
					}
				suppressWarnings(data <- compr.TSD(data, zres=data$rres[1]))
				}
				
			# If the resolution of the vessel log is too low, there may be consecutive identical values. Thus add a small value to 'psxx' at those values which are duplicated:
			if( length(data$sadv) && any(strff(c("x", "d"), xaxis[1])) ){
				#data$sadv <- addToEqual(data$sadv)
				# Using approx did not work, since the gap was populated by an intpolated value:
				#dupsadv <- duplicated(data$sadv)
				#if(any(dupsadv)){
				#	atUnique <- which(!dupsadv)
				#	atUnique[length(atUnique)] <- length(data$sadv)
				#	data$sadv <- approx(atUnique, data$sadv[atUnique], seq_along(data$sadv))$y
				#}
			}
			
			# Detect gaps in the data for presumed gridded data (possibly obtained using the compression compr.event()), and fill these with NAs:
			#if(grid){
			#if(TRUE){
			if(!strff("s", xaxis[1])){
				atvbsc <- which(names(data)=="vbsc")
				# Fill in gaps for vbsc:
				data <- detectAndFill(data, var=atvbsc, gap=gap, gapthr=gapthr, xaxis=xaxis)
				# Fill in gaps for all variables other than vbsc, using interpolation:
				data <- detectAndFill(data, var=-atvbsc, what="interp", gap=gap, gapthr=gapthr, xaxis=xaxis)
			}
			
			#pszx <- (-1)^(!up) * c(dz/4, seq_len(lenb-1)*dz)
			pszx <- (-1)^(!up) * soundbeam_range(data, pos="mid")
			pszx <- data$psze[1] + pszx
			
			# If heave==TRUE, apply heave to the z-positions so that the acoustic data are shifted accordingly:
			if(strff("int", heave)){
				dz <- abs(median(diff(pszx)))
				if(!all(data$pszv==0)){
					for(i in seq_len(dim(data$vbsc)[2])){
						data$vbsc[,i] <- approx(pszx, data$vbsc[,i], pszx + data$pszv[i])$y
					}
				}
			}
			else if(strff("pix", heave)){
				dz <- abs(median(diff(pszx)))
				shift <- round(data$pszv/dz)
				if(!all(shift==0)){
					newdim <- olddim <- dim(data$vbsc)
					above <- max(0, shift)
					below <- max(0, -shift)
					newdim[1] <- olddim[1] + above + below
					temp <- NAs(newdim)
					oldseq <- seq_len(olddim[1]) + below
					for(i in seq_len(newdim[2])){
						temp[oldseq + shift[i],i] <- data$vbsc[,i]
					}
					data$vbsc <- temp
					pszx <- pszx + above * dz
					pszx <- c(pszx, min(pszx) - seq_len(above + below) * dz)
				}
			}
			
			# Defaults:
			mainDefault <- paste0("Frequency: ", round(data$freq * 1e-3), " kHz")
			interpolateDefault <- FALSE
			
			# Orient the data so that the coordinates are increasing, as required by image.plot(), this turns the data also in image_grid.raster():
			if(any(diff(pszx)<0)){
			#if(head(pszx, 1)>tail(pszx, 1)){
				pszx <- rev(pszx)
				data$vbsc <- data$vbsc[rev(seq_len(nrow(data$vbsc))), ]
				}
	
			# Fix of the problem with color scale spanning the entire range of the data (2014-09-23):
			# Set values above the upper break to the upper break + a small value:
			data$vbsc[data$vbsc>breaks[length(breaks)-1]] <- breaks[length(breaks)-1]+1
	
			# Set values below the lower break to NA:
			data$vbsc[data$vbsc<breaks[1]] <- NA
	
	
			########## Output ##########
			if(length(zlim)==0){
				zlim <- range(pszx, na.rm=TRUE)
				}
			
			############################################
			##### 1. Sailed distance along x-axis: #####
			############################################
			if(length(data$sadv) && any(strff(c("x", "d"), xaxis[1]))){
				data$sadv <- addToEqual(data$sadv)
				psxx <- data$sadv * 1852
				if(all(diff(psxx)<=0)){
					warning("All vessel log values equal to 0 (missing?). Data plotted with extremely small log distances.")
					psxx <- psxx + seq_along(psxx)*sqrt(.Machine$double.eps)
				}
				if(length(xlim)==0){
					xlim <- range(psxx, na.rm=TRUE)
				}
				else{
					xlim <- range(xlim, na.rm=TRUE)
				}
				
				xlabDefault <- paste0("Sailed distance", getUnits(xaxis=xaxis, unit=unit, x=psxx)$unitPrint)
				#diffx <- diff(psxx)
				if("echogram" %in% plot)
				if(grid && is.gridded(psxx, tol=tol, na.rm=TRUE, ...)){
					do.callUnique(image_grid.raster, args=list(data=data, colpar=colpar, colvec=colvec, endcol=endcol, breaks=breaks, x=psxx, z=pszx, xlab=xlabDefault, ylab=ylabDefault, xlim=xlim, ylim=zlim, xaxt="n", interpolate=interpolateDefault, main=mainDefault), ...)
				}
				else{
					do.callUnique(image, args=list(x=psxx, y=pszx, z=t(data$vbsc), breaks=breaks, col=colvec, xlab=xlabDefault, ylab=ylabDefault, xlim=xlim, ylim=zlim, xaxt="n", main=mainDefault), ...)
				}
				
				# Add appropriate x-labels:
				plotxaxis(xaxis=xaxis, unit=unit, x=psxx, x0=x0, ...)
			}
			
			#################################
			##### 2. Time along x-axis: #####
			#################################
			else if(strff("t", xaxis[1])){
				# Use utim:
				#t <- utim.TSD(data)[t]
				t <- utim.TSD(data)
				
				#date <- utim2ftim(t, format="yyyy-mm-dd")
				#time <- utim2ftim(t, format="HH:MM:SS")
				if(length(tlim)==0){
					tlim <- range(t, na.rm=TRUE)
				}
				else{
					if(is.numeric(tlim)){
						ranget <- range(t, na.rm=TRUE)
						tlim <- min(ranget) + diff(ranget) * range(tlim, na.rm=TRUE)
					}
					else{
						tlim <-  unclass(as.POSIXct(c(tlim)))
					}
				}
			
				#diffx <- diff(t)
				xlabDefault <- paste0("Time", getUnits(xaxis=xaxis, unit=unit, x=t)$unitPrint)
				#xlabDefault <- "Time "
				if(grid && is.gridded(t, tol=tol, na.rm=TRUE, ...)){
					do.callUnique(image_grid.raster, args=list(data=data, colpar=colpar, colvec=colvec, endcol=endcol, breaks=breaks, x=t, z=pszx, xlab=xlabDefault, ylab=ylabDefault, xlim=tlim, ylim=zlim, xaxt="n", interpolate=interpolateDefault, main=mainDefault), ...)
				}
				else{
					do.callUnique(image, args=list(x=t, y=pszx, z=t(data$vbsc), breaks=breaks, col=colvec, xlab=xlabDefault, ylab=ylabDefault, xlim=tlim, ylim=zlim, xaxt="n", main=mainDefault), ...)
				}
				
				# Add appropriate x-labels:
				plotxaxis(xaxis=xaxis, unit=unit, x=t, x0=x0, date=date, ...)
			}
			
			##################################
			##### 3. Pings along x-axis: #####
			##################################
			else if(any(strff(c("p", "c", "s"), xaxis[1]))){
				if(any(strff(c("c", "s"), xaxis[1]))){
					t <- seq_along(t)
				}
				else{
					t <- data$indt
				}
				if(length(tlim)==0){
					tlim <- range(t, na.rm=TRUE)
				}
				else{
					tlim <- range(tlim, na.rm=TRUE)
				}
				#diffx <- diff(t)
				xlabDefault <- paste0("Ping", getUnits(xaxis=xaxis, unit=unit)$unitPrint)
				#xlabDefault <- "Ping "
				if(grid && is.gridded(t, tol=tol, na.rm=TRUE, ...)){
					do.callUnique(image_grid.raster, args=list(data=data, colpar=colpar, colvec=colvec, endcol=endcol, breaks=breaks, x=t, z=pszx, xlab=xlabDefault, ylab=ylabDefault, xlim=tlim, ylim=zlim, xaxt="n", interpolate=interpolateDefault, main=mainDefault), ...)
				}
				else{
					do.callUnique(image, args=list(x=t, y=pszx, z=t(data$vbsc), breaks=breaks, col=colvec, xlab=xlabDefault, ylab=ylabDefault, xlim=tlim, ylim=zlim, xaxt="n", main=mainDefault), ...)
				}
				
				# Add appropriate x-labels:
				plotxaxis(xaxis=xaxis, ...)
			}
		
		# Add the color bar:
		if(strff("c", plot)){
			image.plot(NULL, col=colvec, breaks=breaks, legend.only=TRUE, nlevel=length(breaks)*10, ...)
		}
		
		if(length(clock)==0){
			if(is.sonar(data)) "bbl" else FALSE
		}
		add.clock(clock=clock, utim=unlist(utim.TSD(data)), indt=data$indt, cex.clock=cex.clock, format.clock=format.clock, digits.clock=digits.clock, col.clock=col.clock, D=2)
		lll <- list(...)
		title(main=if(length(lll$main)) lll$main else paste0("Frequency: ", round(data$freq * 1e-3), " kHz"))
		invisible(list(vbsc=10^(data$vbsc/10), indt=t, pszx=pszx))
		}
	}
	##################################################
	##################################################
	}
