#*********************************************
#*********************************************
#' Add time and date to an rgl plot.
#'
#' @param clock  is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
#' @param utim  is the unix time to plot.
#' @param indt  is the optional index number to add to the output.
#' @param cex.clock  is the numeric character expansion value for the clock.
#' @param adj.clock  is used in text3d() when plotting the date and time.
#' @param format.clock  is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\\n" apparing in 'format.clock' cause a line break.
#' @param digits.clock  is a numeric specifying the number of digits of the seconds part of the time.
#' @param lsp.clock  is the spacing factor for the line break, given as a multiple of the corresponding x-, y- or zlim. Default is 0.04.
#' @param col.clock  is the color of the plotted time and date.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom rgl par3d text3d
#' @importFrom TSD utim2ftim
#' @importFrom graphics legend
#'
#' @export
#' @rdname add.clock
#'
add.clock<-function(clock="bbl", utim=0, indt=NULL, cex.clock=1, adj.clock=c(0,0), format.clock="Ping: indt\nyyyy-mm-dd\nHH:MM:SS.FFF", digits.clock=2, lsp.clock=0.04, col.clock=4, D=3){

	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2013-06-17 - Clean version.
	# Last: 2014-05-27 - Updated to include 'indt'.
	########### DESCRIPTION: ###########
	# Add time and date to an rgl plot.
	########## DEPENDENCIES: ###########
	# 
	############ VARIABLES: ############
	# ---clock--- is the position of the clock timing the frames, either given as a vector of length 3 representing the coordinates of the clock, as FALSE for no clock, or as one of 
	#		"bbl" = bottom (minimum of z) bottom (minimum of y) left (minimum of x)
	#		"bbr" = bottom (minimum of z) bottom (minimum of y) right (maximum of x)
	#		"btl" = bottom (minimum of z) top (maximum of y) left (minimum of x)
	#		"btr" = bottom (minimum of z) top (maximum of y) right (maximum of x)
	#		"tbl" = top (maximum of z) bottom (minimum of y) left (minimum of x)
	#		"tbr" = top (maximum of z) bottom (minimum of y) right (maximum of x)
	#		"ttl" = top (maximum of z) top (maximum of y) left (minimum of x)
	#		"ttr" = top (maximum of z) top (maximum of y) right (maximum of x)
	# ---utim--- is the unix time to plot.
	# ---indt--- is the optional index number to add to the output.
	# ---cex.clock--- is the numeric character expansion value for the clock.
	# ---adj.clock--- is used in text3d() when plotting the date and time.
	# ---format.clock--- is the format of the date and time, specified as the input to utim2ftim(). Default is "yyyy-mm-dd\nHH:MM:SS.FFF" resulting in date and time separated by a line break of width according to the value of 'lsp.clock'. All "\n" apparing in 'format.clock' cause a line break.
	# ---digits.clock--- is a numeric specifying the number of digits of the seconds part of the time.
	# ---lsp.clock--- is the spacing factor for the line break, given as a multiple of the corresponding x-, y- or zlim. Default is 0.04.
	# ---col.clock--- is the color of the plotted time and date.
	

	##################################################
	##################################################
	if(D==2){
		# Plot date and time:
		if(!any(identical(clock,FALSE),length(clock)==0,nchar(clock)==0)){
			sub = utim2ftim(utim[1], format=format.clock, digits=digits.clock, indt=indt[1])
			sub = strsplit(sub, "\n", fixed=TRUE)[[1]]
			legend("bottomleft", sub, text.col=col.clock , cex=cex.clock, bty="n")
			}
		}
	else{
		# Get the upwards direction of the plot, used when plotting the date and time:
		# The elements on the second row of par3d()$userMatrix behave so that the largest in absolute value is the upmost direction, and the magnitude of this value describes the tilt of the plot:
		UM=par3d()$userMatrix[c(2,6,10)]
		xyzlim=array(par3d()$bbox,dim=2:3)
		upwards=which.max(abs(UM))
		UM[-upwards]=0
		UM=UM*(xyzlim[2,]-xyzlim[1,])
		# Define the clock:
		ischarclock=is.character(clock)
		clockstr=c("bbl","bbr","btl","btr","tbl","tbr","ttl","ttr")
		clockind=as.matrix(expand.grid(1:2,3:4,5:6))
		if(isTRUE(clock)){
			clock="bbl"
			}
		if(ischarclock){
			atclockstr=which(clockstr==clock)
			if(length(atclockstr)>0){
				clocknum=xyzlim[clockind[atclockstr[1],]]
				}
			else{
				clocknum=xyzlim[clockind[1,]]
				}
			}
		else if(is.numeric(clock) && length(clock)>2){
			clocknum=clock[1:3]
			}
	
		# Plot date and time:
		if(!any(identical(clock,FALSE),length(clock)==0,nchar(clock)==0)){
			sub=utim2ftim(utim[1],format.clock,digits=digits.clock,indt=indt[1])
			lbr=gregexpr("\n",sub)[[1]]
			if(any(UM<0)){
				datetime=strsplit(sub,"\n",fixed=TRUE)[[1]]
				}
			else{
				datetime=rev(strsplit(sub,"\n",fixed=TRUE)[[1]])
				}
			if(sum(lbr)>0){
				lbr=c(0,lbr,nchar(sub)+1)
				yspan=max(xyzlim[,2])-min(xyzlim[,2])
				for(thislbr in seq_len(length(lbr)-1)){
					# Position the clock inside the plotting frame, by use of abs(UM) and wheter clockind[atclockstr[1],] is odd or not:
					text3d(clocknum + abs(UM) * lsp.clock * (1-thislbr) * c(1,-1)[1+clockind[atclockstr[1],]%%2], texts=datetime[thislbr], adj=adj.clock, color=col.clock, cex=cex.clock)
					}
				}
			else{
				text3d(clocknum, texts=sub, adj=adj.clock, color=col.clock, cex=cex.clock)
				}
			}
		}
	##################################################
	##################################################
	}
