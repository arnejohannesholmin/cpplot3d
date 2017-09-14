#*********************************************
#*********************************************
#' Returns a function of one signel numeric, returning the sizes used when plotting voxels by cplot3d.TSD(). The input to the retunred function is the number of breaks of the color plot. The inputs to cplot3d.size() are the type of size function, the range of the size values and the parameters of the size function.
#'
#' @param type  is a three character string representing the type of function of the size. Current valid values are 
#' @param y  is a vector of the range of the size values.
#' @param par  is a vector of 0, 1 or 2 elements representing the parameters to the size function.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD setrange
#'
#' @export
#' @rdname cplot3d.size
#'
cplot3d.size<-function(type="lin",y=NULL,par=NULL){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2011-01-06 - Clean version.
	########### DESCRIPTION: ###########
	# Returns a function of one signel numeric, returning the sizes used when plotting voxels by cplot3d.TSD(). The input to the retunred function is the number of breaks of the color plot. The inputs to cplot3d.size() are the type of size function, the range of the size values and the parameters of the size function.
	########## DEPENDENCIES: ###########
	# setrange()
	############ VARIABLES: ############
	# ---type--- is a three character string representing the type of function of the size. Current valid values are 
	#		(1) "lin" for linear function between the values y[1] and y[2] (horizontal if length(y)==1), 
	#		(2) "pow" for the power function x^par[1], 
	#		(3) "exp" for the exponential function of x: exp(par[1]*x), 
	#		(4) "sin" for the sine function starting at the angle par[1] and ending at par[2]: sin(seq(par[1],par[2],l=lbreaks)) and 
	#		(5) "log" for the logit funciton 1/(1+exp(-par[2]*(x+par[1]))). All size vectors are scaled to the interval y[1] to y[2].
	# ---y--- is a vector of the range of the size values.
	# ---par--- is a vector of 0, 1 or 2 elements representing the parameters to the size function.
	
	
	##################################################
	##################################################
	##### Preparation and execution #####
	if(type=="lin"){
		if(length(y)==0){
			y=c(5,5)
			}
		if(length(y)==1){
			y=rep(y,l=2)
			}
		type=function(lbreaks){
			seq(y[1],y[2],l=lbreaks)
			}
		}
	else if(type=="pow"){
		if(length(y)==0){
			y=c(0,10)
			}
		if(length(y)==1){
			y=c(0,y)
			}
		if(length(par)==0){
			par=2
			}
		type=function(lbreaks){
			setrange(seq(0,1,l=lbreaks)^par[1],y[1],y[2])
			}
		}
	else if(type=="exp"){
		if(length(y)==0){
			y=c(0,10)
			}
		if(length(y)==1){
			y=c(0,y)
			}
		if(length(par)==0){
			par=1
			}
		type=function(lbreaks){
			setrange(exp(par[1]*seq(0,1,l=lbreaks)),y[1],y[2])
			}
		}
	else if(type=="sin"){
		if(length(y)==0){
			y=c(0,10)
			}
		if(length(y)==1){
			y=c(0,y)
			}
		if(length(par)==0){
			par=c(-90,90)
			}
		par=par/180*pi
		type=function(lbreaks){
			setrange(sin(seq(par[1],par[2],l=lbreaks)),y[1],y[2])
			}
		}
	else if(type=="log"){
		if(length(y)==0){
			y=c(0,10)
			}
		if(length(y)==1){
			y=c(0,y)
			}
		type=function(lbreaks){
			if(length(par)==0){
				par=c(0,round(sqrt(lbreaks)))
				}
			par[2]=log(19)/(par[2]/lbreaks)
			setrange(1/(1+exp(-par[2]*(seq(-1,1,l=lbreaks)+par[1]))),y[1],y[2])
			}
		}
	
	##### output #####
	type
	##################################################
	##################################################
	}
