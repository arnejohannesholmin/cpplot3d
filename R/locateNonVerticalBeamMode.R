#*********************************************
#*********************************************
#' Internal: Detects time steps with non-vertical beam mode, which can be used to override the beam mode specified in raw files.
#'
#' @return 
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom sonR read.event
#' @importFrom TSD prettyIntegers
#'
#' @export
#' @rdname locateNonVerticalBeamMode
#' 
locateNonVerticalBeamMode<-function(t, event){
	#t = read.event(event, t=t, var="indt")$indt
	if(is.integer(t)){
		tplus9 = unique(c(outer(t,seq(0,9),"+")))
		bmmd = read.event(var="bmmd", event=event, t=tplus9)$bmmd
		if(length(bmmd)){
			# Try with 9 pings:
			tplus9 = tplus9[bmmd %in% c(0,1)]
			newt = unlist(lapply(t, function(x) if(any(tplus9>=x)) min(tplus9[tplus9>=x]) else x))
			if(any(t==newt)){
				warning("Non-vertical mode was not found for some time steps.")
				}
			t=newt
			cat("Time steps selected: ", prettyIntegers(t), " (length: ",length(t),")\n", sep="")
			}
		}
	as.double(t)
	}
