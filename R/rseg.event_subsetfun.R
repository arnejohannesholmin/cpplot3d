rseg.event_subsetfun <- function(x, cmpred=NULL, ellipsoid=NULL){
	if(suppressWarnings(sum(!is.na(cmpred))>0) && suppressWarnings(sum(!is.na(ellipsoid))>0)){
		(x$psxx-cmpred[1])^2/ellipsoid[1]^2 + (x$psyx-cmpred[2])^2/ellipsoid[2]^2 + (x$pszx-cmpred[3])^2/ellipsoid[3]^2 <= 1
		}
	else{
		!logical(length(x$psxx))
		}
	}
