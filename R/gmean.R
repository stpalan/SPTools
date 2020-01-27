#' gmean
#'
#' Function to calculate the geometric mean analogously to R's built-in mean() function. It catches negative inputs, properly handles zeroes and allows for both NA removal and trimming (again, as in mean()) before processing. Adapted from Paul McMurdie's excellent example at https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in. 
#' @param x An R object. Currently there are methods for numeric/logical vectors and date, date-time and time interval objects. Negative values will return NaN.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end of x before the mean is computed. Values of trim outside that range are taken as the nearest endpoint.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param zero.propagate a logical value indicating whether inputs containing any 0 values should return 0 (zero.propagate == TRUE) or should calculate the geometric mean ignoring any 0 input values (zero.propagate == FALSE).
#' @keywords geometric mean
#' @export
#' @examples
#' x <- c(0:10, 50)
#' xgm <- gmean(x)
#' c(xgm, gmean(x, trim = 0.10))

gmean<-function(x, trim = 0, na.rm = FALSE, zero.propagate = FALSE){
	#Checks for problems with inputs
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
	
	#Removes NAs if requested
    if (na.rm) 
        x <- x[!is.na(x)]
    if (!is.numeric(trim) || length(trim) != 1L) 
        stop("'trim' must be numeric of length one")
    n <- length(x)
	
	#Trims data, analogous to the mean() function
    if (trim > 0 && n) {
        if (is.complex(x)) 
            stop("trimmed means are not defined for complex data")
        if (anyNA(x)) 
            return(NA_real_)
        if (trim >= 0.5) 
            return(stats::median(x, na.rm = FALSE))
        lo <- floor(n * trim) + 1
        hi <- n + 1 - lo
        x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    }
    
	#Calculates geometric mean following https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
    if(any(x < 0, na.rm = TRUE)){
        return(NaN)
    }
    if(zero.propagate){
        if(any(x == 0, na.rm = TRUE)){
            return(0)
        }
        exp(mean(log(x), na.rm = na.rm))
    } else {
        exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
    }
}