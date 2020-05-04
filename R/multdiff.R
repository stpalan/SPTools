#' multdiff
#'
#' Performs setdiff on the elements of x, where x is a list of vectors of the same type but not necessarily the same length.
#' @param x A list of vectors.
#' @keywords setdiff
#' @export
#' @examples
#' Instead of writing, for example:
#' setdiff(setdiff(a,b),c)
#' one can write
#' multdiff(list(a,b,c))

multdiff <- function(x) {
    
    if(!is.list(x)){stop("x is not a list!")}
    if(length(x)<2){stop("x needs to be a list with at least two elements!")}
    
    temp<-x[[1]]
    for(fV in 2:length(x)){
        temp<-setdiff(temp,x[[fV]])
    }
    
 
    return(temp)

}