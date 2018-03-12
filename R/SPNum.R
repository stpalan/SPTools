#' SPNum
#'
#' Function to transform factor to numeric. Returns input argument as transformed to numeric.
#' @param x A numeric, character or factor variable that can be transformed to numeric.
#' @keywords factor, numeric
#' @export
#' @examples
#' Instead of writing, for example:
#' as.numeric(as.character(x))
#' one can write
#' SPNum(x)

SPNum<-function(x){
    return(as.numeric(as.character(x)))
}