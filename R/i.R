#' i
#'
#' Function to increment a numeric.
#' @param x A numeric.
#' @param by The value to increment x by. Defaults to 1.
#' @keywords increment
#' @export
#' @examples
#' Increments. For example:
#' x<-5
#' i(x)
#' After this operation, x == 6.

i<-function(x, by = 1){
    eval.parent(substitute(x <- x + by))
}