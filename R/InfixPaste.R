#' %+%
#'
#' Infix operator to combine strings. Equivalent to paste(..., sep="").
#' @param x An object of type character.
#' @param y An object of type character.
#' @keywords paste
#' @export
#' @examples
#' z<-5
#' "The result is: " %+% z %+% "."
#' > The result is 5.

"%+%" = function(x,y) paste(x,y,sep="")