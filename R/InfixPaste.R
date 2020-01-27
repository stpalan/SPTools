#' Paste without separator - infix operator
#'
#' Infix operator to combine strings. Equivalent to paste(..., sep="").
#' @param x An object of type character.
#' @param y An object of type character.
#' @keywords paste
#' @export
#' @examples
#' z<-5
#' "The result is: " %_% z %_% "."
#' > The result is 5.

"%_%" = function(x,y) paste(x,y,sep="")