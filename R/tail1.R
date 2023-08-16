#' tail1
#'
#' Function to replicate the tail() function from the 'utils' package, but with a default of returning only one row.
#' @param x A vector, matrix, table, data frame or function.
#' @param n The number of rows to return. Defaults to 1.
#' @keywords tail
#' @export
#' @examples
#' # Return one row
#' tail1(mtcars)
#' # Return multiple rows
#' tail1(x = mtcars, n = 3)
#'
tail1 <- function(x, n = 1) {
  require(utils, quietly = TRUE)
  tail(x, n)
}
