#' head1
#'
#' Function to replicate the head() function from the 'utils' package, but with a default of returning only one row.
#' @param x A vector, matrix, table, data frame or function.
#' @param n The number of rows to return. Defaults to 1.
#' @keywords head
#' @export
#' @examples
#' # Return one row
#' head1(mtcars)
#' # Return multiple rows
#' head1(x = mtcars, n = 3)
#'
head1 <- function(x, n = 1) {
  require(utils, quietly = TRUE)
  head(x, n)
}
