#' print_precisely
#'
#' Function to transform the input to numeric and then print it with maxiumum precision.
#' @param x A numeric, character or factor variable that can be transformed to numeric.
#' @keywords numeric, precision
#' @export
#' @examples
#' print_precisely(x)

print_precisely<-function(x){
  print(format(as.numeric(as.character(x)), digits = 22, scientific = FALSE))
}
