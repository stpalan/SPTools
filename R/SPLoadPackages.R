#' SPLoadPackages
#'
#' Function which compares two dataframes (the second with only a single row) column-wise, yielding a Boolean vector of equality between the two. Can be used to select a specific row out of the first (Data), which corresponds in some columns to the same columns in the second (ComparisonVector).
#' @param packages A character vector containing the names of packages to load.
#' @keywords packages
#' @export
#' @examples
#' Instead of writing, for example:
#' 
#' if (!require(dplyr)) install.packages('dplyr')
#' library(dplyr)
#' if (!require(zoo)) install.packages('zoo')
#' library(zoo)
#' 
#' one can write
#' 
#' SPLoadPackages(c("dplyr","zoo"))

SPLoadPackages<-function(packages){
    for(fP in packages){
        eval(parse(text="if(!require("%_%fP%_%")) install.packages('"%_%fP%_%"')"))
        eval(parse(text="library("%_%fP%_%")"))
    }
}