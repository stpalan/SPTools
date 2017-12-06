#' SPSubset
#'
#' Function which compares two dataframes (the second with only a single row) column-wise, yielding a Boolean vector of equality between the two. Can be used to select a specific row out of the first (Data), which corresponds in some columns to the same columns in the second (ComparisonVector).
#' @param Data A dataframe containing the data you want to select a specific row from.
#' @param ComparisonVector A dataframe with a single row which contains the indices used to select a row from dataframe Data.
#' @param by.Data A character vector of column names of dataframe Data which shall serve as indices for row selection.
#' @param by.ComparisonVector A character vector of column names of dataframe ColumnVector which shall serve as indices for row selection. Needs to have equal length as by.Data and defaults to by.Data.
#' @keywords subset
#' @export
#' @examples
#' Instead of writing, for example:
#' A[A$a==B$a & A$b==B$b & A$c==B$c & A$d==B$d,]
#' one can write
#' A[SPSubset(A,B,c("a","b","c","d")),]

SPSubset<-function(Data,ComparisonVector,by.Data,by.ComparisonVector=by.Data){
    if (nrow(ComparisonVector)!=1) {
        # Checks whether x and y have equal length
        stop("ComparisonVector must only have 1 row!") #Prints error message
    }
    if (length(by.Data)!=length(by.ComparisonVector)) {
        # Checks whether indices have equal length
        stop("The indices must have equal length!") #Prints error message
    }
    SPSubset<-rep(T,nrow(Data))
    for(Col in 1:length(by.Data)){
        SPSubset<-SPSubset&(Data[,by.Data[Col]]==ComparisonVector[,by.ComparisonVector[Col]])
    }
    return(SPSubset)
}