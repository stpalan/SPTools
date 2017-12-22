#' SPClusterSE
#'
#' Function which calculates clustered standard errors for a regression model. This function is basically a copy of Mahmood Ara's work (http://people.su.se/~ma/clustering.pdf, http://people.su.se/~ma/mcluster.R), as described by Kevin Goulding (https://thetarzan.wordpress.com/2011/06/11/clusteredstandarderrorsinr/). The example is also adapted from Kevin's excellent post.
#' @param Data A dataframe containing the data the regression is run in.
#' @param Model A saved regression model.
#' @param ClusterVar The variable to cluster by.
#' @keywords clustered standard errors
#' @export
#' @examples
#' require(foreign)
#' nmar = read.dta("http://www.montana.edu/econ/cstoddard/562/wr-nevermar.dta")
#' # Run a plain linear regression
#' regt = lm(nevermar ~ impdum, data = nmar)
#' # apply the 'SPClusterSE' function by choosing a variable to cluster on.
#' # here, we are clustering on state.
#' SPClusterSE(nmar, regt, nmar$state)


SPClusterSE<-function(Data,Model,ClusterVar){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(ClusterVar))
    N <- length(ClusterVar)
    K <- Model$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj <- apply(estfun(Model),2, function(x) tapply(x, ClusterVar, sum));
    vcovCL <- dfc*sandwich(Model, meat=crossprod(uj)/N)
    coeftest(Model, vcovCL)
}