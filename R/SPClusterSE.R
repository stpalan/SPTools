#' SPClusterSE
#'
#' Function which calculates one-way or two-way clustered standard errors for a regression model. This function is basically a copy of Mahmood Ara's work (http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf), as described by Kevin Goulding (https://thetarzan.wordpress.com/2011/06/11/clusteredstandarderrorsinr/). The example is also adapted from Kevin's excellent post.
#' @param Data A dataframe containing the data the regression is run in. Defaults to NA for two-way case.
#' @param Model A saved regression model.
#' @param ClusterVar1 The (first) variable to cluster by.
#' @param ClusterVar2 The second variable to cluster by. Defaults to NA.
#' @param DFCorrection A degree of freedom correction when the model was estimated on deviation from group mean data. Defaults to 1.
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


SPClusterSE<-function(Data=NA,Model,ClusterVar1,ClusterVar2=NA,DFCorrection=1){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    if(identical(ClusterVar2,NA)){
        M <- length(unique(ClusterVar1))
        N <- length(ClusterVar1)
        K <- Model$rank
        dfc <- (M/(M-1))*((N-1)/(N-K))
        uj <- apply(estfun(Model),2, function(x) tapply(x, ClusterVar1, sum));
        vcovCL <- dfc*sandwich(Model, meat=crossprod(uj)/N)
        coeftest(Model, vcovCL)
    } else {
        cluster12 = paste(ClusterVar1,ClusterVar2, sep="")
        M1 <- length(unique(ClusterVar1))
        M2 <- length(unique(ClusterVar2))
        M12 <- length(unique(cluster12))
        N <- length(ClusterVar1)
        K <- Model$rank
        dfc1 <- (M1/(M1-1))*((N-1)/(N-K))
        dfc2 <- (M2/(M2-1))*((N-1)/(N-K))
        dfc12 <- (M12/(M12-1))*((N-1)/(N-K))
        u1 <- apply(estfun(Model), 2, function(x) tapply(x, ClusterVar1, sum))
        u2 <- apply(estfun(Model), 2, function(x) tapply(x, ClusterVar2, sum))
        u12 <- apply(estfun(Model), 2, function(x) tapply(x, cluster12, sum))
        vc1 <- dfc1*sandwich(Model, meat=crossprod(u1)/N )
        vc2 <- dfc2*sandwich(Model, meat=crossprod(u2)/N )
        vc12 <- dfc12*sandwich(Model, meat=crossprod(u12)/N)
        vcovMCL <- (vc1 + vc2 - vc12)*DFCorrection
        coeftest(Model, vcovMCL)
    }
}