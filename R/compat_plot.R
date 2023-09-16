#' compat.plot
#'
#' Function which returns a compatibility plot for hypothesis testing in ggplot2 format.
#' @param x data the test is run on.
#' @param FUN test function to use.
#' @param test.args list of further arguments to be passed on to the test function
#' @param ... further arguments to be passed on to
#' @param conf.levels confidence levels to highlight
#' @param ylim range of the vertical axis entered as a vector consisting of lower and upper bound
#' @param xlab horizontal axis title
#' @param ylab vertical axis title
#' @param precision step-width and distance from 1 for compatibility intervals to plot
#' @keywords
#' @export
#' @examples
#' Instead of writing, for example:
#' A[A$a==B$a & A$b==B$b & A$c==B$c & A$d==B$d,]
#' one can write
#' A[SPSubset(A,B,c("a","b","c","d")),]


compat.plot <- function(x,
                        FUN,
                        test.args,
                        ...,
                        conf.levels = 0.95,
                        ylim=c(0.8,1),
                        xlab="value",
                        ylab="compatibility level",
                        precision=0.001) {


  # Checks ------------------------------------------------------------------

  if(!is.numeric(precision)) stop("precision must be numeric")
  if(!is.numeric(conf.levels)) stop("conf.levels must be numeric")
  if(!is.numeric(ylim)) stop("ylim must be numeric")
  if (precision<=0 & precision <1) stop("precision must be positive and less than 1") # Checks precision is in allowed range



  # Packages ----------------------------------------------------------------

  require(ggplot2, tidyverse)



  # Extract test results ----------------------------------------------------
  test.args[["x"]]<-x
  test <- do.call(FUN, test.args)

  if (test$method == "One Sample t-test") {
    single.sample <- T
    sample.mean <- test$estimate
  }

  if (test$method == "Welch Two Sample t-test") {
    single.sample <- F
    sample.mean <- as.numeric(diff(test$estimate))
  }



  # Calculate compatibility intervals ------------------------------------------
  comp.ints <- data.frame(list(
    comp.level = numeric(),
    lower = numeric(),
    upper = numeric()
  ))

  for (fI in seq(from=min(ylim),to=min(0.999,max(ylim)),by=0.001)) {
    test.args.loop<-test.args # Prepares list of arguments for test function
    test.args.loop[["conf.level"]]<-fI # Adds confidence level to list of arguments
    temp.test<-do.call(FUN, test.args.loop) # Saves test run with the necessary arguments
    comp.ints[nrow(comp.ints) + 1, ] <- c(fI, as.numeric(temp.test$conf.int)) # Extracts confidence interval
    # comp.ints[[as.character(fI)]]<-as.numeric(FUN(data,...,conf.level=fI)$conf.int)
  }


  # Creates long-format dataframe for plotting
  d<-comp.ints %>%
    pivot_longer(cols=c(lower,upper))



# Plot --------------------------------------------------------------------

  plot<-ggplot(data=d, aes(x=value,y=comp.level,group=name))+
    scale_x_continuous(name=xlab)+ # Axis title
    scale_y_continuous(name=ylab, # Axis title
                       trans="reverse", # Reverses axis order
                       limits=c(max(ylim),min(ylim)))+ # Specifies axis limits
    geom_segment(x=sample.mean,xend=sample.mean,y=min(ylim),yend=max(ylim))+ # Adds vertical line at point estimate
    geom_line()

  plot
}
