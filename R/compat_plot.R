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

# Testdata
x <- c(1,2,3,4,4,5)
FUN<-t.test
test.args<-list(mu=3)
conf.levels <- c(0.95, 0.99)
ylim<-c(0.8,1)
xlab<-"value"
ylab<-"compatibility level"
precision<-0.001
digits.mu<-3


compat.plot <- function(x,
                        FUN,
                        test.args,
                        ...,
                        conf.levels = c(0.95, 0.99),
                        ylim=c(0.8,1),
                        xlab="value",
                        ylab="compatibility level",
                        precision=0.001,
                        digits.mu=5) {


  # Checks ------------------------------------------------------------------

  if(!is.numeric(precision)) stop("precision must be numeric")
  if(!is.numeric(conf.levels)) stop("conf.levels must be numeric")
  if(!is.numeric(ylim)) stop("ylim must be numeric")
  if (precision<=0 & precision <1) stop("precision must be positive and less than 1") # Checks precision is in allowed range



  # Packages ----------------------------------------------------------------

  require(ggplot2)
  require(tidyverse)



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

  # Creates vector of confidence levels to plot which includes the ones the user requested
  temp.levels<-c(conf.levels,seq(from=min(ylim),to=min(0.999,max(ylim)),by=0.001))
  temp.levels<-temp.levels[order(temp.levels)]
  for (fI in temp.levels) {
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
    #scale_x_continuous(name=xlab)+ # Axis title
    scale_y_continuous(name=ylab, # Axis title
                       trans="reverse", # Reverses axis order
                       limits=c(max(ylim),min(ylim)))+ # Specifies axis limits
    geom_segment(aes(x=sample.mean,xend=sample.mean,y=min(ylim),yend=max(ylim)),lwd=0.75)+ # Adds vertical line at point estimate
    # Note: aes is necessary because axis transformation is applied first
    #geom_text(aes(x = sample.mean, y = min(ylim)), label = paste(expression(mu),": ", format(sample.mean,digits=digits.mu),sep=""), vjust = -0.5, cex=4, parse = T)+
    geom_line(lwd=1)

  # Defines custom x-axis breaks
  breaks.x<-numeric() #layer_scales(plot)$x$get_breaks()
  breaks.x<-c(breaks.x[!is.na(breaks.x)],as.numeric(sample.mean))

  # Adds reference lines for confidence levels
  for (fL in conf.levels){
    temp.x.lower<-head(d$value[d$comp.level==fL & d$name=="lower"],1)
    temp.x.upper<-head(d$value[d$comp.level==fL & d$name=="upper"],1)
    plot <- plot +
      geom_segment(aes(x=min(value),xend=temp.x.upper,y=fL,yend=fL),lwd=0.5, lty=2)+ # Adds horizontal line at confidence level
      geom_segment(aes(x=temp.x.lower,xend=temp.x.lower,y=fL,yend=1),lwd=0.5, lty=2)+ # Adds vertical line at confidence level lower boundary
      geom_segment(aes(x=temp.x.upper,xend=temp.x.upper,y=fL,yend=1),lwd=0.5, lty=2) # Adds vertical line at confidence level upper boundary

    # Adds custom x-axis breaks
    breaks.x<-c(breaks.x,temp.x.lower,temp.x.upper)
  }

  # Applies custom x-axis breaks
  breaks.x<-signif(breaks.x[order(breaks.x)],digits.mu)
  plot <- plot +
    scale_x_continuous(name=xlab, breaks=breaks.x) +
    theme(panel.grid.minor = element_blank())

  plot


  ggsave(plot = plot, filename = "D:/periodical/compat_plot.png", width = 600, height = 600, units = "px", dpi = 150)

}
