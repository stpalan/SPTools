#' compat.plot
#'
#' Function which returns a compatibility plot for hypothesis testing in ggplot2 format.
#' @param x data the test is run on.
#' @param y data the test is run on.
#' @param FUN test function to use.
#' @param test.args list of further arguments to be passed on to the test function
#' @param ... further arguments to be passed on to
#' @param conf.levels confidence levels to highlight
#' @param ylim range of the vertical axis entered as a vector consisting of lower and upper bound
#' @param xlab horizontal axis title
#' @param ylab vertical axis title
#' @param precision step-width and distance from 1 for compatibility intervals to plot
#' @param digits.x significant digits for x-axis labels
#' @param digits.y significant digits for y-axis labels
#' @param file.name fully qualified file name for graph output file. If not specified, no image file is produced.
#' @keywords hypothesis testing, compatibility plot, ggplot2
#' @export
#' @examples
#' Instead of writing, for example:
#' A[A$a==B$a & A$b==B$b & A$c==B$c & A$d==B$d,]
#' one can write
#' A[SPSubset(A,B,c("a","b","c","d")),]

# Testdata
x <- c(1,2,3,4,4,5)
y <- NULL
FUN<-t.test
test.args<-list(x=x,mu=3)
conf.levels <- c(0.95, 0.99)
ylim<-c(0.85,1)
xlab<-"Value"
ylab<-"Compatibility level"
precision<-0.001
digits.x<-3
digits.y<-4
file.name<-"test.png"


compat.plot <- function(FUN,
                        test.args,
                        ...,
                        conf.levels = c(0.95, 0.99),
                        ylim=c(0.85,1),
                        xlab=NA,
                        ylab="Compatibility level",
                        precision=0.001,
                        digits.x=5,
                        digits.y=3,
                        file.name=NA) {

  # compat.plot(FUN, test.args, conf.levels = c(0.95, 0.99), ylim=c(0.85,1), xlab=NA, ylab="Compatibility level", precision=0.001, digits.x=5, digits.y=3, file.name=NA)


  # Checks ------------------------------------------------------------------

  if(!is.numeric(precision)) stop("precision must be numeric")
  if(!is.numeric(conf.levels)) stop("conf.levels must be numeric")
  if(!is.numeric(ylim)) stop("ylim must be numeric")
  if (precision<=0 & precision <1) stop("precision must be positive and less than 1") # Checks precision is in allowed range



  # Packages ----------------------------------------------------------------

  require(ggplot2)
  require(tidyverse)



  # Extract test results ----------------------------------------------------
  test <- do.call(FUN, test.args)

  if (test$method == "One Sample t-test") {
    single.sample <- T
    estimate <- test$estimate
    if(is.na(xlab)){xlab<-"Value"}
  }

  if (test$method == "Welch Two Sample t-test") {
    single.sample <- F
    estimate <- as.numeric(test$estimate[1]-test$estimate[2])
    if(is.na(xlab)){xlab<-"Difference"}
  }


  if (test$method == "Exact binomial test") {
    single.sample <- T
    estimate <- test$estimate
    if(is.na(xlab)){xlab<-"Probability"}
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
    geom_segment(aes(x=estimate,xend=estimate,y=min(ylim),yend=max(ylim)),lwd=0.75,col="red")+ # Adds vertical line at point estimate
    # Note: aes is necessary because axis transformation is applied first
    geom_line(lwd=1) # Plots compatibility graph

  # Defines custom axis breaks
  breaks.x<-numeric()
  breaks.x<-c(breaks.x[!is.na(breaks.x)],as.numeric(estimate))
  breaks.y<-layer_scales(plot)$y$get_breaks()

  # Adds reference lines for confidence levels
  temp.x.lower<-numeric()
  temp.x.upper<-numeric()
  for (fL in conf.levels){
    temp.x.lower<-head(d$value[d$comp.level==fL & d$name=="lower"],1)
    temp.x.upper<-head(d$value[d$comp.level==fL & d$name=="upper"],1)

    plot <- plot +
      annotate("segment", x = min(d$value), xend = temp.x.upper,y = fL, yend = fL, lwd=0.5, lty=2, col=match(fL, conf.levels)+1) +
      annotate("segment", x = temp.x.lower, xend = temp.x.lower,y = fL, yend = 1, lwd=0.5, lty=2, col=match(fL, conf.levels)+1) +
      annotate("segment", x = temp.x.upper, xend = temp.x.upper,y = fL, yend = 1, lwd=0.5, lty=2, col=match(fL, conf.levels)+1)

    # Adds custom axis breaks for confidence level reference lines
    breaks.x<-c(breaks.x,tail(temp.x.lower,1),tail(temp.x.upper,1))
    breaks.y<-c(breaks.y,fL)
  }

  # Applies custom axis breaks
  breaks.x<-signif(breaks.x[order(breaks.x)],digits.x)
  breaks.y<-signif(breaks.y[order(breaks.y)],digits.y)
  plot <- plot +
    scale_x_continuous(name=xlab, # Axis title
                       breaks=breaks.x) + # Axis breaks
    scale_y_continuous(name=ylab, # Axis title
                       trans="reverse", # Reverses axis order
                       limits=c(max(ylim),min(ylim)), # Axis limits
                       breaks=breaks.y)+ # Axis breaks
    theme(panel.grid.minor = element_blank()) # Removes default grid lines

    if(!is.na(file.name)){
    ggsave(plot = plot, filename = file.name, width = 900, height = 700, units = "px", dpi = 180)
  }

  print(plot)

}
