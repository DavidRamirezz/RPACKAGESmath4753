#' Title
#' is a function that takes 3 arguments
#' it samples a vector of 1-10 values and displays a chart with time in between
#'
#' @param n amount of choices
#' @param iter number of iterations
#' @param time amount of time between iterations
#'
#' @return returns mysample
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot layout
#' @export
#'
#' @examples mysample(1, iter = 10, time = 0.5)
mysample = function(n = 1, iter = 10, time = 0.5) {
  for(i in 1:iter){
    #make a sample
    s = sample(1:10, n, replace = TRUE)

    # turn the sample into a factor
    sf = factor(s, levels = 1:10)

    #make a barplot
    barplot(table(sf)/n, beside = TRUE, col = rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n, sep="") ,
            ylim=c(0, 0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
