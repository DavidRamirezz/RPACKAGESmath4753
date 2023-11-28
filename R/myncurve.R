#' @title myncurve for Lab 6
#'
#' @param mu the mean
#' @param sigma the standard deviation
#' @param a the ending bound
#'
#' @return returns a curve with a specific shaded area
#' @export
#'
#' @examples myncurve(mu = 5, sigma = 3, a = 2)
myncurve = function(mu, sigma, a) {
  x <- NULL

  graphics::curve(stats::dnorm(x, mean = mu, sd = sigma),
                  xlim = c(mu - (3 * sigma), mu + (3 * sigma)))

  xcurve = seq(mu - (3 * sigma), a, length = 1000)
  ycurve = stats::dnorm(xcurve, mean = mu, sd = sigma)
  graphics::polygon(c(xcurve, a), c(ycurve, 0), col = "Purple")

  area = stats::pnorm(a, mu, sigma) - stats::pnorm(-Inf, mu, sigma)
  area = round(area, 4)

  graphics::text(x = mu, y = 0.5 * stats::dnorm(mu, mu, sigma),
                 paste0("Area = ", area))

}
