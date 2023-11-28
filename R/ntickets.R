#' @title Function that calculates the number of tickets to be sold when the number of seats in the flight is N and the
#' probability of a "show" is p and gamma is the probability a plane will be truly overbooked
#'
#' @param N the maximum capacity
#' @param gamma the maximum acceptable risk of failure.
#' @param p target the probability of success
#'
#' @return returns a list
#' @importFrom graphics abline barplot layout
#' @importFrom stats optimize pbinom pnorm
#' @export
#'
#' @examples ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
# The results of my check:
#
# ─ R CMD check results  FALL234753RAMI0166 0─
# Duration: 38.3s
# 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
# R CMD check succeeded

ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {

  # The range of possible ticket sales
  n <- seq(floor(N * 0.1)) + N

  # Computing the optimal number of tickets sold using a discrete distribution
  nd <- 1 - gamma - pbinom(q = N, size = n, prob = p)

  # Minimizing it for discrete
  ndm <- which.min(abs(nd))

  # Computing the optimal number of tickets sold using a normal approximation
  nc <- 1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))

  # Optimizing for continuous
  f <- function(x) abs(1 - gamma - pnorm(N + 0.5, x * p, sqrt(x * p * (1 - p))))
  res <- optimize(f, interval = c(N, N* 1.1))
  nco <- res$minimum

  # Plotting Objective vs n for discrete case
  plot(n, nd, type = 'b', pch = 18, col = "blue", main = paste("Objective Vs n to find optimal tickets sold\n", "(", n[ndm], ")", "gamma = ", gamma, "N = ", N, "discrete"), ylab = "Objective")
  abline(h = 0, v = n[ndm], lwd = 2, col = "red")

  # Plotting Objective vs n for continuous case
  plot(n, nc, type = 'l', col = "black", main=paste("Objective Vs n to find optimal tickets sold\n", "(", round(nco, 12), ")", "gamma = ", gamma, "N = ", N, "continuous"), ylab = "Objective")
  abline(h = 0, v = round(nco, 4), lwd = 2, col = "blue")

  # Creating a named list containing nd, nc, N, p and gamma
  namedList <- list(nd = n[ndm], nc = round(nco, 4), N = N, p = p, gamma = gamma)

  # Outputting the result list
  print(namedList)

}
