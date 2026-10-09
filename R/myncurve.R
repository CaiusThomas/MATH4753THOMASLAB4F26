#' Normal curve with a shaded lower tail
#'
#' Draws the normal curve with mean `mu` and standard deviation `sigma`,
#' shades the area under the curve from negative infinity up to `x = a`,
#' and calculates that area, which is the probability P(X <= a).
#' The z-score of `a` is also worked out and shown on the plot, so you
#' can see how many standard deviations `a` is from the mean.
#'
#' @param mu The mean of the normal distribution.
#' @param sigma The standard deviation of the normal distribution.
#'   Must be greater than 0.
#' @param a The upper limit of the shaded area. The function finds
#'   P(X <= a).
#'
#' @return A named list with five parts:
#' \describe{
#'   \item{mu}{The mean that was used.}
#'   \item{sigma}{The standard deviation that was used.}
#'   \item{a}{The upper limit of the shaded area.}
#'   \item{z}{The z-score of `a`, `(a - mu) / sigma`.}
#'   \item{prob}{The area P(X <= a), rounded to 4 decimal places.}
#' }
#'
#' @importFrom graphics polygon abline legend
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#' myncurve(mu = 0, sigma = 1, a = 1.96)
myncurve = function(mu, sigma, a) {
  # check the inputs before doing anything
  if (!is.numeric(mu) || length(mu) != 1) {
    stop("mu must be a single number")
  }
  if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
    stop("sigma must be a single number greater than 0")
  }
  if (!is.numeric(a) || length(a) != 1) {
    stop("a must be a single number")
  }

  # x-axis goes 3 sd each side of the mean,
  # widened if a is outside that range so a is always on the plot
  xmin = min(mu - 3 * sigma, a - sigma)
  xmax = max(mu + 3 * sigma, a + sigma)

  # draw the curve
  xs = seq(xmin, xmax, length = 1000)
  ys = dnorm(xs, mean = mu, sd = sigma)

  # z-score: how many sd a is from the mean
  z = (a - mu) / sigma

  # area to the left of a
  prob = round(pnorm(a, mean = mu, sd = sigma), 4)

  plot(xs, ys, type = "l", lwd = 2,
       xlab = "x", ylab = "Density",
       main = paste("P(X <= ", a, ") = ", prob,
                    "   for X ~ N(", mu, ", ", sigma, ")", sep = ""))

  # shade from the left edge of the plot (stands in for -infinity) up to a
  xcurve = seq(xmin, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(xmin, xcurve, a), c(0, ycurve, 0), col = "Red")

  # dashed lines at the mean and at a
  abline(v = mu, lty = 2, col = "grey40")
  abline(v = a, lty = 2, col = "darkred")

  legend("topright",
         legend = c(paste("Area =", prob),
                    paste("z =", round(z, 4)),
                    paste("mean =", mu)),
         col = c("Red", "darkred", "grey40"),
         pch = c(15, NA, NA), lty = c(NA, 2, 2), bty = "n")

  list(mu = mu, sigma = sigma, a = a, z = z, prob = prob)
}
