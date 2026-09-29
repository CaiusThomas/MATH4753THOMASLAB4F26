#' Fit a binomial model to observed data
#'
#' Takes observed numbers of successes (each one out of `n` trials),
#' estimates the probability of success p, and compares the observed
#' proportions to the binomial probabilities for that estimated p.
#' Draws a barplot of the observed proportions with the binomial
#' probabilities added as points on top.
#'
#' @param x A vector of observed numbers of successes, each a whole
#'   number from 0 to `n`.
#' @param n Number of trials behind each observation.
#'
#' @return A list with two parts:
#' \describe{
#'   \item{p.hat}{The estimated probability of success, `mean(x) / n`.}
#'   \item{table}{A data frame with columns `y` (number of successes),
#'   `observed`, `expected` (from `dbinom()` using `p.hat`), and
#'   `difference` (observed minus expected).}
#' }
#'
#' @importFrom graphics barplot points legend
#' @importFrom grDevices rainbow
#' @importFrom stats dbinom
#' @export
#'
#' @examples
#' binFit(freethrows$made, n = 10)
binFit = function(x, n) {
  if (any(x < 0 | x > n)) {
    stop("every value of x must be between 0 and n")
  }
  p.hat = mean(x) / n
  obs.tab = table(factor(x, levels = 0:n))
  observed = as.numeric(obs.tab / length(x))
  expected = dbinom(0:n, n, p.hat)

  mids = barplot(observed, names.arg = 0:n, col = rainbow(n + 1),
                 ylim = c(0, max(observed, expected) * 1.2),
                 main = paste("Observed data vs binomial, p-hat = ", round(p.hat, 3), sep = ""),
                 xlab = "Number of successes", ylab = "Proportion")
  points(mids, expected, pch = 19, type = "b")
  legend("topleft", legend = c("observed (bars)", "binomial (points)"),
         pch = c(15, 19), col = c("grey50", "black"), bty = "n")

  result = data.frame(y = 0:n,
                      observed = round(observed, 4),
                      expected = round(expected, 4),
                      difference = round(observed - expected, 4))
  list(p.hat = p.hat, table = result)
}
