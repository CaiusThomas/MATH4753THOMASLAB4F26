#' Binomial simulation compared to exact probabilities
#'
#' Simulates a binomial experiment `iter` times (like `mybin()` from Lab 5),
#' draws a barplot of the simulated proportions, and adds the exact
#' binomial probabilities from `dbinom()` as points on top of the bars.
#' It also returns a table comparing the simulated and exact values.
#'
#' @param iter Number of experiments to simulate.
#' @param n Number of trials in each experiment.
#' @param p Probability of success on each trial.
#'
#' @return A list with two parts:
#' \describe{
#'   \item{table}{A data frame with columns `y` (number of successes),
#'   `simulated`, `exact`, and `difference` (simulated minus exact).}
#'   \item{max.difference}{The largest difference between a simulated
#'   and an exact value, ignoring the sign.}
#' }
#'
#' @importFrom graphics barplot points legend
#' @importFrom grDevices rainbow
#' @importFrom stats dbinom
#' @export
#'
#' @examples
#' binSim(iter = 1000, n = 10, p = 0.7)
binSim = function(iter = 100, n = 10, p = 0.5) {
  sam.mat = matrix(NA, nrow = n, ncol = iter)
  succ = c()
  for (i in 1:iter) {
    sam.mat[, i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    succ[i] = sum(sam.mat[, i])
  }
  succ.tab = table(factor(succ, levels = 0:n))
  simulated = as.numeric(succ.tab / iter)
  exact = dbinom(0:n, n, p)

  mids = barplot(simulated, names.arg = 0:n, col = rainbow(n + 1),
                 ylim = c(0, max(simulated, exact) * 1.2),
                 main = paste("Binomial simulation, iter = ", iter, sep = ""),
                 xlab = "Number of successes", ylab = "Proportion")
  points(mids, exact, pch = 19, type = "b")
  legend("topleft", legend = c("simulated (bars)", "exact dbinom (points)"),
         pch = c(15, 19), col = c("grey50", "black"), bty = "n")

  result = data.frame(y = 0:n,
                      simulated = round(simulated, 4),
                      exact = round(exact, 4),
                      difference = round(simulated - exact, 4))
  list(table = result, max.difference = max(abs(simulated - exact)))
}
