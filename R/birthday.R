#' Birthday Problem Probability
#'
#' Calculates the probability that at least two people in a group of
#' size n share the same birthday.
#'
#' @param n group size. a single whole number or a vector like 20:25
#'
#' @return a vector of probabilities, one for each value of n
#' @export
#'
#' @examples
#' birthday(23)
#' birthday(20:25)
birthday = function(n) {
  # log of P, written this way since R can't store 365!
  log_p_none = lfactorial(n) + lchoose(365, n) - n * log(365)

  # undo the log, then take the complement
  1 - exp(log_p_none)
}
