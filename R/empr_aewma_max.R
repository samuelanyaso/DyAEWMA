##' Empirical Distribution of the one-sided(max) Adaptive EMWA control chart
##'
##' Estimates the in-control empirical distribution of the one-sided(max) adaptive EWMA chart.
##' @title Empirical Distribution of the one-sided(max) Adaptive EMWA control chart
##' @param repl an integer; number of bootstrap replications
##' @param w an integer; the sample size needed to reach steady-state
##' @param x a numeric vector; observed IC data values
##' @return a numeric vector of the empirical distribution of the proposed AEWMA chart
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @importFrom stats rnorm
##' @examples
##' empr_aewma_max(1000, 50, rnorm(100))
empr_aewma_max <- function(repl = 1e+06, w, x) {
    # rnorm(32611)
    EmprDist(repl, w, x)
}
