##' Empirical Distribution of the Adaptive EMWA control chart
##'
##' Estimates the in-control empirical distribution of the adaptive EWMA chart.
##' @title Empirical Distribution of the Adaptive EMWA control chart
##' @param x a numeric vector; observed IC data values
##' @param nsimul an integer; number of bootstrap simulations
##' @param w an integer; the sample size needed to reach steady-state
##' @return a numeric vector of the empirical distribution of the proposed AEWMA chart
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' empr_aewma(rnorm(100),1000, 50)
empr_aewma <- function(x, nsimul, w) {
    empirW(x, nsimul, w)
}
