##' ARL value of the one-sided(max) Adaptive EWMA control chart (without p-values)
##'
##' Estimates the ARL for one-sided(max) the AEWMA chart (without p-values)
##' @title ARL value of the one-sided(max) Adaptive EWMA control chart (without p-values)
##' @param alpha a real number; the level of significance
##' @param w an integer; the sample size needed to reach steady-state, default value is 50.
##' @param nsimul an integer; the number of simulations
##' @param repl an integer; the number of replications for estimating the emprical distribution
##' @param shift a real number; shift size. If shift=0, IC ARL is returned, else OC ARL is returned. Default value is 0.0.
##' @return ARL, count of success iterations
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' arl_aewma_max(0.025,50,100,10000, 0.0)
arl_aewma_max <- function(alpha, w = 50, nsimul, repl = 1e+06, shift = 0) {
    res <- arl_maxC(alpha, w, nsimul, repl, shift)
    return(c(res[1], res[2]))
}
