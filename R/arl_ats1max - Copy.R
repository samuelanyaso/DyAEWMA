##' ARL for a one-sided(max) Adaptive EWMA control chart.
##'
##' Estimates the ARL for a one-sided(max) AEWMA chart using the bootstrap approach.
##' @title  ARL & ATS of the Adaptive EWMA control chart.
##' @param alpha a real number; the level of significance
##' @param w an integer; the sample size needed to reach steady-state, default value is 50.
##' @param nsimul an integer; the number of replications
##' @param shift a real number;  shift size. If shift=0, IC ARL/ATS is returned, else OC ARL/ATS is returned. Default value is 0.0.
##' @return ARL
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' arl4alpha(0.025,50,100,0.05)
arl4alpha <- function(alpha, w = 50, nsimul, shift = 0) {
    set.seed(83706)
    res <- arl4alpha(alpha, w, nsimul, shift)
    return(c(res[1]/res[2]))
}
