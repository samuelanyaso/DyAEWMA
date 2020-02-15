##' ARL & ATS of the one-sided(max) Adaptive EWMA control chart.
##'
##' Estimates the ARL & ATS for the one-sided(max) AEWMA chart using the bootstrap approach.
##' @title  ARL & ATS of the one-sided(max) Adaptive EWMA control chart.
##' @param alpha a real number; the level of significance
##' @param w an integer; the sample size needed to reach steady-state, default value is 50.
##' @param nsimul an integer; the number of replications
##' @param a a real number; a parameter in the dynamic sampling interval function
##' @param lambda a real number; lambda parameter in the dynamic sampling interval function
##' @param b a real number; b parameter in the dynamic sampling interval function
##' @param shift a real number;  shift size. If shift=0, IC ARL/ATS is returned, else OC ARL/ATS is returned. Default value is 0.0.
##' @return ATS and ARL[when d(.) = 1] and count of successful iterations
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' ats_arl_aewma_max(0.025,50,100,0,2.5,3.1,0.05)
ats_arl_aewma_max <- function(alpha, w = 50, nsimul, a, lambda, b, shift = 0) {
    res <- arl_ats_max(alpha, w, nsimul, a, lambda, b, shift)
    return(c(res[2], res[1], res[3]))
}
