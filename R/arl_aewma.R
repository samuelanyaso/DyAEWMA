##' ARL value of the Adaptive EWMA control chart (with p-values)
##'
##' Estimates the ARL for the AEWMA chart (with p-values)
##' @title ARL value of the Adaptive EWMA control chart (with p-values)
##' @param alpha a real number; the level of significance
##' @param w an integer; the sample size needed to reach steady-state, default value is 50.
##' @param nsimul an integer; the number of replications
##' @param shift a real number; shift size. If shift=0, IC ARL is returned, else OC ARL is returned. Default value is 0.0.
##' @return ARL
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' arl_aewma(0.025,50,100,0.0)
arl_aewma <- function(alpha, w = 50, nsimul, shift = 0) {
    set.seed(1993)
    res <- arlC(alpha, w, nsimul, shift)
    return(res[1]/res[2])
}
