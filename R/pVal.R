##' Computes the p-value from the empirical distribution of the statistic
##'
##' Estimates the p-value (Right-tailed) for the AEWMA chart proposed by Haq et al
##' @title Computes the p-value from the empirical distribution of the statistic
##' @param empr_dist a numeric vector; the empirical distribution
##' @param obsStat a real number; the observed test statistic
##' @return p-value (right-tailed) of the observed test statistic
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @examples
##' pVal(c(0.25,-0.36,-0.54,0.89), 0.64)
pVal <- function(empr_dist, obsStat) {
    findPvalue1sC(empr_dist, obsStat)
}
