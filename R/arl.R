##' Computes the Average Run Length and th SD of the run length of the Adaptive EWMA control chart
##'
##' Computes the ARL and the SD of the run length of the Adaptive EWMA control chart
##' @title ARL & SDRL value of the Adaptive EWMA control chart
##' @param h a real number; the control limit
##' @param omg a real number; smooth constant which lies between 0 and 1, default value is 0.10
##' @param shift a real number; shift size. If shift=0, IC ARL is returned, else OC ARL is returned. Default value is 0.0
##' @param chart_type a character; either 'one' or 'two'. Select 'one' for one-sided chart and 'two' for two-sided chart. Default is 'two'
##' @return ARL and standard deviation of the runlength
##' @author Samuel Anyaso-Samuel
##' @export
##' @useDynLib DyAEWMA
##' @importFrom Rcpp sourceCpp
##' @importFrom stats sd
##' @examples
##' arl(0.5762,0.10,0.0,'two')
arl <- function(h, omg = 0.1, shift = 0, chart_type = "two") {
    if (chart_type == "two") {
        set.seed(2019)
        RL <- arla(h, omg, shift)
    } else if (chart_type == "one") {
        set.seed(2019)
        RL <- arla(h, omg, shift)
    }
    return(c(mean(RL), sd(RL)))
}
