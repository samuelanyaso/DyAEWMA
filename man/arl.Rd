% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arl.R
\name{arl}
\alias{arl}
\title{ARL & SDRL value of the Adaptive EWMA control chart}
\usage{
arl(h, omg = 0.1, shift = 0, chart_type = "two")
}
\arguments{
\item{h}{a real number; the control limit}

\item{omg}{a real number; smooth constant which lies between 0 and 1, default value is 0.10}

\item{shift}{a real number; shift size. If shift=0, IC ARL is returned, else OC ARL is returned. Default value is 0.0}

\item{chart_type}{a character; either 'one' or 'two'. Select 'one' for one-sided chart and 'two' for two-sided chart. Default is 'two'}
}
\value{
ARL and standard deviation of the runlength
}
\description{
Computes the Average Run Length and th SD of the run length of the Adaptive EWMA control chart
}
\details{
Computes the ARL and the SD of the run length of the Adaptive EWMA control chart
}
\examples{
arl(0.5762,0.10,0.0,'two')
}
\author{
Samuel Anyaso-Samuel
}
