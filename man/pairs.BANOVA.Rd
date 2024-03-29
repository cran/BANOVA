% Generated by roxygen2: do not edit by hand
\name{pairs.BANOVA}
\alias{pairs.BANOVA}
\title{Create a matrix of output plots from a \code{BANOVA} object} 
\description{
A \code{\link[=graphics]{pairs}} 
method that is customized for MCMC output.
}
\usage{
\method{pairs}{BANOVA}(x, ...)
}
\arguments{
  \item{x}{an object of class \code{"BANOVA"}}

  \item{...}{Further arguments to be passed to 
\code{\link[=rstan]{pairs.stanfit}}}
}

\details{
For a detailed description see \code{\link[=rstan]{pairs.stanfit}}
}

\examples{
\donttest{
library(rstan)
data(ipadstudy)
res_1 <- BANOVA.run(attitude~owner + age + gender + selfbrand*conspic, 
model_name = 'Normal', data = ipadstudy, id = 'id', iter = 1000, 
thin = 1, chains = 2)
# pairs(res_1, pars = c("beta1[1]","beta1[2]"))
}
}



