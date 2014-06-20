\name{BANOVA.Bin}
\alias{BANOVA.Bin}
\alias{predict.BANOVA.Bin}
\alias{print.BANOVA.Bin}
\alias{summary.BANOVA.Bin}
\title{Estimation of BANOVA with a Binomial dependent variable}
\description{
\code{BANOVA.Bin} implements a Hierarchical Bayesian ANOVA for a binomial response variable using a logit link and a normal heterogeneity distribution.

}
\usage{
BANOVA.Bin(l1_formula = "NA", l2_formula = "NA", data,
  id, num_trails, burnin = 500, sample = 1000, thin = 1, jags = findjags())
\method{summary}{BANOVA.Bin}(object, ...)
\method{predict}{BANOVA.Bin}(object, newdata = NULL,...)
\method{print}{BANOVA.Bin}(x, ...)
}
\arguments{
 \item{l1_formula}{formula for level 1 e.g. 'Y~X1+X2'}

  \item{l2_formula}{formula for level 2 e.g. '~Z1+Z2',
  response variable must not be included}

  \item{data}{a data.frame in long format including all features in level 1 and level 2(covariates and categorical factors) and responses}

  \item{id}{subject ID of each response unit}

  \item{num_trails}{the number of trails of each
  observation(=1, if it is bernoulli), the type is forced to be 'integer'}

  \item{burnin}{the number of burn in draws in the MCMC algorithm. Note that the number of adaptive iterations (see \link[runjags]{run.jags}) is set to 0, thus, there is a warning. But this wouldn't affect the simulation.}

  \item{sample}{target samples in the MCMC algorithm after thinning}

  \item{thin}{the number of samples in the MCMC algorithm that needs to be thinned}
  
  \item{jags}{the system call or path for activating JAGS. Default calls findjags() to attempt to locate JAGS on your system}
  
  \item{object}{object of class \code{BANOVA.Bin} (returned by \code{BANOVA.Bin})}

  \item{newdata}{test data, either a matrix, vector or a
  data frame. It must have the same format with the original data (the same column number)}
  
  \item{x}{object of class \code{BANOVA.Bin} (returned by \code{BANOVA.Bin})}
  
  \item{\dots}{additional arguments,currently ignored}
}

\details{
Level 1 model: \cr
  \eqn{y_i} {~} \eqn{Binomial(ntrails,p_i)}, \eqn{p_i = logit^{-1}(\eta_i)} \cr
  where ntrails is the binomial total for each record i, \eqn{\eta_i = \sum_{p = 0}^{P}\sum_{j=1}^{J_p}X_{i,j}^p\beta_{j,s_i}^p}, \eqn{s_i} is the subject id of response \eqn{i}. see \code{\link{BANOVA-package}}
  
}
\value{
  \code{BANOVA.Bin} returns an object of class \code{"BANOVA.Bin"}. The returned object is a list containing:
  
  \item{anova.table}{table of effect sizes \code{\link{BAnova}}}
  \item{coef.tables}{table of estimated coefficients}
  \item{pvalue.table}{table of p-values \code{\link{table.pvalues}}}
  \item{dMatrice}{design matrices at level 1 and level 2}
  \item{samples_l2_param}{posterior samples of level 2 parameters}
  \item{data}{original data.frame}
  \item{mf1}{model.frame of level 1}
  \item{mf2}{model.frame of level 2}
  \item{JAGSmodel}{JAGS model}
}

\examples{

data(colorad)
# mean center Blur for effect coding
colorad$blur <- colorad$blur - mean(colorad$blur)
res <- BANOVA.Bin(y~typic, ~color*blur, colorad, colorad$id, as.integer(16), 
burnin = 10, sample = 10, thin = 1)
summary(res)
# table.means(res)
# use the the first row of data to predict, 
# only the in-model variables data will be used, the others will be ignored
predict(res, c(1, 0, 8, 2, 1, 0.03400759))

}
