\name{BANOVA.floodlight}
\alias{BANOVA.floodlight}
\alias{print.BANOVA.floodlight}
\title{Floodlight analysis based on BANOVA models}
\description{
\code{BANOVA.floodlight} conducts floodlight analysis based on various BANOVA models.
}
\usage{
BANOVA.floodlight(sol, var_numeric, var_factor)
\method{print}{BANOVA.floodlight}(x, ...)
}
\arguments{
  \item{sol}{a BANOVA.* object}

  \item{var_numeric}{the numeric variable}

  \item{var_factor}{the factor variable}
  
  \item{x}{a BANOVA.floodlight object}

  \item{\dots}{additional arguments, currently ignored}
}

\details{
A floodlight analysis (Spiller et al. 2013; Johnson and Neyman 1936) based on BANOVA models is conducted, which identifies regions of the numeric variable for which differences between the levels of the factor are significant. The endpoints of the 95\% credible interval of the numeric variable provide the Johnson-Neyman points; for values outside of that interval there is 'strong' evidence that there is a difference between the levels of the factor.
  
}
\value{
  \code{BANOVA.floodlight} returns an object of class \code{"BANOVA.floodlight"}. The returned object is a list containing:
  
  \item{sol}{table of the floodlight analysis including the 95\% credible interval}
  \item{num_range}{range of the numeric variable}

}

\references{ 

Spiller, S., Fitzsimons, G., Lynch Jr., J. and McClelland, G. (2013) \emph{Spotlights, Floodlights, and the Magic Number Zero: Simple Effects Tests in Moderated Regression}. Journal of Marketing Research,  Vol. L, pp. 277-288.

Wedel, M. and Dong, C. (2016) \emph{BANOVA: Bayesian Analysis of Variance for Consumer Research}. Submitted.


}

\examples{
data(condstudy)
if(0){
out2 <- BANOVA.Normal(att~cond+pict, ~type, condstudy, condstudy$id, 
burnin = 5000, sample = 1000, thin = 20)      
# floodlight for pict 
BANOVA.floodlight(out2, var_factor = 'type', var_numeric = 'pict') 
# or use BANOVA.run
model <- BANOVA.model('Normal')
stanmodel <- BANOVA.build(model)
res <- BANOVA.run(att~cond+pict, ~type, fit = stanmodel, data = condstudy, 
                  id = condstudy$id, iter = 500, thin = 1, chains = 2)
BANOVA.floodlight(res, var_factor = 'type', var_numeric = 'pict') 
}
}


