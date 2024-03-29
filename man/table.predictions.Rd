% Generated by roxygen2: do not edit by hand
\name{table.predictions}
\alias{table.predictions}
\title{Function to print the table of means}
\description{
Output of this function is a table of means for the categorical predictors (and their interactions) at either within- or between- subjects level. Statistics of interest such as credible intervals and standard deviations of the means are also computed. Means of numeric variables and their interactions will not be computed.
}
\usage{
table.predictions(x)
}
\arguments{
  \item{x}{the object from BANOVA.*}
}

\examples{
data(goalstudy)
\donttest{
library(rstan)
# or use BANOVA.run based on 'Stan'
res <- BANOVA.run(bid~progress*prodvar, model_name = "Normal", 
data = goalstudy, id = 'id', iter = 1000, thin = 1, chains = 2)
table.predictions(res)
}
}

