\name{table.means}
\alias{table.means}
\title{Function to print the table of means}
\description{
Output of this function is a table of means for the categorical predictors at either level-1 or level-2. Statistics of interest such as credible intervals and standard deviations of the means are also computed. 
}
\usage{
table.means(x)
}
\arguments{
  \item{x}{the object from BANOVA.*}
}

\examples{

data(goalstudy)

res <- BANOVA.Normal(bid~1, ~goalprogress*varmeans,  goalstudy, goalstudy$id, 
burnin = 1000, sample = 1000, thin = 1) 

table.means(res)

}

