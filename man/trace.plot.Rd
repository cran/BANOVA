\name{trace.plot}
\alias{trace.plot}
\title{Function to plot the trace of parameters}
\description{
Function to plot the trace of all level-2 parameters. The plots can be saved as a pdf file.
}
\usage{
trace.plot(x, save = FALSE)
}
\arguments{
  \item{x}{the object from BANOVA.*}

  \item{save}{whether to save the trace plot as a pdf file, the default is FALSE}
}

\examples{
data(goalstudy)

res1 <- BANOVA.Normal(bid~1, ~goalprogress*varmeans,  goalstudy, goalstudy$id, 
burnin = 100, sample = 100, thin = 2) 

trace.plot(res1)

}

