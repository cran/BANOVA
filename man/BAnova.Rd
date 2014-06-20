\name{BAnova}
\alias{BAnova}
\title{Function to print the table of effect sizes}
\description{
The analysis of variance is performed at level 2 equation of the Bayesian ANOVA see \code{\link{BANOVA-package}}. This makes it possible to capture the effects of level-2 variables on the heterogeneity distribution of subjects, and compute sums of squares and effect sizes.

}
\usage{
BAnova(x)
}
\arguments{
  \item{x}{the object from BANOVA.*}
}

\details{
Measures of effect size in regression are measures of the degree of association between an effect (e.g., a main effect, an interaction, a linear contrast) and the dependent variable. They can be considered as the correlation between a categorical factor(effect) and the dependent variable. They are usually interpreted as the proportion of variance in the dependent variable that is attributable to each effect. In the package, Eta squared is calculated and displayed. It is defined as follows, \cr

\eqn{\eta^2= \frac{(SS_effect)}{(SS_total )}} \cr

Where:
SS_effect= the sums of squares for the effect of interest \cr
SS_total= the total sums of squares for all effects, interactions, and errors in the regression.

If the design is not balanced, type II(if interactions are not included in the model) and type III sum of squares are computed.

}

\references{ 
  Fox, J. (2008)
  \emph{Applied Regression Analysis and Generalized Linear Models},
  Second Edition. Sage.  
  
  Fox, J. and Weisberg, S. (2011) 
  \emph{An R Companion to Applied Regression}, Second Edition, Sage.
  
}

\examples{

data(goalstudy)

res1 <- BANOVA.Normal(bid~1, ~goalprogress*varmeans,  goalstudy, goalstudy$id, 
burnin = 100, sample = 100, thin = 2) 

BAnova(res1)

}

