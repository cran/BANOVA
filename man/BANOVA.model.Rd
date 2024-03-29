% Generated by roxygen2: do not edit by hand
\name{BANOVA.model}
\alias{BANOVA.model}
\title{Extract BANOVA models}
\description{
\code{BANOVA.model} extracts BANOVA models from the package.
}
\usage{
BANOVA.model(model_name, single_level = F)
}
\arguments{
  \item{model_name}{a character string in c('Normal', 'T', 'Bernoulli', 'Binomial', 'Poisson', 'ordMultinomial', 'Multinomial', 'multiNormal', 'truncNormal')}

  \item{single_level}{if the model is a single level model, default False}
}

\value{
  \code{BANOVA.model} returns an object of class \code{"BANOVA.model"}. The returned object is a list containing:
  
  \item{model_code}{the model code of the extracted model}
  \item{model_name}{the model name}
  \item{single_level}{if the model is a single level model}
}
\details{
The function loads a pre-specified 'Stan' model for the analysis in BANOVA.

\code{'Normal'} model: A model suitable for a continuous dependent variable, which follows a Normal distribution.

\code{'T'} model: A model suitable for a continuous dependent variable, which might be prone to 'outliers' or fatter tails than the Normal.

\code{'Bernoulli'} model: A model suitable for a binary dependent variable, which can take values 0 and 1.

\code{'Binomial'} model: A model suitable for a dependent variable, which represents a number of successes in a sequence of B independent Bernoulli experiments.

\code{'Poisson'} model: A model suitable for a dependent variable, which represents count data. A Poisson distributed dependent variable can take values 0, 1, 2 \dots.

\code{'ordMultinomial'} model: A model suitable for an ordered categorical (ordinal) dependent variable, which follows an ordered Multinomial distribution. This dependent variable can take values from 1 to K, where possible alternatives are ordered according to some principal.

\code{'Multinomial'} model: A model suitable for a categorical (nominal) dependent variable, which follows a Multinomial distribution. This dependent variable can take values from 1 to K, where possible alternatives are unordered.

\code{'multiNormal'} model: A model suitable for a Multivariate Normal dependent variable, which represents L possibly correlated Normal dependent variables with shared predictors. The analysis corresponds to the seemingly unrelated regressions (SUR) technique. 

\code{'truncNormal'} model: A model suitable a dependent variable, which values can only be observed if they lie within a certain range. The variable can be bounded from below, above, or from two sides.
}
\examples{
\donttest{
model <- BANOVA.model('Poisson', single_level = FALSE) 
cat(model$model_code)
}
}



