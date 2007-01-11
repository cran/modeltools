\name{Generics}
\alias{ICL}
\alias{KLdiv}
\alias{cluster}
\alias{getModel}
\alias{parameters}
\alias{posterior}
\alias{prior}
\alias{refit}
\title{Generic Utility Functions}
\description{
  A collection of standard generic functions for which other packages
  provide methods.
}
\usage{
ICL(object, \dots)
KLdiv(object, \dots)
cluster(object, \dots)
getModel(object, \dots)
parameters(object, \dots)
posterior(object, newdata, \dots)
prior(object, \dots)
refit(object, \dots)
}
\arguments{
  \item{object}{S4 classed object.}
  \item{newdata}{Optional new data.}
  \item{\dots}{Some methods for these generic function may take
    additional, optional arguments.}
}
\details{
  \describe{
    \item{ICL:}{Integrated Completed Likelihood criterion for model
      selection.}
    \item{KLdiv:}{Kullback-Leibler divergence.}
    \item{cluster:}{Extract cluster membership information.}
    \item{getModel:}{Get single model from a collection of models.}
    \item{parameters:}{Get parameters of a model (similar to but more
      general than \code{\link{coefficients}}).}
    \item{posterior:}{Get posterior probabilities from a model or
      compute posteriors for new data.}
    \item{prior:}{Get prior probabilities from a model.}
    \item{refit:}{Refit a model (usually to obtain additional
      information that was not computed or stored during the initial
      fitting process).}
  }
}
\keyword{methods}
\author{Friedrich Leisch}
