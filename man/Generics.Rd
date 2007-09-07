\name{Generics}
\alias{ICL}
\alias{KLdiv}
\alias{Lapply}
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
Lapply(object, FUN, \dots)
cluster(object, newdata, \dots)
getModel(object, \dots)
parameters(object, \dots)
posterior(object, newdata, \dots)
prior(object, \dots)
refit(object, newdata, \dots)
}
\arguments{
  \item{object}{S4 classed object.}
  \item{FUN}{The function to be applied.}
  \item{newdata}{Optional new data.}
  \item{\dots}{Some methods for these generic function may take
    additional, optional arguments.}
}
\details{
  \describe{
    \item{ICL:}{Integrated Completed Likelihood criterion for model
      selection.}
    \item{KLdiv:}{Kullback-Leibler divergence.}
    \item{Lapply:}{S4 generic for \code{lapply}}
    \item{cluster:}{Get cluster membership information from a model or
      compute it for new data.}
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
