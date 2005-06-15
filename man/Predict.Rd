\name{Predict}
\alias{Predict}
\title{ Model Predictions }
\description{

    A function for predictions from the results of various model 
    fitting functions.

}
\usage{
Predict(object, ...)
}
\arguments{
  \item{object}{ a model object for which prediction is desired. }
  \item{\dots}{ additional arguments affecting the predictions produced. }
}
\details{

  A somewhat improved version of \code{\link[stats]{predict}} for models
  fitted with objects of class \code{\link{StatModel-class}}.

}
\value{
  
  Should return a vector of the same type as the response variable specified
  for fitting \code{object}.

}
\examples{

df <- data.frame(x = runif(10), y = rnorm(10))
mf <- dpp(linearModel, y ~ x, data = df)
Predict(fit(linearModel, mf))

}
\keyword{misc}
