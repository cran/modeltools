\name{MEapply}
\alias{MEapply,ModelEnv-method}
\alias{MEapply}
\title{Apply functions to Data in Object of Class "ModelEnv"}
\description{Apply a single function or a collection of functions to the
  data objects stored in a model environment.}
\usage{
\S4method{MEapply}{ModelEnv}(object, FUN, clone = TRUE, ...)
}
\arguments{
  \item{object}{Object of class \code{"ModelEnv"}.}
  \item{FUN}{Function or list of functions.}
  \item{clone}{If \code{TRUE}, return a clone of the original object, if
    \code{FALSE}, modify the object itself.}
  \item{\dots}{Passed on to \code{FUN}.}
}
\examples{
data("iris")
me <- ModelEnvFormula(Species+Petal.Width~.-1, data=iris,
                      subset=sample(1:150, 10))

me1 <- MEapply(me, FUN=list(designMatrix=scale,
                            response=function(x) sapply(x, as.numeric)))

me@get("designMatrix")
me1@get("designMatrix")
}
\keyword{methods}
