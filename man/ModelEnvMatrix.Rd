\name{ModelEnvMatrix}
\alias{ModelEnvMatrix}
\title{Generate a model environment from design and response matrix}
\description{
    A simple model environment creator function working off matrices for
    input and response. This is much simpler and more limited than
    formula-based environments, but faster and easier to use, if only
    matrices are allowed as input.
}
\usage{
ModelEnvMatrix(designMatrix=NULL, responseMatrix=NULL,
               subset = NULL, na.action = NULL, other=list(), ...)
}
\arguments{
  \item{designMatrix}{design matrix of input}
  \item{responseMatrix}{matrix of responses}
  \item{subset}{ an optional vector specifying a subset of observations to
                 be used in the fitting process. }
  \item{na.action}{ a function which indicates what should happen when the data
          contain \code{NA}'s. }
  \item{other}{ an optional named list of additional formulae. }
  \item{\dots}{currently not used}
}
\details{
  \code{ModelEnvMatrix} returns an object of class
  \code{\link{ModelEnv-class}} - a high level object for storing
  data improving upon the capabilities of simple data matrices.

  Funny things may happen if the inpiut and response matrices do not have 
  distinct column names and the data new data are supplied via the
  \code{get} and \code{set} slots.
}
\value{
  An object of class \code{\link{ModelEnv-class}}.
}
\examples{
### use Sepal measurements as input and Petal as response
data(iris)
me <- ModelEnvMatrix(iris[,1:2], iris[,3:4])
me

### extract data from the ModelEnv object
dim(me@get("designMatrix"))
summary(me@get("responseMatrix"))

### subsets and missing values
iris[1,1] <- NA
me  <- ModelEnvMatrix(iris[,1:2], iris[,3:4], subset=1:5, na.action=na.omit)

## First case is not complete, so me contains only cases 2:5
me
me@get("designMatrix")
me@get("responseMatrix")

## use different cases
me@set(data=iris[10:20,])
me@get("designMatrix")

## these two should be the same
stopifnot(all.equal(me@get("responseMatrix"), as.matrix(iris[10:20,3:4])))
}
\keyword{misc}
