\name{ModelEnv-class}
\docType{class}
\alias{ModelEnv-class}
\alias{clone,ModelEnv-method}
\alias{clone}
\alias{dimension,ModelEnv,character-method}
\alias{dimension}
\alias{has,ModelEnv,character-method}
\alias{has}
\alias{initialize,ModelEnv-method}
\alias{show,ModelEnv-method}
\alias{subset,ModelEnv-method}
\alias{subset}
\alias{na.pass,ModelEnv-method}
\alias{na.pass}
\alias{na.fail,ModelEnv-method}
\alias{na.fail}
\alias{na.omit,ModelEnv-method}
\alias{na.omit}

\title{Class "ModelEnv"}
\description{ A class for model environments.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ModelEnv", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{env}:}{Object of class \code{"environment"}.}
    \item{\code{get}:}{Object of class \code{"function"} for extracting
          objects from environment \code{env}.}
    \item{\code{set}:}{Object of class \code{"function"} for setting
          object in environment \code{env}.}
  }
}
\section{Methods}{
  \describe{
    \item{clone}{\code{signature(object = "ModelEnv")}: copy an object. }
    \item{dimension}{\code{signature(object = "ModelEnv", which = "character")}:
                     get the dimension of an object. }
    \item{has}{\code{signature(object = "ModelEnv", which = "character")}:
               check if an object \code{which} is available in \code{env}. }
    \item{initialize}{\code{signature(.Object = "ModelEnv")}: setup new
                      objects.}
    \item{show}{\code{signature(object = "ModelEnv")}: show object. }
    \item{subset}{\code{signature(x = "ModelEnv")}: extract subsets from an
                  object. }
    \item{na.pass}{\code{\link{na.action}} method for \code{ModelEnv} objects.}
    \item{na.fail}{\code{\link{na.action}} method for \code{ModelEnv} objects.}
    \item{na.omit}{\code{\link{na.action}} method for \code{ModelEnv} objects.}
  }
}
\details{

  Objects of class \code{ModelEnv} basically consist of an
  \code{\link{environment}} for data storage as well as \code{get} and
  \code{set} methods.

  \code{na.fail} returns \code{FALSE} when at least one missing value occurs
  in \code{object@env}. \code{na.pass} returns \code{object} unchanged and
  \code{na.omit} returns a copy of \code{object} with all missing values
  removed.

}
\examples{

### a new object
me <- new("ModelEnv")

### define a bivariate response variable
me@set("response", data.frame(y = rnorm(10), x = runif(10)))
me

### check if a response is available
has(me, "response")

### the dimensions
dimension(me, "response")

### extract the data
me@get("response")

### NA handling
df <- data.frame(x = rnorm(10), y = rnorm(10))
df$x[1] <- NA
mf <- ModelEnvFormula(x ~ y, data = df, na.action = na.pass)
mf
na.omit(mf)

}
\keyword{classes}
