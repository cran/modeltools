\name{ModelEnvFormula}
\alias{ModelEnvFormula}
\title{ Generate a model environment from a 
        classical formula based interface. }
\description{
    A flexible implementation of the classical formula based interface.
}
\usage{
ModelEnvFormula(formula, data = list(), subset = NULL, 
                na.action = NULL, frame = NULL, 
                enclos = sys.frame(sys.nframe()), other = list(), 
                designMatrix = TRUE, responseMatrix = TRUE,
                setHook = NULL, ...)
}
\arguments{
  \item{formula}{ a symbolic description of the model to be fit. }
  \item{data}{ an optional data frame containing the variables in the model. 
              If not found in \code{data}, the variables are taken from
              \code{frame}, by default the environment from which
              \code{ModelEnvFormula} is called.}
  \item{subset}{ an optional vector specifying a subset of observations to
                 be used in the fitting process. }
  \item{na.action}{ a function which indicates what should happen when the data
          contain \code{NA}'s. }
  \item{frame}{ an optional environment \code{formula} is evaluated in. }
  \item{enclos}{ specifies the enclosure passed to \code{\link{eval}} for
                 evaluating the model frame. The model frame is evaluated in 
                 \code{envir = frame} with \code{enclos = enclos}, 
                 see \code{\link{eval}}.}
  \item{other}{ an optional named list of additional formulae. }
  \item{designMatrix}{ a logical indicating whether the design matrix
                       defined by the right hand side of \code{formula} 
                       should be computed. }
  \item{responseMatrix}{ a logical indicating whether the design matrix
                       defined by the left hand side of \code{formula}
                       should be computed.  }
  \item{setHook}{ a list of functions to \code{\link{MEapply}} every
    time \code{set} is called on the object.  }
  \item{\dots}{ additional arguments for be passed to function, for example
                \code{contrast.arg} to \code{\link{model.matrix}}. }
}
\details{

  This function is an attempt to provide a flexible infrastucture for the
  implementation of classical formula based interfaces. The arguments
  \code{formula}, \code{data}, \code{subset} and \code{na.action} are well
  known and are defined in the same way as in \code{\link{lm}}, for example.

  \code{ModelEnvFormula} returns an object of class
  \code{\link{ModelEnvFormula-class}} - a high level object for storing
  data improving upon the capabilities of \code{data.frame}s.

}
\value{
  An object of class \code{\link{ModelEnvFormula-class}}.
}
\examples{

### the `usual' interface
data(iris)
mf <- ModelEnvFormula(Species ~ ., data = iris)
mf

### extract data from the ModelEnv object
summary(mf@get("response"))
summary(mf@get("input"))
dim(mf@get("designMatrix"))

### contrasts
mf <- ModelEnvFormula(Petal.Width ~ Species, data = iris, 
                      contrasts.arg = list(Species = contr.treatment))
attr(mf@get("designMatrix"), "contrasts")
mf <- ModelEnvFormula(Petal.Width ~ Species, data = iris, 
                      contrasts.arg = list(Species = contr.sum))
attr(mf@get("designMatrix"), "contrasts")

### additional formulae
mf <- ModelEnvFormula(Petal.Width ~ Species, data = iris, 
                      other = list(pl = ~ Petal.Length))
ls(mf@env)
identical(mf@get("pl")[[1]], iris[["Petal.Length"]])

}
\keyword{misc}
