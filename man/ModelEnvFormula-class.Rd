\name{ModelEnvFormula-class}
\docType{class}
\alias{ModelEnvFormula-class}

\title{Class "ModelEnvFormula"}
\description{A class for formula-based model environments.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("ModelEnvFormula", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{env}:}{Object of class \code{"environment"}.}
    \item{\code{get}:}{Object of class \code{"function"} for extracting
          objects from environment \code{env}.}
    \item{\code{set}:}{Object of class \code{"function"} for setting
          object in environment \code{env}.}
    \item{\code{formula}:}{Object of class \code{"list"}.}
    \item{\code{hooks}:}{A list of hook collections.}
  }
}
\section{Extends}{
Class \code{"ModelEnv"}, directly.
Class \code{"FormulaParts"}, directly.
}
\section{Methods}{
No methods defined with class "ModelEnvFormula" in the signature.
}
\keyword{classes}
