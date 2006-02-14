\name{StatModel-class}
\docType{class}

\alias{StatModel-class}
\alias{fit,StatModel,ModelEnv-method}
\alias{fit}
\alias{dpp,StatModel-method}
\alias{dpp}

\alias{linearModel}
\alias{predict.linearModel}
\alias{fitted.linearModel}
\alias{print.linearModel}
\alias{weights.linearModel}
\alias{model.matrix.linearModel}

\alias{glinearModel}
\alias{predict.glinearModel}
\alias{fitted.glinearModel}
\alias{print.glinearModel}
\alias{model.matrix.glinearModel}

\alias{survReg}
\alias{fitted.survReg}
\alias{logLik.survReg}
\alias{print.survReg}
\alias{weights.survReg}

\title{Class "StatModel"}
\description{ A class for unfitted statistical models. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("StatModel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}, the name of the
                        model.}
    \item{\code{dpp}:}{Object of class \code{"function"}, a function for
                       data preprocessing (usually formula-based). }
    \item{\code{fit}:}{Object of class \code{"function"}, a function for
                       fitting the model to data.}
    \item{\code{predict}:}{Object of class \code{"function"}, a function for
                       computing predictions.}
    \item{\code{capabilities}:}{Object of class
                       \code{"StatModelCapabilities"}.}
  }
}
\section{Methods}{
  \describe{
    \item{fit}{\code{signature(model = "StatModel", data = "ModelEnv")}:
               fit \code{model} to \code{data}.}
  }
}
\section{Details}{
  This is an attempt to provide unified infra-structure for unfitted
  statistical models. Basically, an unfitted model provides a function for
  data pre-processing (\code{dpp}, think of generating design matrices), 
  a function for fitting the specified model to data (\code{fit}), and
  a function for computing predictions (\code{predict}).

  Examples for such unfitted models are provided by \code{linearModel} and
  \code{glinearModel} which provide interfaces in the \code{"StatModel"} framework
  to \code{\link[stats]{lm.fit}} and \code{\link[stats]{glm.fit}}, respectively. The functions
  return objects of S3 class \code{"linearModel"} (inheriting from \code{"lm"}) and
  \code{"glinearModel"} (inheriting from \code{"glm"}), respectively. Some 
  methods for S3 generics such as \code{predict}, \code{fitted}, \code{print}
  and \code{model.matrix} are provided to make use of the \code{"StatModel"}
  structure. (Similarly, \code{survReg} provides an experimental interface to
  \code{\link[survival]{survreg}}.)
}
\examples{

  ### linear model example
  df <- data.frame(x = runif(10), y = rnorm(10))
  mf <- dpp(linearModel, y ~ x, data = df)
  mylm <- fit(linearModel, mf)

  ### equivalent
  print(mylm)
  lm(y ~ x, data = df)

  ### predictions
  Predict(mylm, newdata =  data.frame(x = runif(10)))

}
\keyword{classes}
