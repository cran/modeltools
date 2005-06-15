\name{StatModel-class}
\docType{class}
\alias{StatModel-class}
\alias{linearModel}
\alias{fit,StatModel,ModelEnv-method}
\alias{fit}
\alias{dpp,StatModel-method}
\alias{dpp}

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

}
\examples{

  ### linear model example
  df <- data.frame(x = runif(10), y = rnorm(10))
  mf <- dpp(linearModel, y ~ x, data = df)
  mylm <- fit(linearModel, mf)

  ### the same
  print(mylm)
  lm(y ~ x, data = df)

  ### predictions
  Predict(mylm, newdata =  data.frame(x = runif(10)))

}
\keyword{classes}
