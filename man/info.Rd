%
%  Copyright (C) 2005 Friedrich Leisch
%  $Id: info.Rd 1849 2005-10-10 06:15:57Z leisch $
%
\name{info}
\alias{info}
\alias{infoCheck}
\alias{info,ANY,missing-method}
\title{Get Information on Fitted Objects}
\description{
  Returns descriptive information about fitted objects.
}
\usage{
info(object, which, ...)
\S4method{info}{ANY,missing}(object, which, ...)
infoCheck(object, which, ...)
}
\arguments{
  \item{object}{fitted object.}

  \item{which}{which information to get. Use \code{which="help"} to list
    available information.}

  \item{\dots}{passed to methods.}
}
\details{
  Function \code{info} can be used to access slots of fitted 
  objects in a portable way.

  Function \code{infoCheck} returns a logical value that is \code{TRUE}
  if the requested information can be computed from the \code{object}.
} 
\author{Friedrich Leisch}
\keyword{methods}
