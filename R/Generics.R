#
#  Copyright (C) 2006 Torsten Hothorn, Friedrich Leisch
#  $Id: Generics.R 3595 2007-06-29 16:08:37Z gruen $
#



## generics used in flexmix and flexclust which may also be useful in
## other packages

setGeneric("ICL", function(object, ...) standardGeneric("ICL"))

setGeneric("KLdiv", function(object, ...) standardGeneric("KLdiv"))

setGeneric("Lapply", function(object, FUN, ...) standardGeneric("Lapply"))

setGeneric("cluster", function(object, newdata, ...) standardGeneric("cluster"))

setGeneric("getModel", function(object, ...) standardGeneric("getModel"))

setGeneric("parameters", function(object, ...) standardGeneric("parameters"))

setGeneric("posterior", function(object, newdata, ...) standardGeneric("posterior"))

setGeneric("prior", function(object, ...) standardGeneric("prior"))

setGeneric("refit", function(object, newdata, ...) standardGeneric("refit"))

###**********************************************************

setGeneric("info",
function(object, which, ...) standardGeneric("info"))

setMethod("info", signature(object="ANY", which="missing"),
function(object, which, ...)
{
    info(object, which="help")
})
    
infoCheck <- function(object, which, ...)
{
    which %in% info(object, "help")
}



###**********************************************************


