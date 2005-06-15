
### Parse and evaluate a formula, return the data as object of class 
### `ModelEnv'

ModelEnvFormula <- function(formula, data = list(), subset = NULL, 
                            na.action = NULL, frame = NULL,
                            other = list(), designMatrix = TRUE,
                            responseMatrix = TRUE, ...) {
  
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"),
               names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    if (is.null(subset)) mf$subset <- NULL

    ### NA-handling will for the ModelFrame objects later on...
    mf$na.action <- stats::na.pass

    MEF <- new("ModelEnvFormula")
    MEF@formula <- c(ParseFormula(formula, data=data)@formula, other)
    
    if (is.null(frame)) frame <- parent.frame()

    MEF@get <- function(which, data=NULL, frame=parent.frame(), envir = MEF@env)
    {
        if(is.null(data))
            RET <- get(which, envir = envir, inherits=FALSE)
        else{
            oldData <- get(which, envir = envir, inherits=FALSE)
            mf$data <- data
            mf$formula <- MEF@formula[[which]]
            RET <- eval(mf, frame)
            checkData(oldData, RET)
        }
        return(RET)
    }
    
    MEF@set <- function(which = NULL, data = NULL, frame = parent.frame(),
                        envir = MEF@env)
    {
        if (is.null(which)) which <- names(MEF@formula)
        if (any(duplicated(which)))
            stop("Some model terms used more than once")
        
        for (name in which){
            
            if (length(MEF@formula[[name]]) != 2)
                stop("Invalid formula for ", sQuote(name))
            
            mf$data <- data
            mf$formula <- MEF@formula[[name]]

            ### <FIXME> if subset was specied, we try to evaluate it
            ###         everytime `set' is called, even for new data
            ### </FIXME>
            MF <- eval(mf, frame)
            if (exists(name, envir = envir, inherits = FALSE))
                checkData(get(name, envir = envir, inherits = FALSE), MF)
            assign(name, MF, envir = envir)
            mt <- attr(MF, "terms")
            
            ## <FIXME>
            ## maybe we don't want to save input and response
            ## in the cases below?
            ## </FIXME>
            if (name == "input" && designMatrix) {
                assign("designMatrix",
                       model.matrix(mt, data = MF, ...),
                       envir = envir)
            }

            if (name == "response" && responseMatrix) {
                attr(mt, "intercept") <- 0
                assign("responseMatrix",
                       model.matrix(mt, data=MF, ...),
                       envir = envir)
            }
        }
    }
    
    MEF@set(which = NULL, data = data, frame = frame)
    
    ### handle NA's
    if (!is.null(na.action))
        MEF <- na.action(MEF)
    MEF
}

### compare basic properties of two data.frames

checkData <- function(old, new) {

    if (!is.null(old)){

        if (!identical(lapply(old, class), lapply(new, class)))
            stop("Classes of new data do not match original data")

        if (!identical(lapply(old, levels), lapply(new, levels)))
            stop("Levels in factors of new data do not match original data")
    }
}
  
### parse a formula and return the different pieces as `FormulaParts'
### object

ParseFormula <- function(formula, data = list()) {

    formula <- terms(formula, data = data)
    attributes(formula) <- NULL

    if (length(formula) == 3) {
        fresponse <- formula[c(1,2)]
        frhs <- formula[c(1,3)]
        if (frhs[[2]] == "1")
            frhs <- NULL
    }
  
    if (length(formula) == 2) {
        fresponse <- NULL   
        frhs <- formula
    }
  
    finput <- frhs
    fblocks <- frhs

    ### <FIXME>
    ### will fail for `y ~ . | blocks' constructs
    ### </FIXME>

    if (!is.null(frhs) && length(frhs[[2]]) > 1) {
        if (deparse(frhs[[2]][[1]]) == "|") {
            finput[[2]] <- frhs[[2]][[2]]
            fblocks[[2]] <- frhs[[2]][[3]]
        } else {
            fblocks <- NULL
        }
    } else {
        fblocks <- NULL
    }
  
    RET = new("FormulaParts")
  
    RET@formula$response <- fresponse
    RET@formula$input <- finput
    RET@formula$blocks <- fblocks

    return(RET)
}
