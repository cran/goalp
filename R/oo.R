#' Constructor of goalp object
#'
#' It doesn't do any checks, but it does generate objects
#' \itemize{
#'   \item \code{x}: Vector with the optimal value of decision variables.
#'   \item \code{d}: Matrix with the optimal value of the deviations.
#'   \item \code{solutionFound}: TRUE if a solution was found, FALSE otherwise.
#' }
#'
#' @title new_goalp: Creates a new goalp object
#' @param lp lp object. The solution of the underlying linear program.
#' @param A Numeric matrix with goals coefficients. Only for original
#'          variables. Rows and columns must be named.
#' @param m Character vector containg the relation between Ax and b. Each
#'          element can be \code{=, ==, >, <. >=, <=}.
#' @param b Numeric vector with the right hand side of the goals.
#' @param w Numeric matrix (nC x 2) with the weights of each deviation.
#' @param p Numeric matrix containing the priorities of each deviation
#'          variable for lexicographic goal programming. Lower numbers imply
#'          higher priority.
#' @param varType Character vector describing the type of the original
#'                variables, as either "b", "i", or "c".
#' @param X Numeric matrix with the value of the (decision) variables in
#'          each iteration of the lexicographic optimisation.
#' @param obj Numeric vector with the value of the objective function in
#'            each iteration of the lexicographic optimisation.
#' @param eqs Character vector with the human-readable formulation of the
#'            problem. Generated automatically from A, b and w if not provided.
#' @return A goalp object.
#' @importFrom stats setNames
new_goalp <- function(lp, A, m, b, w, p, varType, X, obj, eqs){
  ### Check input
  if(missing(lp)) stop("Cannot create a goalp object without an 'lp' object.")
  if(missing( A)) stop("Cannot create a goalp object without an 'A' matrix.")
  if(missing( b)) stop("Cannot create a goalp object without a 'b' vector.")
  if(missing( m)) stop("Cannot create a goalp object without an 'm' vector.")
  if(missing( w)) stop("Cannot create a goalp object without a 'w' vector.")
  if(missing(varType)) stop("Cannot create a goalp object without a ",
                            "'varType' vector.")
  if(missing(X)) stop("Cannot create a goalp object without an 'X' matrix.")
  if(missing(obj)) stop("Cannot create a goalp object without an 'obj' vector.")

  ### Create eqs if it is missing
  nC <- nrow(A)
  nV <- ncol(A)
  if(missing(eqs)){
    eqs <- rep("", nC)
    for(i in 1:nC){
      if(m[i] %in% c("=", "==", ">=", "<=")){
        # Write name
        eqs[i] <- paste0(rownames(A)[i], " : ")
        # Write equation or inequality
        first <- TRUE
        for(j in 1:nV) if(A[i,j]>0){
          if(first) eqs[i] <- paste0(eqs[i],
                                     ifelse(A[i,j]==1, "", paste0(A[i,j], "*")))
          if(!first) eqs[i] <- paste0(eqs[i], ifelse(A[i,j]>0, " + ", " - "),
                                      ifelse(A[i,j]==1, "",
                                             paste0(abs(A[i,j]),"*")) )
          eqs[i] <- paste0(eqs[i], colnames(A)[j])
          first <- FALSE
        }
        eqs[i] <- paste(eqs[i], m[i], b[i])
        # Write weights
        eqs[i] <- paste0(eqs[i], " | ", formatC(w[i,1], digits=4, width=-1),
                         " ", formatC(w[i,2], digits=4, width=-1))
        # Write lexicographic priorities
        eqs[i] <- paste0(eqs[i], " | ", p[i,1], "# ", p[i,2], "#")
      }
      if(m[i] %in% c("lBound", "uBound")){
        # Write bound
        eqs[i] <- paste(colnames(A)[A[i,]==1], m[i], b[i])
      }
    }
    rm(i, j, first)
  }

  ### Create x, d, obj
  if(length(obj)>0){ # If a solution was found
    solutionFound <- TRUE
    x   <- X[nrow(X),]
    tmp <- b - A%*%x
    d   <- matrix(0, nrow=nC, 2, dimnames=list(rownames(A), c("d-", "d+")))
    for(i in 1:nC) d[i,ifelse(tmp[i]<0,2,1)] <- abs(tmp[i])
    d[is.na(w)] <- NA
  } else { # If a solution was not found
    solutionFound <- FALSE
    x <- setNames(rep(NA, nV), colnames(A))
    d <- matrix(NA, nrow=nC, ncol=2, dimnames=list(rownames(A), c("d-", "d+")))
  }


  ### Create A_, w_ of the first lexicographic iteration
  nD <- sum(!is.na(w))
  w_ <- rep(-1, nD)
  A_ <- matrix(0, nrow=nC, ncol=nD, dimnames=list(rownames(A), rep("", nD)))
  k  <- 1
  for(i in 1:nC) for(j in 1:2) if(!is.na(w[i,j])){
    colnames(A_)[k] <- paste0(rownames(A)[i], ifelse(j==1, "-", "+"))
    A_[i,k]         <- ifelse(j==1, 1, -1)
    w_[  k]         <- w[i,j]
    k               <- k + 1
  }; rm(i,j,k)
  A_ <- cbind(A, A_)
  w_ <- setNames(c(rep(0, nV), w_), colnames(A_))

  ### Put everything together and return
  gp <- list(A  = A,
             b  = b,
             m  = m,
             w  = w,
             p  = p,
             A_ = A_,
             w_ = w_,
             x  = x,
             d  = d,
             obj = obj,
             X   = X,
             eqs = eqs,
             varType = varType,
             lp  = lp,
             solutionFound = solutionFound)
  class(gp) <- "goalp"
  return(gp)
}

#' Checks that the internals of a goalp object are consistent.
#'
#' @title: validate_goalp: A validator for goalp objects.
#' @param gp A goalp object.
#' @return The unmodified input invisibly.
validate_goalp <- function(gp){
  if(!inherits(gp, "goalp")) stop("Argument gp must be a goalp object.")
  ### Check A, b, m, w, p
  validateMatrices(gp$A, gp$b, gp$m, gp$w, gp$p, silent=TRUE)

  ### Check A_, w_
  test <- is.matrix(gp$A_) && is.numeric(gp$A) && nrow(gp$A_)==nrow(gp$A)
  test <- test && ncol(gp$A_)==length(gp$w_)
  if(!test) stop("'A_' must be a numeric matrix with as many rows as ",
                 "goals, and as many columns as original decision ",
                 "variables plus deviations.")
  test <- is.vector(gp$w_) && is.numeric(gp$w_) && length(gp$w_)==ncol(gp$A_)
  if(!test) stop("'w_' must be a numeric vector with as many elements as ",
                 "variables plus deviations.")

  ### Check varType, eqs
  test <- is.vector(gp$varType) && is.character(gp$varType)
  test <- test && length(gp$varType)==ncol(gp$A)
  if(!test) stop("'varType' must be a character vector with as many elements ",
                 "as variables.")
  test <- is.vector(gp$eqs) && is.character(gp$eqs)
  if(!test) stop("'eqs' must be a character vector.")

  ### Check x, d, obj, X, eqs, lp
  test <- is.vector(gp$x) && (is.numeric(gp$x) || anyNA(gp$x)) &&
    length(gp$x)==ncol(gp$A)
  if(!test) stop("'x' must be a numeric vector with as many elements as ",
                 "variables.")
  test <- is.matrix(gp$d) && (is.numeric(gp$d) || anyNA(gp$d)) &&
    all(dim(gp$d)==dim(gp$w))
  if(!test) stop("'d' must be a numeric matrix with as many rows as ",
                 "goals and two columns.")
  test <- is.vector(gp$obj) && (is.numeric(gp$obj) || length(gp$obj)==0)
  if(!test) stop("'obj' must be a numeric vector.")
  test <- anyNA(gp$X) || (is.matrix(gp$X) && is.numeric(gp$X) &&
                            nrow(gp$X)==length(gp$obj))
  if(!test) stop("'X' must be a numeric matrix with as many rows as ",
                 "iterations of the lexicographic problem, or one row ",
                 "if no lexicographic priorities are set.")
  if(!inherits(gp$lp,"lp")) stop("'lp' must be an lp object.")
  #test <- !is.null(gp$lp$solution) && is.vector(gp$lp$solution)
  #test <- test && is.numeric(gp$lp$solution)
  #test <- test && length(gp$lp$solution)==ncol(gp$A_)
  #if(!test) stop("'lp' must contain a numeric vector called 'solution' with ",
  #               "as many elements as variables plus twice the number of ",
  #               "goals.")

  ### Return
  return(invisible(gp))
}

#' Prints a summary of a goalp object to the console.
#'
#' @title: summary.goalp: Prints a summary of a goalp object to screen.
#' @param object A goalp object.
#' @param ... Additional arguments. Ignored.
#' @return No return value (NULL). Called for its side effect of
#'         printing a summary of a goalp object to the standard output
#'         (usually the console).
#' @export
summary.goalp <- function(object, ...){
  # Header
  v <- tryCatch(utils::packageDescription("goalp", fields = "Version"),
                warning=function(w) return("v?"),
                error=function(e) return("v?"))
  txt <- paste("Goal programming model run by", Sys.info()['user'],
               "using goalp", v,
               "on R", paste0(version$major, ".", version$minor),
               "for", Sys.info()['sysname'])
  msg(txt)
  cat("\n")

  # Equations (only if they are less than 20)
  constLim <- 20
  if(nrow(object$A)<=constLim){
    msg("Problem formulation:")
    eqs <- object$eqs
    if(length(eqs)==1) eqs <- trimws(strsplit(eqs, "\n")[[1]])
    for(e in eqs) msg(e)
  } else {
    e <- match.call()[[2]]
    if(is.symbol(e)) e <- as.character(e) else e <- ""
    msg("Problem formulation not printed because there are more than ",
        constLim, " goals. Use print(", e, ") to see them all.")
    rm(e)
  }
  rm(constLim)
  cat("\n")

  # Objective function
  if(object$solutionFound){
    msg("Objective function value: ", formatC(object$obj[length(object$obj)],
                                              digits=6, width=-1))
  } else msg("A solution to the problem could not be found.")
  cat("\n")

  # Variables
  vt <- c(b="binary", i="integer", c="continuous")
  DF <- data.frame(value = object$x,
                   type  = vt[object$varType])
  rownames(DF) <- names(object$x)
  msg("Solution:")
  print(DF)
  cat("\n")

  # Deviations
  D <- cbind(object$d, `w-`=object$w[,1], `w+`=object$w[,2],
             `p-`=object$p[,1], `p+`=object$p[,2])
  msg("Deviations:")
  print(D)

  # return nothing
  return(invisible(NULL))
}


#' Prints a human-readable formulation of a goal programming problem.
#'
#' @title: print.goalp: Prints a summary of a goalp object to screen.
#' @param x A goalp object.
#' @param ... Additional arguments. Ignored.
#' @return A scalar character (i.e. a text string) with a human-readable
#'         formulation of the goal programming problem represented by
#'         goalp object \code{x}. This can be edited and used as an input
#'         to \link{goalp}, if modifications to the goal programming problem
#'         are required.
#' @export
print.goalp <- function(x, ...){
  if(is.null(x$eqs)) stop("Problem formulation is not available in text format.")
  eqs <- x$eqs
  if(length(eqs)>1) eqs <- paste0(eqs, collapse="\n")
  cat(eqs)
  return(invisible(eqs))
}

#' Message function
#'
#' @title: msg: Formats and prints a message to screen.
#' @param ... A series of objects (usually strings and numbers) to concatenate
#'            and print to screen.
#' @return NULL
msg <- function(...) writeLines(strwrap(paste0(...), exdent=2))
