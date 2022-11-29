#' Solves a (linear) goal programming problem
#'
#' Given a set of equations representing goals of a linear goal
#' programming problem, it finds the optimal solution.
#'
#' The actual solution of the linear programming problem is found using lp_solve
#' \url{https://lpsolve.sourceforge.net/}, through its R interface (the lpSolve
#' package).
#'
#' Argument 'eqs' defines the goals of the goal programming problem
#' through easy human-readable text. When writing a constranit, all variables
#' must be on the left-hand side, with only numeric values on the right-hand
#' side. Equations must be valid R expressions. Examples of valid equations
#' are the following:
#' \itemize{
#'   \item \code{"3*x + 2*y = 16"}
#'   \item \code{"4*x -   y =  3"}
#' }
#' On the other hand, the following are not valid expressions:
#' \itemize{
#'   \item \code{"3*x = 16 - 2*y"}
#'   \item \code{"4x + 1y = 5"}
#' }
#'
#' While optional, it is highly encouraged to provide names for each goal.
#' The user can also provide weights and/or lexicographic priorities for the
#' positive (excess) and negative (lack) deviations associated to each
#' goal. The following example shows how to provide this information:
#' \code{"
#' Labour   : 20*A + 12*B + 40*C = 1200 | 0.2 0.1 | 1# 2#
#' Profit   : 11*A + 16*B +  8*C = 1000 | 0.1 0.3 | 3# 4#
#' Batteries:  4*A +  3*B +  6*C =  200 | 0.2 0.1 | 5# 6#"}
#' The name of the goal must be followed by a colon (\code{:}) or split
#' vertical bars (\code{|}). Then the goal. Then the weights associated
#' to the positive deviation first (excess), and the negative deviation (lack)
#' last, separated by an empty space. Finally, the lexicographic priorities
#' for the positive (excess) and negative (lack) deviations can be provided
#' as numbers, each followed by a hashtag, and separated by an space, in
#' that order. Lower values imply a higher priority, and the same priority
#' can be assigned to multiple deviations. Only the equation is mandatory.
#' If the weights are omitted, all of them are assumed to be equal to one.
#' If the lexicographic priorities are omitted, all of them are assumed to
#' be equal to one.
#'
#' @param eqs Character vector describing a set of linear equations. The vector
#'            can either contain a single element with one equation per line,
#'            or multiple elements, each with a single equation. Equations must
#'            be valid R expressions (see details).
#' @param A Numeric matrix with the coefficients of the variables. One row per
#'          equation, one column per variable. Columns can be named according
#'          to the variables they correspond to. Rows can be named for their
#'          corresponding goals. Ignored if argument \code{eqs} is
#'          provided.
#' @param m Character vector with the relationship between the left and
#'          right-hand side of the goals. It can be any of
#'          \code{=, ==, <=, >=}. \code{=} allows for positive (excess) and
#'          negative (lack) deviations. \code{==} do not allow any deviation,
#'          enforcing fulfillment of the goal. \code{<=} automatically assigns
#'          a weight equal to zero to the negative (lack) deviation. \code{>=}
#'          automatically assigns a weight equal to zero to the positive
#'          (excess) deviation.
#' @param b Numeric vector with the values on the right hand side of the
#'          goals. Ignored if argument \code{eqs} is provided.
#' @param w Numeric matrix with the weights associated to the deviations from
#'          each goal. It should have as many rows as goals, and
#'          two columns: the first column corresponding to the weight of the
#'          positive deviation (excess), and the second column corresponding
#'          to the weight of the negative deviation (lack).
#'          This argument is ignored if \code{eqs} is provided.
#'          If omitted and \code{eqs} is not provided either, default weights
#'          are dependent on the type of goal, as follows.
#'          \itemize{
#'            \item \code{=}: Positive and negative deviations are assigned
#'                            equal weights of 1.
#'            \item \code{==}: Positive and negative deviations are assigned
#'                            equal weights of NA, as these deviations will be
#'                            excluded from the problem, i.e. the goal
#'                            will be enforced.
#'            \item \code{>=}: Positive deviation is assigned a weight of
#'                             0, so it does not influence the objective
#'                             function (and therefore the solution to the
#'                             problem). The negative deviation is assigned
#'                             a weight of 1, but if manually set to NA,
#'                             then the inequality is enforced.
#'            \item \code{<=}: Negative deviation is assigned a weight of
#'                             0, so it does not influence the objective
#'                             function (and therefore the solution to the
#'                             problem). The positive deviation is assigned
#'                             a weight of 1, but if manually set to NA,
#'                             then the inequality is enforced.
#'          }
#' @param p Numeric matrix indicating the priority of each deviation under
#'          a lexicographic approach. Lower numbers represent higher
#'          priority (e.g. from lower to higher priority: 1, 2, 3, ...).
#'          It must have as many rows as goals, and two columns.
#'          This argument is ignored if \code{eqs} is provided.
#'          If omitted and not provided in \code{eqs} either, default
#'          priorities are dependent on the type of goal, as follows.
#'          \itemize{
#'            \item \code{=}: Positive and negative deviations are assigned
#'                            equal priority of 1.
#'            \item \code{==}: Positive and negative deviations are assigned
#'                             equal priority of NA, as these deviations will
#'                             be excluded from the problem, i.e. the
#'                             goal will be enforced.
#'            \item \code{>=}: Positive deviation is assigned a priority of
#'                             +Inf, making it irrelevant. The negative
#'                             deviation is assigned a priority of 1.
#'            \item \code{<=}: Negative deviation is assigned a priority of
#'                             +Inf, making it irrelevant. The positive
#'                             deviation is assigned a priority of 1.
#'          }
#' @param varType Named character vector. Defines the type of each variable.
#'                It can be defined as \code{c(x1="int", x2="cont")}. Omitted
#'                variables are assumed to be integer. Each element can be
#'                either \code{"continuous"} (i.e. non-negative real values),
#'                \code{"integer"} (i.e. non-negative natural values), or
#'                \code{"binary"} (i.e. only take values 0 or 1). Using only
#'                the first letters is accepted too. If omitted, all variables
#'                are assumed to be integer.
#' @param normW Logical. TRUE to scale the weights by the inverse of the
#'              corresponding right-hand size value of the goal (\code{b}).
#'              Useful to balance the relevance of all goals.
#'              Equivalent to normalising the problem so \code{b=1} for all
#'              goals.
#' @param silent Logical. TRUE to prevent the function writing anything
#'               to the console (or the default output). Default is FALSE.
#'
#' @return goalp object. It contains the following elements.
#'         \itemize{
#'           \item \code{A}: The coefficient matrix of the decision variables.
#'                           It does not include the coefficients of the
#'                           deviations.
#'           \item \code{m}: The relationship between the left- and right-hand
#'                           side of the goals.
#'           \item \code{b}: The right-hand side of the goals.
#'           \item \code{w}: The weights of the deviation variables.
#'           \item \code{p}: The lexicographic priorities of deviations
#'                           variables.
#'           \item \code{A_}: The coefficient matrix of the decision and
#'                            deviation variables.
#'           \item \code{w_}: The weights of the decision and deviation
#'                            variables.
#'           \item \code{eqs}: Text version of the goal programming problem.
#'           \item \code{varType}: Vector describing the type of the decision
#'                                 variables (binary, integer, or continuous).
#'           \item \code{x}: Optimal value of the decision variables.
#'           \item \code{d}: Optimal value of the deviation variables.
#'           \item \code{obj}: The value of the objective function (sum of
#'                             weighted deviations). If using lexicographic
#'                             priorities, the value for the objective
#'                             function using all deviations (i.e. ignoring the
#'                             priority) in each stage.
#'           \item \code{X}: The value of the decision variables for the
#'                           optimal solution in each stage of the
#'                           lexicographic problem. If there are no
#'                           lexicographic priorities, then a single row matrix.
#'           \item \code{lp}: lp object describing the solution of the
#'                            underlying linear programming problem. See
#'                            \link[lpSolve]{lp.object}. When using
#'                            lexicographic priorities, the solution to the
#'                            last stage.
#'           \item \code{solutionFound}: Logical taking value TRUE if a
#'                                       solution was found, FALSE otherwise.
#'         }
#' @importFrom stats setNames
#' @export
goalp <- function(eqs, A=NULL, m=NULL, b=NULL, w=NULL, p=NULL,
                  varType=NULL, normW=FALSE, silent=FALSE){
  ### Validate input
  # Parse if necessary
  if(!missing(eqs)){
    test <- !silent && (!is.null(A) || !is.null(b))
    if(test) msg("NOTICE: Argument 'eqs' was provided, so arguments 'A', 'b', ",
                 "and 'w' will be ignored.")
    L <- parseGoal(eqs)
    A <- L$A
    m <- L$m
    b <- L$b
    w <- L$w
    p <- L$p
    rm(L)
  }
  # Validate A, b, m, w, p
  L <- validateMatrices(A, b, m, w, p, setDefaults=TRUE, silent=silent)
  for(i in 1:length(L)) assign(names(L)[i], L[[i]], envir=environment())
  rm(L, i)
  # Validate varType
  varTDef <- "i"
  if(is.null(varType)) varType <- setNames(rep(varTDef, ncol(A)), colnames(A))
  test <- is.vector(varType) && is.character(varType)
  if(!test) stop("Argument 'varType' must be a character vector.")
  test <- all(varType %in% c("c", "cont", "continuous",
                             "i", "int", "integer",
                             "b", "bin", "binary"))
  if(!test) stop("Elements in argument 'varType' can take values ",
                 '"continuous", "cont", "integer", "int", "binary", or "bin". ',
                 "No other values are allowed.")
  if(length(varType)==1) varType <- setNames(rep(varType, ncol(A)), colnames(A))
  if(!is.null(names(varType))){
    if(is.null(colnames(A))) stop("Variables are not named, so argument ",
                                  "'varType' cannot be defined using ",
                                  "variable names")
    test <- names(varType)[!(names(varType) %in% colnames(A))]
    if(length(test)>0) stop("Variables '", paste(test, collapse="', '"),
                            "' are not used in the goals or are not",
                            "among the names of the columns of matrix 'A'.")
    test <- length(unique(names(varType)))==length(varType)
    if(!test) stop("Repeated variables names found in argument 'varType'.")
    if(length(varType)<ncol(A)){
      varType <- c(varType,
                   setNames(rep(varTDef, ncol(A)-length(varType)),
                            colnames(A)[!(colnames(A) %in% names(varType))]))
    }
    varType <- varType[colnames(A)]
  }
  test <- length(varType)==ncol(A)
  if(!test) stop("Argument 'varType' must have either one element, or as many ",
                 "as variables in the problem.")
  varType <- substr(tolower(varType), 1, 1)
  rm(test, varTDef)
  # Normalise weights if requested
  if(normW){
    test <- all( w[!is.na(w)] %in% 0:1 ) && !silent
    if(!test) msg("NOTICE: Weights given by the user will be divided by the ",
                  "right-hand side  of each goal.")
    w <- w/b
  }


  ### Solution
  nV <- ncol(A)
  nC <- nrow(A)
  pp <- sort(unique(as.vector(p)), decreasing=FALSE, na.last=NA)
  pp <- pp[is.finite(pp)] # any p = +Inf will never be considered
  bb <- b
  dN <- matrix(paste0(rownames(A), rep(c("-", "+"), each=nC)),
               nrow=nC, ncol=2)
  x  <- setNames(rep(0, nV), colnames(A))
  ww <- w
  X  <- matrix(NA, nrow=length(pp), ncol=nV, dimnames=list(NULL, colnames(A)))
  obj<- rep(NA, length(pp))
  for(l in 1:length(pp)){
    # Print progress
    if(!silent){
      if(length(pp)==1) msg("Solving underlying linear optimisation problem.")
      if(length(pp)>1) msg("Solving for priority level ", pp[l], " ",
                           "(", l, "/", length(pp), ").")
    }
    # Update weights
    for(i in 1:nC) for(j in 1:2) if(!is.na(ww[i,j])){
      if(p[i,j]>pp[l]) ww[i,j] <- 0 else ww[i,j] <- w[i,j]
    }
    # Solve with modified b and weights
    lp <- solveGP(A, bb, ww, varType, silent=silent)
    # Check and store solution
    if(lp$status==0) x <- setNames(lp$solution[1:nV], colnames(A)) else break
    X[l,]  <- x
    obj[l] <- 0
    for(i in 1:nC){
      tmp <- b[i] - sum(A[i,]*x)
      tmp <- abs(tmp)*w[i, ifelse(tmp<0, 2, 1)]
      if(!is.na(tmp)) obj[l] <- obj[l] + tmp
    }; rm(tmp)
    if(l>1 && obj[l]>obj[l-1]) break
    # Remove deviations solved for
    tmp <- dN[ww!=0]
    tmp <- tmp[!is.na(tmp)]
    for(j in tmp){
      con <- substr(j, 1, nchar(j)-1)
      sig <- substr(j, nchar(j), nchar(j))
      val <- lp$solution[names(lp$objective)==j]
      bb[con] <- bb[con] + ifelse(sig=="+", 1, -1)*val
      ww[con, ifelse(sig=="+",2,1)] <- NA
    }
  }
  # trim X, obj
  obj <- obj[!is.na(obj)]
  X   <- X[1:length(obj),, drop=FALSE]

  ### Return
  gp <- new_goalp(lp, A, m, b, w, p, varType, X, obj)
  gp <- validate_goalp(gp)
  #gp <- tryCatch(validate_goalp(gp),                       # DEBUG
  #               error=function(e){print(e); return(gp)})  # DEBUG
  return(gp)
}
