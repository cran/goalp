#' Validates the input of a goal programming problem
#'
#' @param A Numeric matrix with the coefficients of the variables. One row
#'          per equation, one column per variable.
#' @param m Character vector with the relationship between the left and
#'          right-hand side of the goals. It can be any of
#'          \code{=, ==, <=, >=}.
#' @param b Numeric vector with the values on the right hand side of the
#'          goals.
#' @param w Numeric matrix with the weights associated to the deviations
#'          from each goal. It should have as many rows as goals,
#'          and two columns: the first column corresponding to the weight of
#'          the positive deviation (excess), and the second column
#'          corresponding to the weight of the negative deviation (lack).
#' @param p Numeric matrix indicating the priority of each deviation under
#'          a lexicographic approach. Lower numbers represent higher
#'          priority (e.g. from lower to higher priority: 1, 2, 3, ...).
#'          It must have as many rows as goals, and two columns.
#' @param setDefaults Scalar logical. If TRUE, A, b, m, w, and p are filled
#'                    in with default values as required.
#' @param silent Logical. TRUE to prevent the function writing anything
#'               to the console (or the default output). Default is FALSE.
validateMatrices <- function(A, b, m, w=NULL, p=NULL,
                             setDefaults=FALSE, silent=FALSE){
  # Check A
  test <- is.matrix(A) && is.numeric(A)
  if(!test) stop("Argument 'A' must be a numeric matrix.")
  if(setDefaults){
    if(is.null(colnames(A))) colnames(A) <- paste0("x", 1:ncol(A))
    if(is.null(rownames(A))) rownames(A) <- paste0("goal", 1:nrow(A))
  }
  test <- !is.null(colnames(A)) && !is.null(rownames(A))
  if(!test) stop("Rows and columns of matrix 'A' must be named.")
  test <- length(unique(rownames(A)))==nrow(A)
  if(!test) stop("Names of goals must be unique.")
  test <- length(unique(colnames(A)))==ncol(A)
  if(!test) stop("Names of variables must be unique.")

  # Check m
  test <- is.vector(m) && is.character(m) && length(m)==nrow(A) &&
    all(m %in% c("=", "==", "<=", ">=", "lBound", "uBound"))
  if(!test) stop("Argument 'm' must be a character vector with as many ",
                 "elements as goals, and each element can only be  ",
                 '"=", "==", ">=", "<=", "lBound", or "uBound".')
  if(!is.null(names(m))){
    test <- all(names(m) %in% rownames(A)) && all(rownames(A) %in% names(m)) &&
      length(unique(names(m)))==length(m)
    if(!test) stop("Argument 'm' must be named the same as the goals, ",
                   "with no repeated names.")
    if(any(names(m)!=rownames(A))) m <- m[rownames(A)]
  }
  r <- m %in% c("lBound", "uBound")
  if(any(r)){
    test <- apply(A[r,,drop=FALSE], MARGIN=1, function(x) all(x %in% 0:1)) &
      rowSums(A[r,,drop=FALSE])==1
    if(any(!test)) stop("Rows in matrix A associated to lBound or uBound ",
                         "goals can only be filled with zeros, except ",
                         "for a single 1 per row. Rows ",
                         paste0(rownames(A)[r][!test], collapse=", "),
                         " do not fulfill this requirement.")
    test <- colSums(A[m=="lBound",,drop=FALSE])>1
    if(any(test)) stop("Lower bound defined more than once for variables ",
                       paste0(colnames(A)[test], collapse=", "))
    test <- colSums(A[m=="uBound",,drop=FALSE])>1
    if(any(test)) stop("Upper bound defined more than once for variables ",
                       paste0(colnames(A)[test], collapse=", "))
  }
  if(setDefaults && is.null(names(m))){
    r <- m %in% c("lBound", "uBound")
    if(all(rownames(A)==paste0("goal", 1:nrow(A)))) for(i in which(r)){
      rownames(A)[i] <- paste0(colnames(A)[A[i,]>0], "_", m[i])
    }
    names(m) <- rownames(A)
  }
  rm(r)

  # Check b
  test <- is.vector(b) && is.numeric(b)
  if(!test) stop("Argument 'b' must be a numeric vector.")
  test <- length(b)==nrow(A)
  if(!test) stop("Argument 'b' must have as many elements as goals.")
  if(setDefaults && is.null(names(b))) names(b) <- rownames(A)
  test <- all(names(b) %in% rownames(A)) && all(rownames(A) %in% names(b))
  if(!test) stop("The row names in 'A' do not match the names in 'b'")
  if(!all(names(b)==rownames(A)) && setDefaults) b <- b[rownames(A)]



  # Check w
  if(setDefaults && is.null(w)){
    w <- matrix(1, nrow=nrow(A), ncol=2)
    for(i in 1:nrow(A)){
      if(m[i]=="="     ) w[i,] <- c( 1,  1)
      if(m[i]=="=="    ) w[i,] <- c(NA, NA)
      if(m[i]==">="    ) w[i,] <- c( 1,  0)
      if(m[i]=="<="    ) w[i,] <- c( 0,  1)
      if(m[i]=="lBound") w[i,] <- c(NA,  0)
      if(m[i]=="uBound") w[i,] <- c( 0, NA)
    }
  }
  test <- is.matrix(w) && is.numeric(w) && all(dim(w)==c(nrow(A), 2))
  if(!test) stop("Argument 'w' must be a numeric matrix with as many rows ",
                 "as goals, and two columns.")
  test <- any(is.infinite(w))
  if(test) stop("There cannot be any weights with value equal to infinity.")
  if(any(w[!is.na(w)]<0)) msg("NOTICE: Some negative weights have been ",
                              "provided. This will cause the corresponding ",
                              "deviations to be maximised.")
  if(setDefaults && is.null(colnames(w))) colnames(w) <- c("w-", "w+")
  if(setDefaults && is.null(rownames(w))) rownames(w) <- rownames(A)
  for(i in 1:nrow(A)){
    cn   <- rownames(A)[i]
    test <- m[i]!="=" || !anyNA(w[i,])
    if(!test) stop("Weights cannot be NA when using '=' in ", cn)
    test <- m[i]!="==" || all(is.na(w[i,]))
    if(!test) stop("Weights must be NA when using '==' in ", cn)
    #test <- m[i]!=">=" || !is.na(w[i,1])
    #if(!test) stop("goal ", cn, " is a >= inequality. Therefore the ",
    #               "weight of its negative deviation cannot be NA, ",
    #               "yet it is set to ", w[i,1])
    test <- !silent && m[i]==">=" && is.na(w[i,1])
    if(test) msg("NOTICE: goal ", cn, " is a >= inequality with a ",
                 "negative deviation weight equal to NA, meaning the >= ",
                 "inequality will be enforced.")
    test <- m[i]!=">=" || (!is.na(w[i,2]) && w[i,2]==0)
    if(!test) stop("goal ", cn, " is a >= inequality. Therefore the ",
                   "weight of its positive deviation must be zero, ",
                   "yet it is set to ", w[i,2])
    test <- m[i]!="<=" || (!is.na(w[i,1]) && w[i,1]==0)
    if(!test) stop("goal ", cn, " is a <= inequality. Therefore the ",
                   "weight of its negative deviation must be zero, ",
                   "yet it is set to ", w[i,1])
    test <- !silent && m[i]=="<=" && is.na(w[i,2])
    if(test) msg("NOTICE: goal ", cn, " is a <= inequality with a ",
                 "negative deviation weight equal to NA, meaning the <= ",
                 "inequality will be enforced.")
    test <- m[i]!="lBound" || is.na(w[i,1])
    if(!test) stop("goal ", cn, " is a lower bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the weight of its negative deviation must be NA, ",
                   "yet it is set to ", w[i,1])
    test <- m[i]!="lBound" || (!is.na(w[i,2]) && w[i,2]==0)
    if(!test) stop("goal ", cn, " is a lower bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the weight of its positive deviation must be zero, ",
                   "yet it is set to ", w[i,2])
    test <- m[i]!="uBound" || (!is.na(w[i,1]) && w[i,1]==0)
    if(!test) stop("goal ", cn, " is an upper bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the weight of its negative deviation must be zero, ",
                   "yet it is set to ", w[i,1])
    test <- m[i]!="uBound" || is.na(w[i,2])
    if(!test) stop("goal ", cn, " is an upper bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the weight of its positive deviation must be NA, ",
                   "yet it is set to ", w[i,2])

  }

  # Check lexicographic priorities
  if(setDefaults && is.null(p)){
    p <- matrix(1, nrow=nrow(A), ncol=2)
    for(i in 1:nrow(A)){
      if(m[i]=="="     ) p[i,] <- c(  1,  1)
      if(m[i]=="=="    ) p[i,] <- c( NA, NA)
      if(m[i]==">="    ) p[i,] <- c(  1,Inf)
      if(m[i]=="<="    ) p[i,] <- c(Inf,  1)
      if(m[i]=="lBound") p[i,] <- c(NA, Inf)
      if(m[i]=="uBound") p[i,] <- c(Inf, NA)
    }
  }
  test <- is.matrix(p) && is.numeric(p) && all(dim(p)==dim(w))
  if(!test) stop("Argument 'p' must be a numeric matrix with as many rows ",
                 "as goals, and two columns.")
  test <- all(is.na(w)==is.na(p))
  if(!test) stop("If arguments 'w' and 'p' contain NA values, they ",
                 "must be in the same positions.")
  if(setDefaults && is.null(colnames(p))) colnames(p) <- c("p-", "p+")
  if(setDefaults && is.null(rownames(p))) rownames(p) <- rownames(A)
  for(i in 1:nrow(A)){
    cn   <- rownames(A)[i]
    test <- m[i]!="=" || !anyNA(p[i,])
    if(!test) stop("Lexicographic priorities cannot be NA when using '=' in ", cn)
    test <- m[i]!="==" || all(is.na(p[i,]))
    if(!test) stop("Lexicographic priorities must be NA when using '==' in ", cn)
    #test <- m[i]==">=" && is.na(p[i,1]) && !silent
    #if(test) msg("NOTICE: Goal ", cn, " is a >= inequality with a ",
    #             "negative deviation priority equal to NA, meaning the >= ",
    #             "inequality will be enforced.")
    test <- m[i]!=">=" || is.infinite(p[i,2])
    if(!test) stop("goal ", cn, " is a >= inequality. Therefore the ",
                   "lexicographic priority of its positive deviation must be ",
                   "positive infinity, yet it is set to ", p[i,2])
    test <- m[i]!="<=" || is.infinite(p[i,1])
    if(!test) stop("goal ", cn, " is a <= inequality. Therefore the ",
                   "lexicographic priority of its negative deviation must be ",
                   "positive infinity, yet it is set to ", p[i,1])
    #test <- m[i]=="<=" && is.na(p[i,2]) && !silent
    #if(test) smg("NOTICE: Goal ", cn, " is a <= inequality with a ",
    #             "negative deviation priority equal to NA, meaning the <= ",
    #             "inequality will be enforced.")
    test <- m[i]!="lBound" || is.na(p[i,1])
    if(!test) stop("goal ", cn, " is a lower bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the lexicographic priority of its negative deviation must ",
                   "be NA, yet it is set to ", p[i,1])
    test <- m[i]!="lBound" || is.infinite(p[i,2])
    if(!test) stop("goal ", cn, " is a lower bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the lexicographic priority of its positive deviation must ",
                   "be positive infinity, yet it is set to ", p[i,2])
    test <- m[i]!="uBound" || is.infinite(p[i,1])
    if(!test) stop("goal ", cn, " is an upper bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the lexicographic priority of its negative deviation must ",
                   "be positive infinity, yet it is set to ", p[i,1])
    test <- m[i]!="uBound" || is.na(p[i,2])
    if(!test) stop("goal ", cn, " is an upper bound for decision ",
                   "variable ", colnames(A)[which(A[i,]>0)], ". Therefore, ",
                   "the lexicographic priority of its positive deviation must ",
                   "be NA, yet it is set to ", p[i,2])
  }

  # Return
  return(list(A=A, b=b, m=m, w=w, p=p))
}
