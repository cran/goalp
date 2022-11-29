#' Parses text describing goal programming problem.
#'
#' Given a character vector describing a series of linear equations, it parses
#' them into an \code{A} numerical matrix describing the variables coefficient
#' in the left hand size, a \code{b} numerical vector with values on the right
#' hand size, and an \code{m} character vector indicating the relation between
#' the left and right hand side (\code{=, ==, <=, >=, <, >}).
#'
#' This function can only parse linear equations. All variables must be on the
#' left-hand side, with only numeric values on the right-hand side. Equations
#' must be valid R expressions. Examples of valid equations are the following:
#' \itemize{
#'   \item \code{"3*x + 2*y = 16"}
#'   \item \code{"4*x -   y =  3"}
#' }
#' The following are not valid expressions:
#' \itemize{
#'   \item \code{"3*x = 16 - 2*y"}
#'   \item \code{"4x + 1y = 5"}
#' }
#'
#' Signs \code{=} and \code{==} are considered equivalent, and the first will
#' be replaced by the second after parsing.
#'
#' Optionally, names, weights and lexicographic priorities can be provided for
#' each goal (equation) using the following format:
#' \code{"
#' Labour   : 20*A + 12*B + 40*C = 1200 | 0.2 0.1 | 1# 2#
#' Profit   : 11*A + 16*B +  8*C = 1000 | 0.1 0.3 | 3# 4#
#' Batteries:  4*A +  3*B +  6*C =  200 | 0.2 0.1 | 5# 6#"}
#' The name of the goal must be followed by a colon (\code{:}) or
#' vertical bars (\code{|}). Then the goal. Then the weights associated
#' to the negative deviation first (lack), and the positive deviation (excess)
#' last, separated by an empty space. Finally, the lexicographic priorities
#' for the negative (lack) and positive (excess) deviations can be provided
#' as numbers, each followed by a hashtag (\code{#}), and separated by an
#' space, in that order. Lower values imply a higher priority, and the same
#' priority can be assigned to multiple deviations. Only the equation is
#' mandatory. If the weights are omitted, all of them are assumed to be
#' equal to one for equations with the \code{=} sign. If the equation is
#' actually an inequality with \code{>=}, then the default positive (excess)
#' deviation weight is zero. If \code{<=}, then the default negative (lack)
#' deviation is zero. If the lexicographic priorities are omitted, all of them
#' are assumed to be equal to one for equations, but for inequalities \code{>=}
#' the positive (excess) deviation is given a priority of +Inf (i.e. it will
#' never be minimised), and for inequalities \code{<=} the negative (lack)
#' deviation is given a default priority of +Inf (i.e. it will never be
#' minimised).
#'
#' @param eqs Character vector describing a set of linear equations. The vector
#'            can either contain a single element with one equation per line,
#'            or multiple elements, each with a single equation. Equations must
#'            be valid R expressions (see details).
#'
#' @return List with five elements.
#'         \itemize{
#'           \item \code{A}: Numeric matrix with the coefficients of the
#'                           variables. One row per equation, one column per
#'                           variable. Columns are named according to the
#'                           variables they represent. Rows are named for each
#'                           equation, if a name for them was provided.
#'           \item \code{b}: Numeric vector with the values on the right
#'                           hand side of the equations.
#'           \item \code{m}: Character vector with as many elements as
#'                           equations. Each element is one of
#'                           \code{=, ==, <=, >=, <, >}.
#'           \item \code{w}: Numeric matrix with the weights associated to
#'                           the deviations of each goal. Each row
#'                           corresponds to a goal. The first column
#'                           corresponds to the positive deviation (excess)
#'                           and the second column to the negative deviation
#'                           (lack).
#'           \item \code{p}: Numeric matrix with the lexicographic priority
#'                           associated to each goal. Lower values
#'                           represent higher priority. Each row corresponds
#'                           to a goal. The first column corresponds
#'                           to the positive deviation (excess) and the second
#'                           column to the negative deviation (lack).
#'         }
#' @importFrom stats setNames runif
parseGoal <- function(eqs) {
  ### Check input
  if(!is.character(eqs)) stop("Argument 'eqs' must be a character vector.")
  if(length(eqs)==1 && grepl("\n", eqs)){
    eqs <- strsplit(eqs, "\n")[[1]]
    eqs <- eqs[eqs!=""]
  }
  eqs <- trimws(eqs)

  ### Split among general goals and bounds
  r   <- grepl("range|lBound|uBound", eqs)
  bnd <- eqs[ r]
  eqs <- eqs[!r]
  rm(r)

  ### Parse ranges and bounds
  if(length(bnd)==0){
    R    <- NULL
    nREq <- 0
  } else {
    # Validate bounds and ranges
    test <- any(grepl("[=<>#|:]", bnd))
    if(test) stop("Variable bounds (or ranges) must be described in a ",
                  'line each, without any "=", "<", ">", ":", "|", or "#" ',
                  "symbols.")
    # Count variables with bounds
    L <- function(l){l <- trimws(l); l <- l[l!=""]; return(l)}
    L <- lapply(strsplit(bnd, " ", fixed=TRUE), L)
    n <- unique(sapply(L, "[", 1))
    R <- matrix(c(0,Inf), nrow=length(n), ncol=2, byrow=TRUE,
                dimnames=list(n, c("lBound","uBound")))
    NB <- matrix(0, nrow=length(n), ncol=3, dimnames=list(n, c("r", "l", "u")))
    for(l in L){
      ll <- paste(l, collapse=" ")
      if(l[2]=="range"){ # Parse range
        if(NB[l[1],"r"]>0) stop("Cannot define range twice for ", l[1])
        if(sum(NB[l[1],c("l", "u")])>0) stop("Cannot use range together with ",
                                             "lBound or uBound for ", l[1])
        NB[l[1],"r"] <- NB[l[1],"r"] + 1
        if(length(l)>3) l[3] <- paste(l[3:length(l)], collapse="")
        test <- grepl("[][()]", l[3]) && grepl(",", l[3])
        if(!test) stop("Cannot parse ", ll, ". It must follow the ",
                       'format "varName range [lowerBound, upperBound]".')
        x <- trimws(strsplit(l[3], ",")[[1]]); x <- x[x!=""]
        x <- sub("[])[(]", "", x)
        if(length(x)!=2) stop("Cannot parse lower and upper bounds in ", ll)
        for(j in 1:2) if(x[j]!="") R[l[1],j] <- tryCatch(eval(str2lang(x[j])),
                                                         error=function(e) NaN)
        rm(x)
      } else if(l[2] %in% c("lBound", "uBound")){ # Parse bound
        if(NB[l[1], "r"]>0) stop("Cannot use lBound or uBound together with ",
                                 "range for ", l[1])
        if(l[2]=="lBound") j <- 1 else j <- 2
        if(NB[l[1], 1+j]>0) stop("Cannot define ",
                                 ifelse(j==1, "lBound", "uBound"), " twice ",
                                 "for ", l[1])
        NB[l[1], 1+j] <- NB[l[1], 1+j] + 1
        R[l[1], j] <- tryCatch(eval(str2lang(l[3])), error=function(e) NaN)
      } else stop("Cannot parse bound or range ", ll)
      test <- any(is.nan(R[l[1],]) | is.na(R[l[1],]))
      if(test) stop("Cannot parse lower and upper bounds in ", ll)
    }
    nREq <- sum(R[,1]!=0) + sum(is.finite(R[,2])) # number of range equations
    rm(L, n, NB, l, ll)
  }


  ### Set up
  nCon <- length(eqs)
  const <- setNames(rep("", nCon), paste0("goal", 1:nCon))
  if(nREq>0) n <- c(names(const), paste0("bound", 1:nREq)) else n <- names(const)
  m <- setNames(rep("", nCon+nREq), n)
  b <- setNames(rep( 0, nCon+nREq), n)
  w <- matrix(1, nrow=nCon+nREq, ncol=2, dimnames=list(n, c("w-", "w+")))
  p <- matrix(1, nrow=nCon+nREq, ncol=2, dimnames=list(n, c("p-", "p+")))
  E <- list()
  L <- lapply(strsplit(eqs, "[:|]"), trimws)
  if(length(L)!=nCon) stop("Cannot parse text input.")


  ### Parse each line of text
  for(i in 1:length(L)){ # Loop over lines of text
    eqOrig <- paste0(L[[i]], collapse=" | ")
    wSet   <- FALSE
    pSet   <- FALSE
    for(j in 1:length(L[[i]])){ # Loop over elements in each line
      # Figure out what this element is
      isName <- j==1 && !grepl("[=<>#]", L[[i]][j])
      isCons <- j<=2 &&  grepl("[=<>]", L[[i]][j]) && !grepl("#", L[[i]][j])
      #isWeig <- j>=2 && !grepl("[=<>[:alpha:]#]", L[[i]][j])
      isWeig <- j>=2 && !grepl("[=<>#]", L[[i]][j])
      isPrio <- j>=2 &&  grepl("#", L[[i]][j]) && !grepl("[=<>]", L[[i]][j])
      test   <- (isName + isCons + isWeig + isPrio)==1
      if(!test) stop("Cannot parse ", eqOrig)
      # Parse name
      if(isName){
        names(const)[i] <- L[[i]][j]
        rownames(w)[i]  <- L[[i]][j]
        rownames(p)[i]  <- L[[i]][j]
        names(b)[i]     <- L[[i]][j]
        names(m)[i]     <- L[[i]][j]
      }
      # Parse goal (except A matrix)
      if(isCons){
        const[i] <- L[[i]][j]
        e <- tryCatch(str2lang(const[i]),
                      error=function(e) stop("Cannot parse ", const[i], "."))
        # Must be a call with 3 elements
        test <- is.call(e) && length(e)==3
        # First element must be "<", ">", "<=", ">=", "=" or "=="
        test <- test && !is.null(e[[1]]) && is.symbol(e[[1]]) &&
          as.character(e[[1]]) %in% c("<", ">", "<=", ">=", "=", "==")
        # Right side must not contain any variable
        test <- test && !is.null(e[[3]]) && length(all.vars(e[[3]]))==0
        if(!test) stop("Expression ", const[i], " is not a valid goal.")
        # Store elements
        b[i]   <- eval(e[[3]], envir=baseenv())
        m[i]   <- as.character(e[[1]])
        E[[i]] <- e
        # Set default values for w and p. Deviations with w=NA will be dropped
        if(m[i]=="=" ){ w[i,] <- c( 1,  1); p[i,] <- c(   1,   1) }
        if(m[i]=="=="){ w[i,] <- c(NA, NA); p[i,] <- c(  NA,  NA) }
        if(m[i]==">="){ w[i,] <- c( 1,  0); p[i,] <- c(   1,+Inf) }
        if(m[i]=="<="){ w[i,] <- c( 0,  1); p[i,] <- c(+Inf,   1) }
      }
      # Parse weight
      if(isWeig){
        tmp <- trimws(strsplit(L[[i]][j], " ")[[1]]); tmp <- tmp[tmp!=""]
        n   <- length(tmp); tmpW <- rep(0, n)
        for(k in 1:n) tmpW[k] <- tryCatch(eval(str2lang(tmp[k])),
                                          error=function(e) stop(
                                            "Cannot parse weights ", L[[i]][j],
                                            " in line ", eqOrig))
        if(n==1){
          if(m[i]== "=") w[i,] <- tmpW
          if(m[i]=="==") w[i,] <- tmpW
          if(m[i]==">=") w[i,] <- c(tmpW, 0)
          if(m[i]=="<=") w[i,] <- c(0, tmpW)
        }
        if(n==2) w[i,] <- tmpW
        test <- n>2 || (n==2 && m[i] %in% c(">=", "<=")) || (n>0 && m[i]=="==")
        if(test) stop("Too many weights in ", eqOrig)
        wSet <- TRUE
        rm(tmp, n, tmpW, k)
      }
      # Parse priority
      if(isPrio){
        tmp <- trimws(strsplit(L[[i]][j], " ")[[1]]); tmp <- tmp[tmp!=""]
        tmp <- sub("#", "", tmp); n <- length(tmp); tmpP <- rep(0, n)
        for(k in 1:n) tmpP[k] <- tryCatch(eval(str2lang(tmp[k])),
                                          error=function(e) stop(
                                            "Cannot parse priority ", L[[i]][j],
                                            " in line ", eqOrig))
        if(n==1){
          if(m[i]== "=") p[i,] <- tmpP
          if(m[i]=="==") p[i,] <- tmpP
          if(m[i]==">=") p[i,] <- c(tmpP, +Inf)
          if(m[i]=="<=") p[i,] <- c(+Inf, tmpP)
        }
        if(n==2) p[i,] <- tmpP
        test <- n>2 || (n==2 && m[i] %in% c(">=", "<=")) || (n>0 && m[i]=="==")
        if(test) stop("Too many lexicographic priorities in ", eqOrig)
        pSet <- TRUE
        rm(tmp, n, tmpP, k)
      }
    }# End of within-line loop
    # If w or p were set by the user to NA, and the other left to default,
    # set the other to NA
    if(anyNA(w[i,]) || anyNA(p[i,])) for(j in 1:2){
      if(wSet && is.na(w[i,j]) && !pSet) p[i,j] <- NA
      if(pSet && is.na(p[i,j]) && !wSet) w[i,j] <- NA
    }

  }# End of between-line loop
  rm(L, i, isCons, isName, isWeig, isPrio, j, test, eqOrig)


  ### Construct A
  # Initialise A
  vars <- unique(unlist(lapply(E, all.vars)))
  nVar <- length(vars)
  A    <- matrix(0, nrow=nCon+nREq, ncol=nVar,
                 dimnames=list(rownames(w), vars))
  test <- nREq==0 || all(rownames(R) %in% vars)
  if(!test){
    txt <- paste0(rownames(R)[!(rownames(R) %in% vars)], collapse='", "')
    stop('Ranges have being defined for "', txt, '" but these variables ',
         "do not show up in the goals. Ranges are only allowed for variables ",
         "appearing in the goals.")
  }
  # Check linearity of goals
  tEnv1 <- setNames(2 + 10*round(runif(nVar),4), vars)
  tEnv2 <- 2*tEnv1
  tEnv1 <- list2env(as.list(tEnv1))
  tEnv2 <- list2env(as.list(tEnv2))
  for(i in 1:nCon){
    test1 <- eval(E[[i]][[2]], envir=tEnv1)
    test2 <- eval(E[[i]][[2]], envir=tEnv2)
    if(abs(2*test1 - test2)>1e-10) stop("Goal ", const[i],
                                        " is not linear.")
  }; rm(tEnv1, tEnv2, i, test1, test2)
  # Fill in goals
  envi <- list2env(setNames(split(diag(nVar), rep(1:nVar,each=nVar)), vars))
  for(i in 1:nCon) A[i,] <- eval(E[[i]][[2]], envir=envi)
  # Fill in ranges
  i <- nCon + 1
  if(nREq>0) for(k in 1:nrow(R)){
    j <- vars[vars==rownames(R)[k]]
    if(R[k,1]!=0){
      A[i,j] <- 1; w[i,] <- c(NA, 0);  p[i,] <- c(NA, Inf); b[i] <- R[k,1]
      m[i] <- "lBound"; names(b)[i] <- names(m)[i] <- paste0(j, "_lBound")
      rownames(A)[i] <- rownames(w)[i] <- rownames(p)[i] <- paste0(j, "_lBound")
      i <- i + 1
    }
    if(is.finite(R[k,2])){
      A[i,j] <- 1; w[i,] <- c( 0,NA);  p[i,] <- c(Inf, NA); b[i] <- R[k,2]
      m[i] <- "uBound"; names(b)[i] <- names(m)[i] <- paste0(j, "_uBound")
      rownames(A)[i] <- rownames(w)[i] <- rownames(p)[i] <- paste0(j, "_uBound")
      i <- i + 1
    }
  }


  ### Return
  return(list(A=A, b=b, m=m, w=w, p=p))
}
