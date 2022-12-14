% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goalp.R
\name{goalp}
\alias{goalp}
\title{Solves a (linear) goal programming problem}
\usage{
goalp(
  eqs,
  A = NULL,
  m = NULL,
  b = NULL,
  w = NULL,
  p = NULL,
  varType = NULL,
  normW = FALSE,
  silent = FALSE
)
}
\arguments{
\item{eqs}{Character vector describing a set of linear equations. The vector
can either contain a single element with one equation per line,
or multiple elements, each with a single equation. Equations must
be valid R expressions (see details).}

\item{A}{Numeric matrix with the coefficients of the variables. One row per
equation, one column per variable. Columns can be named according
to the variables they correspond to. Rows can be named for their
corresponding goals. Ignored if argument \code{eqs} is
provided.}

\item{m}{Character vector with the relationship between the left and
right-hand side of the goals. It can be any of
\code{=, ==, <=, >=}. \code{=} allows for positive (excess) and
negative (lack) deviations. \code{==} do not allow any deviation,
enforcing fulfillment of the goal. \code{<=} automatically assigns
a weight equal to zero to the negative (lack) deviation. \code{>=}
automatically assigns a weight equal to zero to the positive
(excess) deviation.}

\item{b}{Numeric vector with the values on the right hand side of the
goals. Ignored if argument \code{eqs} is provided.}

\item{w}{Numeric matrix with the weights associated to the deviations from
each goal. It should have as many rows as goals, and
two columns: the first column corresponding to the weight of the
positive deviation (excess), and the second column corresponding
to the weight of the negative deviation (lack).
This argument is ignored if \code{eqs} is provided.
If omitted and \code{eqs} is not provided either, default weights
are dependent on the type of goal, as follows.
\itemize{
  \item \code{=}: Positive and negative deviations are assigned
                  equal weights of 1.
  \item \code{==}: Positive and negative deviations are assigned
                  equal weights of NA, as these deviations will be
                  excluded from the problem, i.e. the goal
                  will be enforced.
  \item \code{>=}: Positive deviation is assigned a weight of
                   0, so it does not influence the objective
                   function (and therefore the solution to the
                   problem). The negative deviation is assigned
                   a weight of 1, but if manually set to NA,
                   then the inequality is enforced.
  \item \code{<=}: Negative deviation is assigned a weight of
                   0, so it does not influence the objective
                   function (and therefore the solution to the
                   problem). The positive deviation is assigned
                   a weight of 1, but if manually set to NA,
                   then the inequality is enforced.
}}

\item{p}{Numeric matrix indicating the priority of each deviation under
a lexicographic approach. Lower numbers represent higher
priority (e.g. from lower to higher priority: 1, 2, 3, ...).
It must have as many rows as goals, and two columns.
This argument is ignored if \code{eqs} is provided.
If omitted and not provided in \code{eqs} either, default
priorities are dependent on the type of goal, as follows.
\itemize{
  \item \code{=}: Positive and negative deviations are assigned
                  equal priority of 1.
  \item \code{==}: Positive and negative deviations are assigned
                   equal priority of NA, as these deviations will
                   be excluded from the problem, i.e. the
                   goal will be enforced.
  \item \code{>=}: Positive deviation is assigned a priority of
                   +Inf, making it irrelevant. The negative
                   deviation is assigned a priority of 1.
  \item \code{<=}: Negative deviation is assigned a priority of
                   +Inf, making it irrelevant. The positive
                   deviation is assigned a priority of 1.
}}

\item{varType}{Named character vector. Defines the type of each variable.
It can be defined as \code{c(x1="int", x2="cont")}. Omitted
variables are assumed to be integer. Each element can be
either \code{"continuous"} (i.e. non-negative real values),
\code{"integer"} (i.e. non-negative natural values), or
\code{"binary"} (i.e. only take values 0 or 1). Using only
the first letters is accepted too. If omitted, all variables
are assumed to be integer.}

\item{normW}{Logical. TRUE to scale the weights by the inverse of the
corresponding right-hand size value of the goal (\code{b}).
Useful to balance the relevance of all goals.
Equivalent to normalising the problem so \code{b=1} for all
goals.}

\item{silent}{Logical. TRUE to prevent the function writing anything
to the console (or the default output). Default is FALSE.}
}
\value{
goalp object. It contains the following elements.
        \itemize{
          \item \code{A}: The coefficient matrix of the decision variables.
                          It does not include the coefficients of the
                          deviations.
          \item \code{m}: The relationship between the left- and right-hand
                          side of the goals.
          \item \code{b}: The right-hand side of the goals.
          \item \code{w}: The weights of the deviation variables.
          \item \code{p}: The lexicographic priorities of deviations
                          variables.
          \item \code{A_}: The coefficient matrix of the decision and
                           deviation variables.
          \item \code{w_}: The weights of the decision and deviation
                           variables.
          \item \code{eqs}: Text version of the goal programming problem.
          \item \code{varType}: Vector describing the type of the decision
                                variables (binary, integer, or continuous).
          \item \code{x}: Optimal value of the decision variables.
          \item \code{d}: Optimal value of the deviation variables.
          \item \code{obj}: The value of the objective function (sum of
                            weighted deviations). If using lexicographic
                            priorities, the value for the objective
                            function using all deviations (i.e. ignoring the
                            priority) in each stage.
          \item \code{X}: The value of the decision variables for the
                          optimal solution in each stage of the
                          lexicographic problem. If there are no
                          lexicographic priorities, then a single row matrix.
          \item \code{lp}: lp object describing the solution of the
                           underlying linear programming problem. See
                           \link[lpSolve]{lp.object}. When using
                           lexicographic priorities, the solution to the
                           last stage.
          \item \code{solutionFound}: Logical taking value TRUE if a
                                      solution was found, FALSE otherwise.
        }
}
\description{
Given a set of equations representing goals of a linear goal
programming problem, it finds the optimal solution.
}
\details{
The actual solution of the linear programming problem is found using lp_solve
\url{https://lpsolve.sourceforge.net/}, through its R interface (the lpSolve
package).

Argument 'eqs' defines the goals of the goal programming problem
through easy human-readable text. When writing a constranit, all variables
must be on the left-hand side, with only numeric values on the right-hand
side. Equations must be valid R expressions. Examples of valid equations
are the following:
\itemize{
  \item \code{"3*x + 2*y = 16"}
  \item \code{"4*x -   y =  3"}
}
On the other hand, the following are not valid expressions:
\itemize{
  \item \code{"3*x = 16 - 2*y"}
  \item \code{"4x + 1y = 5"}
}

While optional, it is highly encouraged to provide names for each goal.
The user can also provide weights and/or lexicographic priorities for the
positive (excess) and negative (lack) deviations associated to each
goal. The following example shows how to provide this information:
\code{"
Labour   : 20*A + 12*B + 40*C = 1200 | 0.2 0.1 | 1# 2#
Profit   : 11*A + 16*B +  8*C = 1000 | 0.1 0.3 | 3# 4#
Batteries:  4*A +  3*B +  6*C =  200 | 0.2 0.1 | 5# 6#"}
The name of the goal must be followed by a colon (\code{:}) or split
vertical bars (\code{|}). Then the goal. Then the weights associated
to the positive deviation first (excess), and the negative deviation (lack)
last, separated by an empty space. Finally, the lexicographic priorities
for the positive (excess) and negative (lack) deviations can be provided
as numbers, each followed by a hashtag, and separated by an space, in
that order. Lower values imply a higher priority, and the same priority
can be assigned to multiple deviations. Only the equation is mandatory.
If the weights are omitted, all of them are assumed to be equal to one.
If the lexicographic priorities are omitted, all of them are assumed to
be equal to one.
}
