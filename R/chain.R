#' Pipe/chain an object forward into a function call/expression.
#'
#' @param lhs a value
#' @param rhs a function/call/expression. Enclose in parentheses to force
#' evaluation of rhs before piping is carried out, e.g. anonymous functions
#' or call-generating expressions.
#' @return The result of evaluting the right-hand side with the left-hand side
#' as the specified argument(s).
#' @rdname chain

`%>%` <- magrittr::`%>%`
