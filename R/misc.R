#' covariate function builder
#'
#' @export
f_cov <- function (...) {
  substitute( with(cov, { ... } ) )
}

min_decimal_places <- function(x) {
  max(vapply(x, function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }, sum()))
}


cumhaz_to_surv <- function(cumhaz) {
  exp(-cumhaz)
}

get_size_ode <- function(ode, p) {
  p$dose_times <- c(0)
  p$dose_amts  <- c(0)
  p$rate <- 1
  dum <- ode(1, rep(1, 1000), p)
  length(dum[[1]])
}

triangle_to_full <- function (vect) {
  lower_triangle_mat_size <- function (mat) {
    x <- length(mat)
    i <- 1
    while (x > 0) {
      x <- x-i
      i <- i+1
    }
    return(i-1)
  }
  nr <- lower_triangle_mat_size (vect)
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nr ) ] , nr = nr ))
}
