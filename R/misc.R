#' covariate function builder
#'
#' @param ... parameters to pass to cov
#'
#' @export
#' @return Covariate function
f_cov <- function (...) {
  substitute( with(cov, { ... } ) )
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

as.num <- function(x) {
  as.numeric(as.character(x))
}

cleanup_code <- function(code) {
  if(!is.null(code)) {
    code <- gsub("\\r\\n", "\n", code)
    code <- gsub("\\n", ";\n", code)
    code <- gsub("$", ";\n", code)
    code <- gsub("^;", "", code)
  }
  return(code)
}

#' Find string and replace in file
#'
#' @param files vector of files
#' @param find find what string, vector of character
#' @param replacement replace with what, vector of character, should be equal in length to `find`
#' @export
#' @return Function does not return a value but edits files on disk
search_replace_in_file <- function(files = c(), find = NULL, replacement = NULL) {
  for(file in files) {
    x <- readLines(file)
    if(length(find) != length(replacement)) {
      stop("Search string and replacement are not equal length.")
    }
    for(i in 1:length(find)) {
      x <- gsub(find[i], replacement[i], x)
    }
    cat(x, file = file, sep="\n")
  }
}


#' Put vector values in quotes
#'
#' @param x vector of string / numeric
#' @param quote what type of quotes (`double` or `single`)
#' @export
#' @return Character vector of input with quotation marks around each value
add_quotes <- function(x, quote = "double") {
  q <- '"'
  if(quote == "single") {
    q <- "'"
  }
  for(i in 1:length(x)) {
    x[i] <- paste0(q, x[i], q)
  }
  return(x)
}

#' Return a list in R syntax
#'
#' @param x list to be printed
#' @param wrapper wrap in list object?
#' @export
#' @return Original list in R syntax
print_list <- function(x, wrapper = TRUE) {
  UseMethod("print_list")
}

#' @export
print_list.NULL <- function(x, wrapper = TRUE) {
  return("")
}

#' @export
print_list.list <- function(x, wrapper = TRUE) {
  if(length(x) == 0) return("")
  result <- paste0(utils::capture.output(dput(x)), collapse = "")
  if(isTRUE(wrapper)) {
    return(result)
  } else {
    result <- gsub("^list\\(", "", result)
    result <- gsub("\\)$", "", result)
    result
  }
}

#' Current time in UTC
#'
#' @return POSIXct object containing current time in UTC
#' @keywords internal
now_utc <- function() {
  as.POSIXct(as.POSIXlt(Sys.time(), tz = "UTC"))
}
