#' Get model parameters from code
#' @param code code
#' @param state_init state init vector
#' @param declare_variables declared variables
#' @export
#' @return Vector of parameter names
get_parameters_from_code <- function (code, state_init, declare_variables = NULL) {
  ## find newly defined parameters in code
  if(!is.null(state_init)) {
    code <- paste0(code, state_init)
  }
  code <- gsub("\\n", " ", code)
  code <- gsub("double (.*?)=", "", code) # parameter declarations in code
  code <- gsub("[\\;\\/\\*\\^\\+\\=\\(\\)\\-\\{\\}\\>\\<\\,]", " ", code)
  code <- gsub("\\-", " ", code)
  code <- gsub("(if|then|else|pow|sqrt|exp|log|cout)", " ", code)
  A <- gregexpr("A\\[([0-9]*)\\]", code)
  m1 <- paste0(unlist(regmatches(code, A, invert = TRUE)), collapse="")
  dAdt <- gregexpr("dAdt\\[([0-9]*)\\]", m1)
  m2 <- paste0(unlist(regmatches(m1, dAdt, invert = TRUE)), collapse="")
  spl <- strsplit(m2, " ")[[1]]
  spl <- spl[spl!=""]
  spl2 <- c()
  for (i in seq(spl)) {
    if (gsub("[0-9\\.]", "", spl[i]) != "" &! spl[i] %in% c("conc", "rate", declare_variables)) {
      spl2 <- c(spl2, spl[i])
    }
  }
  pars <- c(unique(spl2))
  reserved_words <- c("beta", "gamma") # add more later
  if (any(reserved_words %in% pars)) {
    stop("The following variable names are reserved words and cannot be used in your code, please rename:\n   ", paste(pars[match(reserved_words, pars)], collapse=" "))
  } else {
    defined <- c(declare_variables, "t", "t_prv_dose", "prv_dose", "times[i]", "dose_cmt[i]", "bioav[i]", "doses[i]")
    match_def <- match(defined, pars)
    if (length(match_def[!is.na(match_def)]) > 0) {
      pars <- pars[-match_def[!is.na(match_def)]]
    }
    ## remove partial matches too
    partial <- c("cov_")
    for(i in seq(partial)) {
      idx <- grep(partial[i], pars)
      if(length(idx) > 0) pars <- pars[-idx]
    }
    return(pars)
  }
}
