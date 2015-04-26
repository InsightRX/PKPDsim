#' @export
get_parameters_from_code <- function (code) {
  code <- gsub("\\n", "", code)
  code <- gsub("[\\;\\/\\*\\^\\+\\=\\(\\)\\-]", " ", code)
  A <- gregexpr("A\\[([0-9])\\]", code)
  m1 <- paste0(unlist(regmatches(code, A, invert = TRUE)), collapse="")
  dAdt <- gregexpr("dAdt\\[([0-9])\\]", m1)
  m2 <- paste0(unlist(regmatches(m1, dAdt, invert = TRUE)), collapse="")
  spl <- strsplit(m2, " ")[[1]]
  spl <- spl[spl!=""]
  pars <- c(unique(spl))
  return(pars)
}

