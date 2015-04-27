#' @export
get_parameters_from_code <- function (code, declare_variables = NULL) {
  code <- gsub("\\n", "", code)
  code <- gsub("[\\;\\/\\*\\^\\+\\=\\(\\)\\-]", " ", code)
  A <- gregexpr("A\\[([0-9])\\]", code)
  m1 <- paste0(unlist(regmatches(code, A, invert = TRUE)), collapse="")
  dAdt <- gregexpr("dAdt\\[([0-9])\\]", m1)
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
  return(pars)
}

