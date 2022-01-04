#' Translate a model from/to various PKPD simulators
#'
#' Currently only supports PKDPsim <--> RxODE
#'
#' @param code character string with ODE code
#' @param auto is auto-detect syntax (`from`)
#' @param from from syntax
#' @param to to syntax
#' @param verbose verbose, `TRUE` or `FALSE`
#' @export
#' @return Translated PKPDsim or RxODE model
translate_ode <- function(
  code,
  auto = TRUE,
  from = NULL,
  to = NULL,
  verbose = TRUE) {
  supported <- c("PKPDsim", "RxODE")

  if("PKPDsim" %in% class(code)) {
    # not actually specified as code, but as compiled model.
    # we can extract the original code from the attributes though:
    code <- paste(attr(code, "pk_code"), attr(code, "code"), sep = '\n')
  }

  ## auto-detect conversion type
  syntax <- list()
  if(auto) {
    syntax <- detect_ode_syntax(code)
  }
  if(!is.null(from)) { # override auto
    syntax$from <- from
  }
  if(!is.null(to)) { # override auto
    syntax$to <- to
  }
  if(!(syntax$from %in% supported) || !(syntax$from %in% supported)) {
    stop(paste0("Sorry, requested translator not supported (only ", paste(supported, collapse=", "),")."))
  }
  if(syntax$from == syntax$to) return(code)

  if(verbose) message(paste0("Converting model code from ", syntax$from, " to ", syntax$to, " syntax."))

  separators <- "[\\s\\*\\^\\/\\+\\-\\;\\(\\)\\[\\]]"
  lines <- stringr::str_split(code, "\\n")[[1]]

  if(tolower(syntax$from) == "rxode" && tolower(syntax$to) == "pkpdsim") {

    ## replace compartment declaration syntax
    lines <- stringr::str_replace_all(lines, "d/dt\\((.+?)\\)", "dAdt\\[\\1\\]")

    ## find compartment declarations
    comps <- stringr::str_extract(lines, "dAdt\\[(.+?)\\]")
    comps <- stringr::str_replace_all(comps[!is.na(comps)], "(dAdt|\\[|\\])", "")

    ## update compartments in LH side
    for(i in seq(comps)) {
      lines <- stringr::str_replace_all(lines, paste0("dAdt\\[", comps[i], "\\]"), paste0("dAdt\\[", i, "\\]"))
    }

    ## update compartments in RH side
    for(i in seq(comps)) {
      lines <- stringr::str_replace_all(lines,
                                        paste0("(", separators, ")", comps[i], "(", separators,")"),
                                        paste0("\\1A\\[", i, "\\]\\2"))
    }

    ## if power specified using ^ in RxODE model, need to translate to pow() function
    ## since C++ only allows for that, Fortran allows for both. This cannot be done
    ## simply using regex, need to be parsed more properly. For now, let's just throw an error:
    if(any(stringr::str_detect(lines, "^"))) {
      stop("Sorry, use of the `base^exp` syntax for power functions is not supported by translator yet. Please specify RxODE model using `pow(base, exp)` instead.")
    }

  }

  if(tolower(syntax$from) == "pkpdsim" && tolower(syntax$to) == "rxode") {

    # replace compartment declaration syntax
    lines <- stringr::str_replace_all(lines, "dAdt\\[(.+?)\\]", "d/dt\\(\\1\\)")

    ## find compartment declarations
    comps <- stringr::str_extract(lines, "d/dt\\((.+?)\\)")
    comps <- stringr::str_replace_all(comps[!is.na(comps)], "(d/dt|\\(|\\))", "")

    ## update compartments in LH side
    for(i in seq(comps)) {
      lines <- stringr::str_replace_all(lines,
                                        paste0("d/dt\\(", comps[i], "\\)"),
                                        paste0("d/dt\\(A", comps[i], "\\)"))
    }

    ## update compartments in RH side
    for(i in seq(comps)) {
      lines <- stringr::str_replace_all(lines,
                                        paste0("(", separators, ")", "A\\[", comps[i], "\\]", "(", separators, "|$)"),
                                        paste0("\\1A", comps[i], "\\2"))
    }

    ## remove rate[x] definitions, if present
    lines <- stringr::str_replace(lines, "\\+(\\s*?)rate\\[.?+]", "")

    ## remove ";" if not useful
    lines <- stringr::str_replace(lines, "(\\s)+?\\;", "")

    ## remove empty lines
    lines <- lines[lines != ""]

  }

  out <- stringr::str_c(lines, collapse = '\n')

  return(out)
}
