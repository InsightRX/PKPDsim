#' R starts counting vector indices at 1, c++ starts at 0,
#' so reduce all state numbers in the Cpp function definition by 1
#'
#' @param ode_def ODE definition
#' @param n add/substract what number, default = -1
#'
shift_state_indices <- function(ode_def, n = -1) {

  shift <- function(ode_def, n = -1, regxp = "A") {
    ode_def_orig <- ode_def
    m <- gregexpr(paste0(regxp, "\\[([0-9]*)\\]"), ode_def, perl=TRUE)
    if(length(m) > 0 && m[[1]][1] > 0) {
      start <- unlist(attr(m[[1]], "capture.start"))[,1]
      len <- attr(m[[1]], "capture.length")
      txt <- unlist(regmatches(ode_def, m))
      txt <- as.num(gsub(paste0("[",regxp,"\\[\\]]"), "", txt, perl=TRUE)) + n
      if(any(txt < 0)) return(ode_def_orig)

      for(i in 1:length(start)) {
        if(length(start[i]) > 0) {
          ode_def <- paste0(substr(ode_def, 1, start[i]-1),
                            txt[i],
                            substr(ode_def, start[i]+len[i], 1e6)
          )
        }
      }
    }
    return(ode_def)
  }

  ode_def <- shift(ode_def, -1, "A")
  ode_def <- shift(ode_def, -1, "dAdt")

  return(ode_def)
}
