#' Model library
#' @param name name of model in library. If none specified, will show list of available models.
#' @export
model_library <- function(name = NULL) {
  lib <- list(
    "pk_1cmt_iv" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1] + rate;
      ",
      obs=list(cmt = 1, scale = "V"),
      dose = list(cmt = 1)
    ),
    "pk_1cmt_iv_mm" = list(
      code = "
        dAdt[1] = -(VMAX*(A[1]/V)) / (KM+A[1]/V)  + rate;
      ",
      obs=list(cmt = 1, scale = "V"),
      dose = list(cmt = 1)
    ),
    "pk_2cmt_iv" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2] + rate;
        dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1];
      ",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 2
    ),
    "pk_1cmt_oral" = list(
      code = "
        dAdt[1] = -KA*A[1];
        dAdt[2] = KA*A[1] - (CL/V)*A[2];
      ",
      obs=list(cmt = 2, scale = "V"),
      dose = list(cmt = 1)
    )
  )
  if (is.null(name)) {
    return(paste(names(lib), collapse="\n  "))
  }
  if (!name %in% names(lib)) {
    message(paste0("Model ", name, " not found in PKPDsim library. Available models are: \n  ", paste(names(lib), collapse="\n  ")))
  } else {
    return(lib[[name]])
  }
}
