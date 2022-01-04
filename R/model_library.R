#' Model library
#' @param name name of model in library. If none specified, will show list of available models.
#' @export
#' @return List containing information about the named model
model_library <- function(name = NULL) {
  lib <- list(
    "pk_1cmt_iv" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1]
      ",
      obs=list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 1
    ),
    "pk_1cmt_iv_auc" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1]
        dAdt[2] = A[1]/V
      ",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 2
    ),
    "pk_1cmt_iv_mm" = list(
      code = "
        dAdt[1] = -(VMAX*(A[1]/V)) / (KM+A[1]/V)
      ",
      obs=list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 1
    ),
    "pk_2cmt_iv" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2]
        dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1]
      ",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 2
    ),
    "pk_2cmt_iv_auc" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2]
        dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1]
        dAdt[3] = A[1]/V
      ",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 3
    ),
    "pk_3cmt_iv" = list(
      code = "
        dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2] - (Q2/V)*A[1] + (Q2/V3)*A[3]
        dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1]
        dAdt[3] = -(Q2/V3)*A[3] + (Q2/V)*A[1]
      ",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1),
      size = 3
    ),
    "pk_1cmt_oral" = list(
      code = "
        dAdt[1] = -KA*A[1]
        dAdt[2] = KA*A[1] - (CL/V)*A[2]
      ",
      obs = list(cmt = 2, scale = "V"),
      dose = list(cmt = 1),
      size = 2
    ),
    "pk_2cmt_oral" = list(
      code = "
        dAdt[1] = -KA*A[1]
        dAdt[2] = KA*A[1] - (CL/V)*A[2] + (Q/V2)*A[3] - (Q/V)*A[2]
        dAdt[3] = -(Q/V2)*A[3] + (Q/V)*A[2]
      ",
      obs = list(cmt = 2, scale = "V"),
      dose = list(cmt = 1),
      size = 3
    ),
    "pk_3cmt_oral" = list(
      code = "
        dAdt[1] = -KA*A[1]
        dAdt[2] = KA*A[1] - (CL/V)*A[2] + (Q/V2)*A[3] - (Q/V)*A[2] + (Q2/V3)*A[4] - (Q2/V)*A[2]
        dAdt[3] = -(Q/V2)*A[3] + (Q/V)*A[2]
        dAdt[4] = -(Q2/V3)*A[4] + (Q2/V)*A[2]
      ",
      obs = list(cmt = 2, scale = "V"),
      dose = list(cmt = 1),
      size = 4
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
