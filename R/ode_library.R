# #' @export
# ode_library <- list(
#   "pk_1cmt_iv" = new_ode_model(
#     code = "
#        dAdt[1] = -(CL/V)*A[1] + rate ;
#      ",
#     parameters = list("CL" = 1, "V" = 1),
#     size = 1,
#     obs = list (cmt = 1, scale = "V"),
#     dose = list(cmt = 1)
#   )
# )
#
