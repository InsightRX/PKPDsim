# #' @export
# ode_library <- list (
#   "pk_1cmt_iv" =
#     new_ode_model(
#       code = "dAdt[1] = -(CL/V)*A[1] + rate ;",
#       parameters = list("CL" = 5, "V" = 50),
#       obs = list (cmt = 1, scale = "V"),
#       dose = list (cmt = 1)
#   ),
#   "pk_1cmt_iv_mm" =
#     new_ode_model(
#       code = "dAdt[1] = -VMAX * (A[1]/V) / (A[1]/V + KM) + rate ;",
#       parameters = list("VMAX" = 1, "V" = 50, "KM" = 1),
#       obs = list (cmt = 1, scale = "V"),
#       dose = list(cmt = 1)
#     ),
#   "pk_1cmt_oral" =
#     new_ode_model(
#       code = "
#         dAdt[0] = -KA*A[1];
#         dAdt[1] = KA*A[1] - (CL/V)*A[2];
#       ",
#       parameters = list("KA" = 1, "CL" = 5, "V" = 50),
#       obs = list(cmt = 2, scale = "V"),
#       dose = list(cmt = 1)
#     )
# )
