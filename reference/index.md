# Package index

## All functions

- [`add_quotes()`](https://insightrx.github.io/PKPDsim/reference/add_quotes.md)
  : Put vector values in quotes

- [`add_ruv()`](https://insightrx.github.io/PKPDsim/reference/add_ruv.md)
  : Add residual variability to the dependent variable

- [`add_ruv_to_quantile()`](https://insightrx.github.io/PKPDsim/reference/add_ruv_to_quantile.md)
  : Calculate the increase in a specific quantile for a distribution on
  y when residual variability is added

- [`adherence_binomial()`](https://insightrx.github.io/PKPDsim/reference/adherence_binomial.md)
  : Binomial adherence

- [`adherence_markov()`](https://insightrx.github.io/PKPDsim/reference/adherence_markov.md)
  : Markov adherence model

- [`advan()`](https://insightrx.github.io/PKPDsim/reference/advan.md) :
  ADVAN-style functions to calculate linear PK systems

- [`advan_create_data()`](https://insightrx.github.io/PKPDsim/reference/advan_create_data.md)
  : Create ADVAN-style dataset

- [`advan_parse_output()`](https://insightrx.github.io/PKPDsim/reference/advan_parse_output.md)
  : Internal function to parse the raw output from ADVAN-style functions

- [`advan_process_infusion_doses()`](https://insightrx.github.io/PKPDsim/reference/advan_process_infusion_doses.md)
  : Add column RATEALL to ADVAN-style dataset to handle infusions

- [`apply_duration_scale()`](https://insightrx.github.io/PKPDsim/reference/apply_duration_scale.md)
  : Apply infusion duration scale to a regimen

- [`available_default_literature_models()`](https://insightrx.github.io/PKPDsim/reference/available_default_literature_models.md)
  : See models from the literature available for installation

- [`calc_auc_analytic()`](https://insightrx.github.io/PKPDsim/reference/calc_auc_analytic.md)
  : Convenience function to calculate the AUC based on PK model
  parameters at any given moment, for linear iv models.

- [`calc_dydP()`](https://insightrx.github.io/PKPDsim/reference/calc_dydP.md)
  : Calculate derivative

- [`calc_ss_analytic()`](https://insightrx.github.io/PKPDsim/reference/calc_ss_analytic.md)
  : Returns the state of a linear PK system at steady state (trough)
  using analytics equations (so for linear PK systems only).

- [`calculate_parameters()`](https://insightrx.github.io/PKPDsim/reference/calculate_parameters.md)
  : Calculate model-specific variables using a dummy call to sim_ode()

- [`check_obs_input()`](https://insightrx.github.io/PKPDsim/reference/check_obs_input.md)
  : Checks obs input for valid combinations of cmt, var, scale

- [`compile_sim_cpp()`](https://insightrx.github.io/PKPDsim/reference/compile_sim_cpp.md)
  : Compile ODE model to c++ function

- [`covariate_last_obs_only()`](https://insightrx.github.io/PKPDsim/reference/covariate_last_obs_only.md)
  : Use only last observed covariate values

- [`covariates_table_to_list()`](https://insightrx.github.io/PKPDsim/reference/covariates_table_to_list.md)
  : Convert covariate table specified as data.frame

- [`cv_to_omega()`](https://insightrx.github.io/PKPDsim/reference/cv_to_omega.md)
  : Create lower-diagonal omega matrix from CV for parameter estimates

- [`detect_ode_syntax()`](https://insightrx.github.io/PKPDsim/reference/detect_ode_syntax.md)
  : Auto-detect the syntax for the ODE code

- [`f_cov()`](https://insightrx.github.io/PKPDsim/reference/f_cov.md) :
  covariate function builder

- [`get_fixed_parameters()`](https://insightrx.github.io/PKPDsim/reference/get_fixed_parameters.md)
  : Get fixed parameters from model definition.

- [`get_model_parameters()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_covariates()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_fixed_parameters()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_structure()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_linearity()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_auc_compartment()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  [`get_model_iov()`](https://insightrx.github.io/PKPDsim/reference/get_model_info.md)
  : Functions for getting information about a model

- [`get_ode_model_size()`](https://insightrx.github.io/PKPDsim/reference/get_ode_model_size.md)
  : Get the number of states in the ODE from the code code C++ code for
  model

- [`get_parameters_from_code()`](https://insightrx.github.io/PKPDsim/reference/get_parameters_from_code.md)
  : Get model parameters from code

- [`get_var_y()`](https://insightrx.github.io/PKPDsim/reference/get_var_y.md)
  : Get expected variance/sd/ci of dependent variable based on PKPDsim
  model, parameters, and regimen

- [`ifelse0()`](https://insightrx.github.io/PKPDsim/reference/ifelse0.md)
  : ifelse function but then based on whether value is NULL or not

- [`install_default_literature_model()`](https://insightrx.github.io/PKPDsim/reference/install_default_literature_model.md)
  : Install default literature model

- [`is_positive_definite()`](https://insightrx.github.io/PKPDsim/reference/is_positive_definite.md)
  : Is matrix positive definite

- [`join_cov_and_par()`](https://insightrx.github.io/PKPDsim/reference/join_cov_and_par.md)
  : Combines covariates and parameters into a single list, useful for
  reparametrization of the model.

- [`join_regimen()`](https://insightrx.github.io/PKPDsim/reference/join_regimen.md)
  : Join two dosing regimens

- [`lower_triangle_mat_size()`](https://insightrx.github.io/PKPDsim/reference/lower_triangle_mat_size.md)
  : Size of the lower triangle of the matrix

- [`merge_regimen()`](https://insightrx.github.io/PKPDsim/reference/merge_regimen.md)
  : Merge two regimens together.

- [`model_from_api()`](https://insightrx.github.io/PKPDsim/reference/model_from_api.md)
  : Load model definition from API, and compile to R library

- [`model_library()`](https://insightrx.github.io/PKPDsim/reference/model_library.md)
  : Model library

- [`mvrnorm2()`](https://insightrx.github.io/PKPDsim/reference/mvrnorm2.md)
  : More powerful multivariate normal sampling function

- [`na_locf()`](https://insightrx.github.io/PKPDsim/reference/na_locf.md)
  : Fill in NAs with the previous non-missing value

- [`new_adherence()`](https://insightrx.github.io/PKPDsim/reference/new_adherence.md)
  : Probabilistically model adherence

- [`new_covariate()`](https://insightrx.github.io/PKPDsim/reference/new_covariate.md)
  : New covariate

- [`new_covariate_model()`](https://insightrx.github.io/PKPDsim/reference/new_covariate_model.md)
  : covariate model function

- [`new_ode_model()`](https://insightrx.github.io/PKPDsim/reference/new_ode_model.md)
  : Create new ODE model

- [`new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)
  : Dose regimen for sim_ode

- [`nlmixr_parse_parameters()`](https://insightrx.github.io/PKPDsim/reference/nlmixr_parse_parameters.md)
  : Function to parse parameters for a model into a structure used by
  nlmixr

- [`nm_to_regimen()`](https://insightrx.github.io/PKPDsim/reference/nm_to_regimen.md)
  : Create a regimen from NONMEM data

- [`parse_lagtime()`](https://insightrx.github.io/PKPDsim/reference/parse_lagtime.md)
  : Parse lagtime specified to main sim() function

- [`pkdata`](https://insightrx.github.io/PKPDsim/reference/pkdata.md) :
  PK dataset

- [`pkpdsim_to_nlmixr()`](https://insightrx.github.io/PKPDsim/reference/pkpdsim_to_nlmixr.md)
  : Convert a model generated with PKPDsim to an object for nlmixr

- [`pop_regimen()`](https://insightrx.github.io/PKPDsim/reference/pop_regimen.md)
  : Remove n doses (from tail) of PKPDsim regimen

- [`print_list()`](https://insightrx.github.io/PKPDsim/reference/print_list.md)
  : Return a list in R syntax

- [`read_model_json()`](https://insightrx.github.io/PKPDsim/reference/read_model_json.md)
  : Read model definition from JSON

- [`regimen_to_nm()`](https://insightrx.github.io/PKPDsim/reference/regimen_to_nm.md)
  : Convert PKPDsim regimen to NONMEM table (doses only)

- [`reparametrize()`](https://insightrx.github.io/PKPDsim/reference/reparametrize.md)
  : Reparametrize model parameters using a reparametrization defined
  within the model.

- [`search_replace_in_file()`](https://insightrx.github.io/PKPDsim/reference/search_replace_in_file.md)
  : Find string and replace in file

- [`shift_regimen()`](https://insightrx.github.io/PKPDsim/reference/shift_regimen.md)
  : Remove n doses (from start) of PKPDsim regimen

- [`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md) :
  Simulate ODE or analytical equation

- [`sim_core()`](https://insightrx.github.io/PKPDsim/reference/sim_core.md)
  : Only core function of the simulation function, always just returns
  observations. Mostly useful for estimations / optimal design. Has no
  checks (for speed)!

- [`sim_ode()`](https://insightrx.github.io/PKPDsim/reference/sim_ode.md)
  :

  Deprecated function, renamed to
  [`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md)

- [`sim_ode_shiny()`](https://insightrx.github.io/PKPDsim/reference/sim_ode_shiny.md)
  : Simulate ODE and create a Shiny app

- [`table_to_list()`](https://insightrx.github.io/PKPDsim/reference/table_to_list.md)
  : Convert a table to a list

- [`test_model()`](https://insightrx.github.io/PKPDsim/reference/test_model.md)
  : Test a model

- [`test_pointer()`](https://insightrx.github.io/PKPDsim/reference/test_pointer.md)
  : Test if model still in memory

- [`translate_ode()`](https://insightrx.github.io/PKPDsim/reference/translate_ode.md)
  : Translate a model from/to various PKPD simulators

- [`triangle_to_full()`](https://insightrx.github.io/PKPDsim/reference/triangle_to_full.md)
  : Convert triangle omega matrix to full omega matrix
