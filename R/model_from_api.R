#' Load model definition from API, and compile to R library
#'
#' @param model model id
#' @param url URL of API
#' @param github not a custom API but GitHub
#' @param repo GH repository specified as `owner/repo`
#' @param auth_token for private repositories
#' @param verbose verbosity (T/F)
#' @param definition_only return only the model definition, do not compile
#' @param force force install, even if model inactive
#' @param run_tests run tests associated with model?
#' @export
model_from_api <- function(model = NULL,
                           url = "http://localhost:8080/api",
                           github = FALSE,
                           repo = NULL,
                           auth_token = NULL,
                           verbose = TRUE,
                           get_definition = FALSE,
                           to_package = FALSE,
                           force = FALSE,
                           run_tests = FALSE,
                           ...) {
  if(is.null(model)) {
    if(github) {
      mods <- github_download(repo = "InsightRX/models", action = "ls",
                              path = "models",
                              auth_token = auth_token)
      defs <- mods$name %>% stringr::str_replace_all(".json[5].?", "")
    } else {
      defs <- jsonlite::fromJSON(paste0(url, "/models"))
    }
    message("No `model` specified, returning available PKPDsim models.")
    return(defs)
  }
  if(verbose) {
    message("Connecting to API...")
  }
  if(github) {
    if(is.null(repo)) stop("Please specify GitHub repository!")
    def <- github_download(repo = repo, path = paste0("models/", model, ".json5"),
                                 auth_token = auth_token, raw = FALSE)
    if(run_tests) {
      test_txt <- github_download(repo = repo, path = paste0("tests/", model, ".R"),
                                  auth_token = auth_token, raw = TRUE)
      tmp_file <- tempfile()
      fileConn <- file(tmp_file)
      writeLines(test_txt, fileConn)
      close(fileConn)
    }
  } else {
    postfix <- ".json5"
    if(length(grep("http", url)) > 0) { # if API and not local, don't add json5 extension
      postfix <- ""
    }
    lines <- paste(readLines(paste0(url, "/models/", model, postfix)), collapse="\n") %>%
      stringr::str_replace_all("'", "\"") %>%
      stringr::str_replace_all("\\\\n", "\n") %>%
      stringr::str_replace_all("\n", "") %>%
      stringr::str_replace_all("\\\\", "\\\\n")
    def <- jsonlite::fromJSON(lines)
    if(run_tests) {
      test_txt <- readLines(paste0(url, "/tests/", model, ".R"))
      tmp_file <- tempfile()
      fileConn <- file(tmp_file)
      writeLines(test_txt, fileConn)
      close(fileConn)
    }
  }
  if(verbose) {
    message("Retrieving model definition (", model, ")...")
  }
  if(is.null(def) || is.null(def$ode_code)) {
    stop("Returned model definition is empty, please make sure the specified model id is correct.")
  }
  if(get_definition) {
    return(def)
  }
  if(!def$build && !force) {
    message("Model not flagged for building, skipping compilation. Use `force`=TRUE to force build.")
  }
  if(def$build || force) {
    if(verbose) {
      message("Compiling model.")
    }
    package <- NULL
    if(to_package) {
      package <- gsub("_", "", def$id)
    }
    mod <- PKPDsim::new_ode_model(code = def$ode_code,
                                  pk_code = def$pk_code,
                                  dose_code = def$dose_code,
                                  obs = def$obs,
                                  dose = def$dose,
                                  size = def$n_comp,
                                  parameters = def$parameters,
                                  declare_variables = def$variables,
                                  covariates = def$covariates,
                                  as_is = TRUE,
                                  package = package,
                                  iiv = def$iiv,
                                  omega_matrix = def$omega_matrix,
                                  ruv = def$ruv,
                                  default_parameters = def$default_parameters,
                                  ...)
    if(run_tests) {
      if(verbose) message("Running test(s)...")
      system(paste0("Rscript ", tmp_file))
    }
  }
  if(is.null(mod)) {
    message("Done.")
  } else {
    return(mod)
  }
}
