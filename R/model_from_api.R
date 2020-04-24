#' Load model definition from API, and compile to R library
#'
#' @param model model id
#' @param url URL of API
#' @param github not a custom API but GitHub
#' @param repo GH repository specified as `owner/repo`
#' @param auth_token for private repositories
#' @param verbose verbosity (T/F)
#' @param get_definition return only the model definition, do not compile
#' @param to_package compile to package?
#' @param force force install, even if model inactive
#' @param run_tests run tests associated with model?
#' @param ... arguments passed to `new_ode_model()` function
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
      if(length(grep("http", url)) > 0) { ## if internet
        defs <- jsonlite::fromJSON(paste0(url, "/models"))
      } else {
        defs <- jsonlite::fromJSON(readLines(paste0(url, "/models")))
      }
    }
    message("No `model` specified, returning available PKPDsim models.")
    return(defs)
  }
  validation <- NULL
  custom_parameters <- NULL
  test_file <- NULL
  if(github) {
    if(verbose) {
      message("Connecting to API at GitHub...")
    }
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
      if(verbose) {
        message("Connecting to API...")
      }
    } else {
      if(verbose) {
        message("Reading from local repository...")
      }
    }
    lines <- paste(readLines(paste0(url, "/models/", model, postfix)), collapse="\n") %>%
      stringr::str_replace_all("'", "\"") %>%
      stringr::str_replace_all("\\\\n", "\n") %>%
      stringr::str_replace_all("\n", "") %>%
      stringr::str_replace_all("\\\\", "\\\\n")
    def <- jsonlite::fromJSON(lines)
    test_file <- paste0(url, "/tests/", model, ".R")
    if(run_tests) {
      if(file.exists(test_file)) {
        test_txt <- readLines(test_file)
        tmp_file <- tempfile()
        fileConn <- file(tmp_file)
        writeLines(test_txt, fileConn)
        close(fileConn)
      } else {
        if(def$build || force) {
          stop(paste0("Test file (", test_file,") not found!"))
        }
      }
    }
    if(file.exists(paste0(url, "/validation/", model, ".json"))) {
      validation <- paste0(url, "/validation/", model, ".json")
    }
    ## Parameter sets:
    if(file.exists(paste0(url, "/parameters/", model, ".json"))) {
      custom_parameters <- paste0(url, "/parameters/", model, ".json")
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
    message(paste0("Model ", model, " not flagged for building, skipping compilation. Use `force`=TRUE to force build."))
  }
  if(!is.null(def$misc$init_parameter) && !is.null(def$misc$model_type)) {
    ## Add a parameter and initialization code for setting the initial concentration based on a TDM value
    if(def$misc$init_parameter) {
      def$parameters$TDM_INIT <- 0
      if(!is.null(def$state_init)) {
        stop("Sorry, state init already specified. Cannot override for TDM-based initializiaton.")
      }
      if(! def$misc$model_type %in% c("1cmt_iv", "2cmt_iv")) {
        stop("Sorry, TDM initialization not supported for this model type yet.")
      } else {
        if(def$misc$model_type == "1cmt_iv") {
          def$state_init <- "\
              A[0] = TDM_INIT * Vi;\
          "
        }
        if(def$misc$model_type == "2cmt_iv") {
          def$state_init <- "\
              A[0] = TDM_INIT * Vi;\
              A[1] = (Qi/Vi)*(TDM_INIT * Vi) / (Qi/V2i);\
          "
        }
      }
    }
  }
  nonmem <- NULL
  if(!is.null(def$implementations$nonmem)) {
    nonmem <- paste(readLines(paste0(url, "/models/nonmem/", def$implementations$nonmem)), collapse="\n")
    # %>%
    #   stringr::str_replace_all("'", "\"") %>%
    #   stringr::str_replace_all("\\\\n", "\n") %>%
    #   stringr::str_replace_all("\n", "") %>%
    #   stringr::str_replace_all("\\\\", "\\\\n")
  }
  if(is.null(def$comments)) def$comments <- ""
  mod <- NULL
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
                                  parameters = names(def$parameters),
                                  units = def$units,
                                  declare_variables = def$variables,
                                  covariates = def$covariates,
                                  as_is = TRUE,
                                  package = package,
                                  iiv = def$iiv,
                                  iov = def$iov,
                                  omega_matrix = def$omega_matrix,
                                  ruv = def$ruv,
                                  ltbs = def$ltbs,
                                  misc = def$misc,
                                  lagtime = def$lagtime,
                                  default_parameters = def$parameters,
                                  state_init = def$state_init,
                                  verbose = verbose,
                                  nonmem = nonmem,
                                  validation = validation,
                                  custom_parameters = custom_parameters,
                                  int_step_size = def$simulation$int_step_size,
                                  comments = stringr::str_replace_all(def$comments, '\\"', ""),
                                  version = ifelse(!is.null(def$version), def$version, "0.1.0"),
                                  test_file = test_file,
                                  ...)
    if(run_tests) {
      if(file.exists(tmp_file)) {
        if(verbose) message("Running test(s)...")
        source(tmp_file, local=TRUE)
        detach(paste0("package:", package), unload=TRUE, character.only=TRUE)
      }
    }
  }
  if(is.null(mod)) {
    message("Done.")
  } else {
    return(mod)
  }
}
