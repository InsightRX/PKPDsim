# Helper: generate (but do not install) a model package, and return the path
# to the generated R/model.R file.
generate_model_package <- function(package, ferx = NULL, nonmem = NULL) {
  folder <- tempfile("ferx_pkg")
  dir.create(folder)
  # install = FALSE runs `R CMD build`, which writes a tarball to the working
  # directory, so build from within the temp folder.
  old_wd <- setwd(folder)
  on.exit(setwd(old_wd), add = TRUE)
  suppressMessages(
    new_ode_model(
      code = "dAdt[0] = -(CL/V)*A[0];",
      obs = list(cmt = 1, scale = "V"),
      dose = list(cmt = 1, bioav = 1),
      parameters = list(CL = 5, V = 50),
      as_is = TRUE,
      package = package,
      folder = folder,
      install = FALSE,
      nonmem = nonmem,
      ferx = ferx,
      quiet = FALSE
    )
  )
  file.path(folder, package, "R", "model.R")
}

# Helper: evaluate the generated model.R and return the model attributes,
# without needing to compile/install the package. `model()` looks up the
# compiled simulation function in the attached package, so stub that lookup.
get_generated_attributes <- function(model_file) {
  env <- new.env()
  env$as.environment <- function(...) emptyenv()
  env$get <- function(...) function() NULL
  eval(parse(model_file), envir = env)
  attributes(env$model())
}

test_that("ferx code is stored as model attribute in generated package", {
  skip_on_cran()
  ferx_code <- "model 1cmt_iv\n  CL = THETA(1)\n  V = THETA(2)\nend"
  model_file <- generate_model_package("ferxtest1", ferx = ferx_code)
  expect_true(file.exists(model_file))
  model_txt <- readLines(model_file)
  expect_false(any(grepl("[FERX]", model_txt, fixed = TRUE)))
  attrs <- get_generated_attributes(model_file)
  expect_equal(attrs$ferx, ferx_code)
})

test_that("ferx attribute defaults to 'NULL' when not supplied", {
  skip_on_cran()
  model_file <- generate_model_package("ferxtest2")
  attrs <- get_generated_attributes(model_file)
  expect_equal(attrs$ferx, "NULL")
  expect_equal(attrs$nonmem, "NULL")
})

test_that("ferx attribute defaults to 'NULL' when empty vector supplied", {
  skip_on_cran()
  model_file <- generate_model_package("ferxtest3", ferx = character(0))
  attrs <- get_generated_attributes(model_file)
  expect_equal(attrs$ferx, "NULL")
})

test_that("ferx and nonmem attributes are independent", {
  skip_on_cran()
  model_file <- generate_model_package(
    "ferxtest4",
    nonmem = "$PROB nonmem code",
    ferx = "ferx code"
  )
  attrs <- get_generated_attributes(model_file)
  expect_equal(attrs$nonmem, "$PROB nonmem code")
  expect_equal(attrs$ferx, "ferx code")
})

test_that("model_from_api reads ferx file when implementation is defined", {
  def_file <- tempfile(fileext = ".json5")
  ferx_file <- tempfile(fileext = ".ferx")
  writeLines(c("line 1", "line 2"), ferx_file)
  def <- read_model_json(test_path("sample_json", "test_1cmt_iv.json5"))
  def$implementations <- list(ferx = TRUE)
  writeLines(jsonlite::toJSON(def, auto_unbox = TRUE), def_file)

  captured <- NULL
  local_mocked_bindings(
    new_ode_model = function(...) {
      captured <<- list(...)
      "model"
    }
  )
  model_from_api(
    url = def_file,
    model = "test_1cmt_iv",
    ferx = ferx_file,
    verbose = FALSE,
    force = TRUE
  )
  expect_equal(captured$ferx, "line 1\nline 2")
  expect_null(captured$nonmem)
})

test_that("model_from_api passes NULL ferx when no ferx file supplied", {
  def_file <- tempfile(fileext = ".json5")
  def <- read_model_json(test_path("sample_json", "test_1cmt_iv.json5"))
  def$implementations <- list(ferx = TRUE)
  writeLines(jsonlite::toJSON(def, auto_unbox = TRUE), def_file)

  captured <- NULL
  local_mocked_bindings(
    new_ode_model = function(...) {
      captured <<- list(...)
      "model"
    }
  )
  model_from_api(
    url = def_file,
    model = "test_1cmt_iv",
    verbose = FALSE,
    force = TRUE
  )
  expect_true("ferx" %in% names(captured))
  expect_null(captured$ferx)
})

test_that("ferx attribute available on installed model package", {
  skip_on_cran()
  instloc <- tempfile("package")
  srcloc <- file.path(instloc, "src")
  dir.create(instloc)
  dir.create(srcloc)
  on.exit(unlink(instloc, recursive = TRUE), add = TRUE)
  ferx_code <- "ferx model code"
  suppressWarnings(
    suppressMessages(
      new_ode_model(
        code = "dAdt[0] = -(CL/V)*A[0];",
        obs = list(cmt = 1, scale = "V"),
        dose = list(cmt = 1, bioav = 1),
        parameters = list(CL = 5, V = 50),
        as_is = TRUE,
        package = "ferxinstalltest",
        folder = srcloc,
        lib_location = instloc,
        ferx = ferx_code,
        quiet = NULL
      )
    )
  )
  expect_true("ferxinstalltest" %in% installed.packages(lib.loc = instloc))
  suppressMessages(require(ferxinstalltest, lib.loc = instloc))
  on.exit(
    detach("package:ferxinstalltest", unload = TRUE),
    add = TRUE,
    after = FALSE
  )
  mod <- ferxinstalltest::model()
  expect_equal(attr(mod, "ferx"), ferx_code)
  expect_equal(attr(mod, "nonmem"), "NULL")
})

test_that("ferx code with quotes, backslashes and newlines round-trips", {
  skip_on_cran()
  ferx_code <- paste(
    'label = "central"',
    "path = 'C:\\models\\1cmt'",
    "regex = \\1 and \\\\",
    sep = "\n"
  )
  model_file <- generate_model_package("ferxtest5", ferx = ferx_code)
  expect_silent(parse(model_file))
  attrs <- get_generated_attributes(model_file)
  expect_equal(attrs$ferx, ferx_code)
})

test_that("ferx code is stored as attribute on directly compiled models", {
  skip_on_cran()
  ferx_code <- 'model "1cmt"\nend'
  mod <- new_ode_model(
    code = "dAdt[0] = -(CL/V)*A[0];",
    obs = list(cmt = 1, scale = "V"),
    parameters = list(CL = 5, V = 50),
    ferx = ferx_code
  )
  expect_equal(attr(mod, "ferx"), ferx_code)
  expect_null(attr(mod_1cmt_iv, "ferx"))
})

test_that("ferx argument does not shift existing positional arguments", {
  args_api <- names(formals(model_from_api))
  expect_equal(
    args_api[1:8],
    c("url", "model", "nonmem", "verbose", "get_definition",
      "to_package", "force", "install_all")
  )
  args_ode <- names(formals(new_ode_model))
  expect_equal(args_ode[length(args_ode)], "ferx")
  expect_equal(args_ode[length(args_ode) - 1], "definition")
})
