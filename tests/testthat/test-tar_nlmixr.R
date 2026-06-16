test_that("tar_nlmixr object generation", {
  pheno <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(1,
                          0.01, 1)
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl + etalcl)
      vc <- exp(lvc + etalvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      cp ~ add(cpaddSd)
    })
  }
  target_list <- tar_nlmixr(name = pheno_model, object = pheno, data = nlmixr2data::pheno_sd, est = "saem")
  expect_true(inherits(target_list, "list"))
})

test_that("tar_nlmixr expected errors", {
  expect_error(
    tar_nlmixr(name = pheno_model, object = pheno, data = nlmixr2data::pheno_sd),
    regexp = "'est' must not be null",
    fixed = TRUE
  )
  expect_error(
    tar_nlmixr(
      name = pheno_model, object = pheno, data = nlmixr2data::pheno_sd,
      est = "saem", env = "not an environment"
    ),
    regexp = "Must be an environment",
    fixed = TRUE
  )
})

# Helper: build a minimal fit-like object that assign_origData() will
# accept. The function only touches fit$env (an environment) and
# fit$env$origData (a data frame), so we don't need real nlmixr2 here.
make_fake_fit <- function(orig = data.frame(x = 1:3, y = 4:6)) {
  e <- new.env(parent = emptyenv())
  e$origData <- orig
  list(env = e)
}

test_that("assign_origData swaps origData in place", {
  fit <- make_fake_fit(data.frame(x = 1:3, y = 4:6))
  new_data <- data.frame(x = 11:13, y = 14:16)
  out <- assign_origData(fit = fit, data = new_data)
  expect_identical(out$env$origData, new_data)
  # Same env, mutated in place
  expect_identical(fit$env$origData, new_data)
})

test_that("assign_origData rejects nrow mismatch", {
  fit <- make_fake_fit(data.frame(x = 1:3))
  expect_error(
    assign_origData(fit = fit, data = data.frame(x = 1:4)),
    regexp = "Must have exactly 3 rows"
  )
})

test_that("assign_origData rejects non-data-frame inputs", {
  fit <- make_fake_fit()
  expect_error(
    assign_origData(fit = fit, data = list(x = 1:3)),
    regexp = "Must be of type 'data.frame'"
  )
})

test_that("assign_origData rejects fits without an env", {
  expect_error(
    assign_origData(fit = list(env = NULL), data = data.frame(x = 1:3)),
    regexp = "Must be an environment"
  )
})

test_that("assign_origData rejects fits without origData", {
  e <- new.env(parent = emptyenv())
  expect_error(
    assign_origData(fit = list(env = e), data = data.frame(x = 1:3)),
    regexp = "Must be of type 'data.frame'"
  )
})

test_that("tar_nlmixr_raw constructs four targets with expected names", {
  out <- tar_nlmixr_raw(
    name = "fit_x",
    object = quote(my_model),
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    object_simple_name = "fit_x_object_simple",
    data_simple_name = "fit_x_data_simple",
    fit_simple_name = "fit_x_fit_simple",
    env = environment()
  )
  expect_named(out, c("object_simple", "data_simple", "fit_simple", "fit"))
  expect_equal(out$object_simple$settings$name, "fit_x_object_simple")
  expect_equal(out$data_simple$settings$name, "fit_x_data_simple")
  expect_equal(out$fit_simple$settings$name, "fit_x_fit_simple")
  expect_equal(out$fit$settings$name, "fit_x")
  # Verify dependency wiring: data_simple consumes object_simple,
  # fit_simple consumes both, fit consumes fit_simple.
  expect_true("fit_x_object_simple" %in% targets::tar_deps_raw(out$data_simple$command$expr))
  expect_true("fit_x_object_simple" %in% targets::tar_deps_raw(out$fit_simple$command$expr))
  expect_true("fit_x_data_simple" %in% targets::tar_deps_raw(out$fit_simple$command$expr))
  expect_true("fit_x_fit_simple" %in% targets::tar_deps_raw(out$fit$command$expr))
})

test_that("tar_nlmixr_raw rejects malformed name arguments", {
  args <- list(
    object = quote(my_model),
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    object_simple_name = "fit_x_object_simple",
    data_simple_name = "fit_x_data_simple",
    fit_simple_name = "fit_x_fit_simple",
    env = environment()
  )
  expect_error(do.call(tar_nlmixr_raw, c(list(name = ""),       args)), regexp = "name")
  expect_error(do.call(tar_nlmixr_raw, c(list(name = NA_character_), args)), regexp = "name")
  expect_error(do.call(tar_nlmixr_raw, c(list(name = c("a", "b")), args)), regexp = "name")
})

test_that("tar_nlmixr_raw rejects malformed simple_name arguments", {
  base_args <- list(
    name = "fit_x",
    object = quote(my_model),
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    object_simple_name = "fit_x_object_simple",
    data_simple_name = "fit_x_data_simple",
    fit_simple_name = "fit_x_fit_simple",
    env = environment()
  )
  bad_object <- base_args; bad_object$object_simple_name <- ""
  bad_data <- base_args; bad_data$data_simple_name <- NA_character_
  expect_error(do.call(tar_nlmixr_raw, bad_object), regexp = "object_simple_name")
  expect_error(do.call(tar_nlmixr_raw, bad_data), regexp = "data_simple_name")
})

# Issue #35: the error mode chosen by the user must reach the generated
# fit_simple target command so nlmixr2_indirect() runs in the right mode.
# (Called directly rather than via do.call() so the quoted `object` symbol is
# passed through as a language object instead of being evaluated.)
test_that("tar_nlmixr_raw threads the error mode into the fit_simple command", {
  # Default mode is "stop".
  out_default <- tar_nlmixr_raw(
    name = "fit_x",
    object = quote(my_model),
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    object_simple_name = "fit_x_object_simple",
    data_simple_name = "fit_x_data_simple",
    fit_simple_name = "fit_x_fit_simple",
    env = environment()
  )
  cmd_default <- paste(deparse(out_default$fit_simple$command$expr), collapse = " ")
  expect_match(cmd_default, 'error = "stop"', fixed = TRUE)

  out_continue <- tar_nlmixr_raw(
    name = "fit_x",
    object = quote(my_model),
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    object_simple_name = "fit_x_object_simple",
    data_simple_name = "fit_x_data_simple",
    fit_simple_name = "fit_x_fit_simple",
    env = environment(),
    error = "continue"
  )
  cmd_continue <- paste(deparse(out_continue$fit_simple$command$expr), collapse = " ")
  expect_match(cmd_continue, 'error = "continue"', fixed = TRUE)
})

test_that("tar_nlmixr rejects an unknown error mode", {
  expect_error(
    tar_nlmixr(
      name = pheno_model, object = pheno, data = nlmixr2data::pheno_sd,
      est = "saem", error = "nope"
    ),
    regexp = "should be one of"
  )
})

test_that("tar_nlmixr_raw rejects an unknown error mode", {
  expect_error(
    tar_nlmixr_raw(
      name = "fit_x",
      object = quote(my_model),
      data = quote(my_data),
      est = "saem",
      control = quote(list()),
      table = quote(list()),
      object_simple_name = "fit_x_object_simple",
      data_simple_name = "fit_x_data_simple",
      fit_simple_name = "fit_x_fit_simple",
      env = environment(),
      error = "nope"
    ),
    regexp = "element of set"
  )
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_nlmixr execution", {
  targets::tar_script({
    library(nlmixr2targets)
    pheno <- function() {
      ini({
        lcl <- log(0.008); label("Typical value of clearance")
        lvc <-  log(0.6); label("Typical value of volume of distribution")
        etalcl + etalvc ~ c(1,
                            0.01, 1)
        cpaddSd <- 0.1; label("residual variability")
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc + etalvc)
        kel <- cl/vc
        d/dt(central) <- -kel*central
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }

    nlmixr2targets::tar_nlmixr(
      name=pheno_model,
      object=pheno,
      data=nlmixr2data::pheno_sd,
      est="saem",
      # Minimize time spent
      control=nlmixr2est::saemControl(nBurn=1, nEm=1)
    )
  })
  expect_equal(
    targets::tar_manifest()$name,
    c("pheno_model_object_simple", "pheno_model_data_simple", "pheno_model_fit_simple", "pheno_model")
  )
  suppressMessages(suppressWarnings(
    targets::tar_make(callr_function = NULL)
  ))
  # A successful model estimation step should return an nlmixr2FitCore object
  # (testing of model results is outside the scope of nlmixr2targets)
  expect_type(targets::tar_read(pheno_model_object_simple), "character")
  expect_s3_class(targets::tar_read(pheno_model_data_simple), class = "data.frame")
  expect_s3_class(targets::tar_read(pheno_model_fit_simple), "nlmixr2FitCore")
  expect_s3_class(targets::tar_read(pheno_model), "nlmixr2FitCore")
  # tar_nlmixr sets the original data back into the object (#17)
  expect_false(
    identical(
      tar_read(pheno_model_fit_simple)$env$origData,
      tar_read(pheno_model)$env$origData
    )
  )
  expect_equal(
    tar_read(pheno_model)$env$origData,
    nlmixr2data::pheno_sd
  )
  # tar_nlmixr restores parameter labels on the final fit (#28). The
  # simplified-and-stripped fit has all-NA labels; the final fit reads
  # them back from the original model.
  fit_simple <- targets::tar_read(pheno_model_fit_simple)
  expect_true(all(is.na(fit_simple$ui$iniDf$label)))
  iniDf_final <- targets::tar_read(pheno_model)$ui$iniDf
  expect_equal(
    iniDf_final$label[iniDf_final$name == "lcl"],
    "Typical value of clearance"
  )
  expect_equal(
    iniDf_final$label[iniDf_final$name == "lvc"],
    "Typical value of volume of distribution"
  )
  expect_equal(
    iniDf_final$label[iniDf_final$name == "cpaddSd"],
    "residual variability"
  )
})

targets::tar_test("tar_nlmixr handling with initial conditions central(0), without running the target", {
  pheno <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(1,
                          0.01, 1)
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl + etalcl)
      vc <- exp(lvc + etalvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      central(initial) <- 0
      cp ~ add(cpaddSd)
    })
  }

  nlmixr2targets::tar_nlmixr(
    name=pheno_model,
    object=pheno,
    data=nlmixr2data::pheno_sd,
    est="saem",
    # Minimize time spent
    control=nlmixr2est::saemControl(nBurn=1, nEm=1)
  )

  expect_type(pheno, "closure")
})

targets::tar_test("tar_nlmixr handling with initial conditions central(0) including model piping, without running the target", {
  pheno <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(1,
                          0.01, 1)
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl + etalcl)
      vc <- exp(lvc + etalvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      central(0) <- 0
      cp ~ add(cpaddSd)
    })
  }

  nlmixr2targets::tar_nlmixr(
    name=pheno_model,
    object=pheno |> ini(lcl = log(0.1)),
    data=nlmixr2data::pheno_sd,
    est="saem",
    # Minimize time spent
    control=nlmixr2est::saemControl(nBurn=1, nEm=1)
  )

  expect_type(pheno, "closure")
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
#
# Exercises the user-facing cmt(0) syntax (no manual cmt(initial)
# workaround) end-to-end. tar_outdated() is the operation that walks
# env functions via codetools::findGlobals(), so any regression in the
# construction-time in-env rewrite surfaces here.
targets::tar_test("tar_nlmixr handles user-written central(0) end-to-end", {
  targets::tar_script({
    pheno <- function() {
      ini({
        lcl <- log(0.008); label("Typical value of clearance")
        lvc <-  log(0.6); label("Typical value of volume of distribution")
        etalcl + etalvc ~ c(1,
                            0.01, 1)
        cpaddSd <- 0.1; label("residual variability")
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc + etalvc)
        kel <- cl/vc
        d/dt(central) <- -kel*central
        cp <- central/vc
        central(0) <- 0
        cp ~ add(cpaddSd)
      })
    }

    nlmixr2targets::tar_nlmixr(
      name=pheno_model,
      object=pheno,
      data=nlmixr2data::pheno_sd,
      est="saem",
      # Minimize time spent
      control=nlmixr2est::saemControl(nBurn=1, nEm=1)
    )
  })
  expect_equal(
    targets::tar_manifest()$name,
    c("pheno_model_object_simple", "pheno_model_data_simple", "pheno_model_fit_simple", "pheno_model")
  )
  expect_no_error(targets::tar_outdated(callr_function = NULL))
  suppressWarnings(targets::tar_make(callr_function = NULL))
  expect_s3_class(tar_read(pheno_model), "nlmixr2FitCore")
})

# Pipe form: the LHS function has cmt(0) in its body AND the pipe RHS
# is a model({...}) block that also uses cmt(0). Both the in-env
# rewrite and the runtime delayed-eval wrap must be active for this to
# work.
targets::tar_test("tar_nlmixr handles central(0) on the RHS of a model pipe", {
  targets::tar_script({
    pheno_base <- function() {
      ini({
        lcl <- log(0.008); label("Typical value of clearance")
        lvc <-  log(0.6); label("Typical value of volume of distribution")
        etalcl + etalvc ~ c(1,
                            0.01, 1)
        cpaddSd <- 0.1; label("residual variability")
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc + etalvc)
        kel <- cl/vc
        d/dt(central) <- -kel*central
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }

    nlmixr2targets::tar_nlmixr(
      name=pheno_model,
      object = pheno_base |> model({ central(0) <- 0 }, append = TRUE),
      data=nlmixr2data::pheno_sd,
      est="saem",
      control=nlmixr2est::saemControl(nBurn=1, nEm=1)
    )
  })
  expect_no_error(targets::tar_outdated(callr_function = NULL))
  suppressWarnings(targets::tar_make(callr_function = NULL))
  expect_s3_class(tar_read(pheno_model), "nlmixr2FitCore")
})

# Documented limitation (per Option A): a function with cmt(0) declared
# in env but NOT routed through tar_nlmixr() still trips targets'
# env-wide codetools walk. This expected-failing test pins the
# limitation so any future change that unintentionally widens or
# narrows the scope is visible.
targets::tar_test("known limitation: cmt(0) functions outside tar_nlmixr still error", {
  targets::tar_script({
    pheno_unrouted <- function() {
      ini({ lcl <- log(0.008) })
      model({
        d/dt(central) <- -exp(lcl) * central
        central(0) <- 0
      })
    }
    # Deliberately no tar_nlmixr() call -- pheno_unrouted is just in env.
    list(targets::tar_target(x, 1 + 1))
  })
  # codetools::findGlobals() walks every function in env (referenced or
  # not), so this still errors. If a future change makes this pass, the
  # test fails loudly and we should update the docs.
  expect_error(
    targets::tar_outdated(callr_function = NULL),
    regexp = "bad assignment"
  )
})

# Issue #35 end-to-end: with error = "continue", an estimation failure must
# not halt tar_make(). The pipeline runs to completion and the final target
# stores a failure sentinel. A deliberately invalid `est` is the deterministic
# failure trigger: the model and data simplification steps (which do not use
# `est`) succeed, then nlmixr2est errors at the estimation step.
targets::tar_test("tar_nlmixr(error = 'continue') records a failure sentinel without halting the pipeline", {
  targets::tar_script({
    library(nlmixr2targets)
    pheno <- function() {
      ini({
        lcl <- log(0.008); label("Typical value of clearance")
        lvc <-  log(0.6); label("Typical value of volume of distribution")
        etalcl + etalvc ~ c(1,
                            0.01, 1)
        cpaddSd <- 0.1; label("residual variability")
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc + etalvc)
        kel <- cl/vc
        d/dt(central) <- -kel*central
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }

    nlmixr2targets::tar_nlmixr(
      name = pheno_model,
      object = pheno,
      data = nlmixr2data::pheno_sd,
      est = "this_is_not_a_real_estimation_method",
      error = "continue"
    )
  })
  # tar_make() completes (no error escapes) even though estimation fails.
  expect_no_error(suppressMessages(suppressWarnings(
    targets::tar_make(callr_function = NULL)
  )))
  # The simplification steps succeeded; only the fit failed.
  expect_type(targets::tar_read(pheno_model_object_simple), "character")
  expect_s3_class(targets::tar_read(pheno_model_data_simple), "data.frame")
  # Both the fit_simple and the final fit hold the failure sentinel.
  fit_simple <- targets::tar_read(pheno_model_fit_simple)
  fit <- targets::tar_read(pheno_model)
  expect_s3_class(fit_simple, "nlmixr2targetsError")
  expect_s3_class(fit, "nlmixr2targetsError")
  expect_s3_class(fit, "try-error")
  expect_false(inherits(fit, "nlmixr2FitCore"))
})

# The default (error = "stop") must still halt tar_make() on an estimation
# failure, preserving the pre-issue-#35 behavior.
targets::tar_test("tar_nlmixr default (error = 'stop') still halts on an estimation failure", {
  targets::tar_script({
    library(nlmixr2targets)
    pheno <- function() {
      ini({
        lcl <- log(0.008); label("Typical value of clearance")
        lvc <-  log(0.6); label("Typical value of volume of distribution")
        etalcl + etalvc ~ c(1,
                            0.01, 1)
        cpaddSd <- 0.1; label("residual variability")
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc + etalvc)
        kel <- cl/vc
        d/dt(central) <- -kel*central
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }

    nlmixr2targets::tar_nlmixr(
      name = pheno_model,
      object = pheno,
      data = nlmixr2data::pheno_sd,
      est = "this_is_not_a_real_estimation_method"
    )
  })
  expect_error(suppressMessages(suppressWarnings(
    targets::tar_make(callr_function = NULL)
  )))
})
