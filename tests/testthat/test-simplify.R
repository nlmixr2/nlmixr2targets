pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <-  log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)*WT/70
    vc <- exp(lvc + etalvc)
    kel <- cl/vc
    d/dt(central) <- -kel*central
    cp <- central/vc
    cp ~ add(cpaddSd)
  })
}

# `nlmixr_object_simplify()` writes the simplified rxUi to disk. The
# function's default `directory` resolves to
# `file.path(targets::tar_config_get("store"), "user/nlmixr2")` which —
# outside a `tar_test()` / `tar_dir()` sandbox — points under
# tests/testthat/. CRAN policy disallows writing there, so this file
# routes all writes/reads through a per-session tempfile dir.
.simplify_cache_dir <- tempfile("nlmixr2targets-test-cache-")
dir.create(.simplify_cache_dir, recursive = TRUE)

model_simple <-
  suppressMessages(suppressWarnings(
    nlmixr_object_simplify(pheno, directory = .simplify_cache_dir)
  ))

test_that("nlmixr_data_simplify", {
  # Columns are kept in the correct order
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd, object = model_simple,
      directory = .simplify_cache_dir
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
  # table's 'keep' argument is respected
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "APGR"),
      directory = .simplify_cache_dir
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "APGR", "WT")
  )
  # duplication between table's 'keep' argument and covariates does not
  # duplicate columns
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "WT"),
      directory = .simplify_cache_dir
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
  # duplication between table's 'keep' argument and nlmixr2 columns does not add
  # them
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "MDV"),
      directory = .simplify_cache_dir
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
})

test_that("nlmixr_data_simplify expected errors", {
  bad_data_lower_case <- nlmixr2data::pheno_sd
  bad_data_lower_case$id <- bad_data_lower_case$ID
  expect_error(
    nlmixr_data_simplify(
      data = bad_data_lower_case, object = model_simple,
      directory = .simplify_cache_dir
    ),
    regexp = "The following column(s) are duplicated when lower case: 'id'",
    fixed = TRUE
  )
  bad_data_no_cov <- nlmixr2data::pheno_sd
  bad_data_no_cov$WT <- NULL
  expect_error(
    nlmixr_data_simplify(
      data = bad_data_no_cov, object = model_simple,
      directory = .simplify_cache_dir
    ),
    regexp = "The following covariate column(s) are missing from the data: 'WT'",
    fixed = TRUE
  )
})

test_that("nlmixr_object_simplify_zero_initial", {
  pheno <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(1,
                          0.01, 1)
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl + etalcl)*WT/70
      vc <- exp(lvc + etalvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      cp ~ add(cpaddSd)
      central(initial) <- 1
    })
  }
  pheno_0 <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(1,
                          0.01, 1)
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl + etalcl)*WT/70
      vc <- exp(lvc + etalvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      cp ~ add(cpaddSd)
      central(0) <- 1
    })
  }
  new_model <- nlmixr_object_simplify_zero_initial(pheno)
  expect_equal(body(new_model), body(pheno_0))
})

test_that("nlmixr_object_simplify_zero_initial_helper rewrites cmt(initial) to cmt(0)", {
  expr <- quote(central(initial) <- 1)
  expected <- quote(central(0) <- 1)
  expect_equal(nlmixr_object_simplify_zero_initial_helper(expr), expected)
})

test_that("nlmixr_object_simplify_zero_initial_helper recurses into nested calls", {
  expr <- quote({
    a <- 1
    central(initial) <- 2
    depot(initial) <- 3
  })
  out <- nlmixr_object_simplify_zero_initial_helper(expr)
  expect_equal(out[[2]], quote(a <- 1))
  expect_equal(out[[3]], quote(central(0) <- 2))
  expect_equal(out[[4]], quote(depot(0) <- 3))
})

test_that("nlmixr_object_simplify_zero_initial_helper leaves non-matching expressions unchanged", {
  # atomic
  expect_identical(nlmixr_object_simplify_zero_initial_helper(42), 42)
  expect_identical(nlmixr_object_simplify_zero_initial_helper("foo"), "foo")
  # name
  nm <- as.name("x")
  expect_identical(nlmixr_object_simplify_zero_initial_helper(nm), nm)
  # call that doesn't match the template
  call_other <- quote(f(x, y))
  expect_equal(nlmixr_object_simplify_zero_initial_helper(call_other), call_other)
})

test_that("nlmixr_object_simplify_zero_initial returns non-functions unchanged", {
  # The wrapper short-circuits when the input is not a function.
  expect_identical(nlmixr_object_simplify_zero_initial(42), 42)
  expect_identical(nlmixr_object_simplify_zero_initial("foo"), "foo")
})

test_that("nlmixr_object_protect_zero_initial rewrites cmt(0) inside model({})", {
  body_with_zero <- quote({
    ini({
      lcl <- log(0.008); cpaddSd <- 0.1
    })
    model({
      cl <- exp(lcl)
      d/dt(eff) <- -cl * eff
      eff(0) <- 1.5
    })
  })
  body_with_initial <- quote({
    ini({
      lcl <- log(0.008); cpaddSd <- 0.1
    })
    model({
      cl <- exp(lcl)
      d/dt(eff) <- -cl * eff
      eff(initial) <- 1.5
    })
  })
  res <- nlmixr_object_protect_zero_initial(body_with_zero)
  expect_equal(res$n_rewrites, 1L)
  expect_equal(res$expr, body_with_initial)

  # Round-trip via the existing inverse must recover the original.
  expect_equal(
    nlmixr_object_simplify_zero_initial_helper(res$expr),
    body_with_zero
  )
})

test_that("nlmixr_object_protect_zero_initial rewrites multiple cmt(0) entries", {
  body_with_zero <- quote({
    model({
      central(0) <- 0
      depot(0) <- 1
    })
  })
  body_with_initial <- quote({
    model({
      central(initial) <- 0
      depot(initial) <- 1
    })
  })
  res <- nlmixr_object_protect_zero_initial(body_with_zero)
  expect_equal(res$n_rewrites, 2L)
  expect_equal(res$expr, body_with_initial)
})

test_that("nlmixr_object_protect_zero_initial scopes to model({}) only", {
  # `cmt(0)` outside any `model({...})` block is left alone -- the user
  # explicitly asked for this scoping. (codetools would still reject it,
  # but no realistic nlmixr2 model has cmt(0) outside model({}) anyway.)
  expr <- quote({
    ini({ a <- 1 })
    eff(0) <- 2
  })
  res <- nlmixr_object_protect_zero_initial(expr)
  expect_equal(res$n_rewrites, 0L)
  expect_equal(res$expr, expr)
})

test_that("nlmixr_object_protect_zero_initial is a no-op for non-call values", {
  for (val in list(quote(make_model()), as.name("pheno"), 1, "x")) {
    res <- nlmixr_object_protect_zero_initial(val)
    expect_equal(res$n_rewrites, 0L)
  }
})

test_that("nlmixr_object_zero_initial_eval restores cmt(0) at runtime", {
  envir <- new.env(parent = baseenv())
  # Fake nlmixr2 model() function: captures its second arg unevaluated
  # so we can compare against the rewritten block.
  envir$fake_model <- function(x, expr) substitute(expr)
  expr <- quote(fake_model(0, { eff(initial) <- 1 }))
  res <- nlmixr_object_zero_initial_eval(expr, envir = envir)
  expect_equal(res, quote({ eff(0) <- 1 }))
})

test_that("nlmixr_object_zero_initial_eval supplies corrected closures for symbols", {
  envir <- new.env(parent = baseenv())
  envir$pheno <- function() {
    model({
      eff(initial) <- 1
    })
  }
  envir$grab_body <- function(fn) body(fn)
  expr <- quote(grab_body(pheno))
  res <- nlmixr_object_zero_initial_eval(expr, envir = envir)
  expect_equal(res, quote({ model({ eff(0) <- 1 }) }))
  # The original binding in envir is left untouched -- the corrected
  # copy only lives in the override frame.
  expect_equal(
    body(envir$pheno),
    quote({ model({ eff(initial) <- 1 }) })
  )
})

test_that("nlmixr_data_simplify_cols returns cols unchanged when all present", {
  d <- data.frame(WT = 1, AGE = 2, ID = 3)
  expect_identical(
    nlmixr_data_simplify_cols(d, cols = c("WT", "AGE"), type = "covariate"),
    c("WT", "AGE")
  )
  # NULL cols are preserved (used for table$keep when keep is unset)
  expect_null(nlmixr_data_simplify_cols(d, cols = NULL, type = "keep"))
  # Empty character vector also preserved
  expect_identical(
    nlmixr_data_simplify_cols(d, cols = character(0), type = "covariate"),
    character(0)
  )
})

test_that("nlmixr_data_simplify_cols error message names the type and missing column", {
  d <- data.frame(WT = 1)
  expect_error(
    nlmixr_data_simplify_cols(d, cols = c("WT", "AGE"), type = "covariate"),
    regexp = "The following covariate column(s) are missing from the data: 'AGE'",
    fixed = TRUE
  )
  expect_error(
    nlmixr_data_simplify_cols(d, cols = c("APGR"), type = "keep"),
    regexp = "The following keep column(s) are missing from the data: 'APGR'",
    fixed = TRUE
  )
})

test_that("nlmixr_data_simplify rejects non-data-frame data", {
  expect_error(
    nlmixr_data_simplify(data = list(a = 1), object = model_simple),
    regexp = "Must be of type 'data.frame'"
  )
})

test_that("nlmixr_data_simplify rejects non-list table", {
  expect_error(
    nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd, object = model_simple, table = "bad",
      directory = .simplify_cache_dir
    ),
    regexp = "Must be of type 'list'"
  )
})

test_that("re-estimating a model works with covariates (#9)", {
  bad_data_lower_case <- nlmixr2data::pheno_sd
  bad_data_lower_case$id <- bad_data_lower_case$ID
  fit_estimated <-
    suppressMessages(
      nlmixr2est::nlmixr(
        object = read_nlmixr2obj_indirect(model_simple, directory = .simplify_cache_dir),
        data = nlmixr2data::pheno_sd,
        est = "focei",
        control = list(eval.max = 1)
      )
    )
  expect_true(
    "WT" %in% names(nlmixr_data_simplify(data = nlmixr2data::pheno_sd, object = fit_estimated))
  )
})

# Tests for the strip-and-restore round trip introduced for #28.

test_that("nlmixr_object_simplify writes the simplified rxUi with labels and meta stripped", {
  cached <- read_nlmixr2obj_indirect(model_simple, directory = .simplify_cache_dir)
  expect_true(all(is.na(cached$iniDf$label)))
  expect_equal(ls(envir = cached$meta, all.names = TRUE), character(0))
})

test_that("nlmixr_object_simplify yields the same hash for models that differ only in labels (#28)", {
  model_with <- function() {
    ini({
      lcl <- log(0.008); label("Typical clearance")
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl)
      d/dt(central) <- -cl/1*central
      cp <- central/1
      cp ~ add(cpaddSd)
    })
  }
  model_relabeled <- function() {
    ini({
      lcl <- log(0.008); label("Different clearance label")
      cpaddSd <- 0.1; label("Different residual label")
    })
    model({
      cl <- exp(lcl)
      d/dt(central) <- -cl/1*central
      cp <- central/1
      cp ~ add(cpaddSd)
    })
  }
  hash_with <- suppressMessages(suppressWarnings(
    nlmixr_object_simplify(model_with, directory = .simplify_cache_dir)
  ))
  hash_relabeled <- suppressMessages(suppressWarnings(
    nlmixr_object_simplify(model_relabeled, directory = .simplify_cache_dir)
  ))
  expect_identical(hash_with, hash_relabeled)
})

test_that("nlmixr_object_complicate restores labels, meta, and data from the original model", {
  model <- function() {
    ini({
      lcl <- log(0.008); label("Typical clearance")
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl)
      d/dt(central) <- -cl/1*central
      cp <- central/1
      cp ~ add(cpaddSd)
    })
  }
  # Build a fake fit that mimics the post-fit shape: fit$env holds origData
  # and a "ui" slot, and fit$ui is a list (with class rxUi) keyed off
  # fit$env$ui (the same shape nlmixr2est attaches to fitted objects).
  fit <- list(env = new.env(parent = emptyenv()))
  fit$env$origData <- data.frame(x = 1:3)
  source_ui <- suppressMessages(suppressWarnings(nlmixr2est::nlmixr(model)))
  source_ui$iniDf$label <- NA_character_
  rm(list = ls(envir = source_ui$meta, all.names = TRUE), envir = source_ui$meta)
  # Snapshot to a list (the way targets serialization produces it):
  fit$env$ui <-
    structure(
      list(
        iniDf = source_ui$iniDf,
        meta = list()
      ),
      class = c("rxUi", "list")
    )
  out <-
    nlmixr_object_complicate(
      fit = fit,
      object = model,
      data = data.frame(x = 11:13)
    )
  iniDf <- out$env$ui$iniDf
  expect_equal(iniDf$label[iniDf$name == "lcl"], "Typical clearance")
  expect_equal(iniDf$label[iniDf$name == "cpaddSd"], "residual variability")
  expect_identical(out$env$origData, data.frame(x = 11:13))
})

test_that("nlmixr_object_complicate works when fit$ui is an environment (no fit$env$ui)", {
  model <- function() {
    ini({
      lcl <- log(0.008); label("Typical clearance")
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl)
      d/dt(central) <- -cl/1*central
      cp <- central/1
      cp ~ add(cpaddSd)
    })
  }
  fit <- list(env = new.env(parent = emptyenv()))
  fit$env$origData <- data.frame(x = 1:3)
  ui <- suppressMessages(suppressWarnings(nlmixr2est::nlmixr(model)))
  ui$iniDf$label <- NA_character_
  rm(list = ls(envir = ui$meta, all.names = TRUE), envir = ui$meta)
  fit$ui <- ui
  out <-
    nlmixr_object_complicate(
      fit = fit,
      object = model,
      data = data.frame(x = 11:13)
    )
  iniDf <- out$ui$iniDf
  expect_equal(iniDf$label[iniDf$name == "lcl"], "Typical clearance")
  expect_equal(iniDf$label[iniDf$name == "cpaddSd"], "residual variability")
  expect_identical(out$env$origData, data.frame(x = 11:13))
})

test_that("nlmixr_object_complicate errors when no ui is present", {
  # Neither fit$env$ui nor fit$ui — should fail before even touching the
  # source model.
  fit <- list(env = new.env(parent = emptyenv()), ui = NULL)
  fit$env$origData <- data.frame(x = 1:3)
  expect_error(
    nlmixr_object_complicate(
      fit = fit,
      object = quote(unused_because_we_fail_early),
      data = data.frame(x = 1:3)
    ),
    regexp = "Could not find a ui on the fit"
  )
})

test_that("nlmixr_object_complicate rejects nrow-mismatched data via assign_origData", {
  model <- function() {
    ini({lcl <- log(0.008); cpaddSd <- 0.1})
    model({
      cl <- exp(lcl)
      d/dt(central) <- -cl/1*central
      cp <- central/1
      cp ~ add(cpaddSd)
    })
  }
  fit <- list(env = new.env(parent = emptyenv()))
  fit$env$origData <- data.frame(x = 1:3)
  ui <- suppressMessages(suppressWarnings(nlmixr2est::nlmixr(model)))
  ui$iniDf$label <- NA_character_
  rm(list = ls(envir = ui$meta, all.names = TRUE), envir = ui$meta)
  fit$ui <- ui
  expect_error(
    nlmixr_object_complicate(
      fit = fit,
      object = model,
      data = data.frame(x = 1:4)
    ),
    regexp = "Must have exactly 3 rows"
  )
})
