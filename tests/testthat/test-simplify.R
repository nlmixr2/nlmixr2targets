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

model_simple <-
  suppressMessages(suppressWarnings(
    nlmixr_object_simplify(pheno)
  ))

test_that("nlmixr_data_simplify", {
  # Columns are kept in the correct order
  expect_equal(
    names(nlmixr_data_simplify(data = nlmixr2data::pheno_sd, object = model_simple)),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
  # table's 'keep' argument is respected
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "APGR")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "APGR", "WT")
  )
  # duplication between table's 'keep' argument and covariates does not
  # duplicate columns
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "WT")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
  # duplication between table's 'keep' argument and nlmixr2 columns does not add
  # them
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "MDV")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
})

test_that("nlmixr_data_simplify expected errors", {
  bad_data_lower_case <- nlmixr2data::pheno_sd
  bad_data_lower_case$id <- bad_data_lower_case$ID
  expect_error(
    nlmixr_data_simplify(data = bad_data_lower_case, object = model_simple),
    regexp = "The following column(s) are duplicated when lower case: 'id'",
    fixed = TRUE
  )
  bad_data_no_cov <- nlmixr2data::pheno_sd
  bad_data_no_cov$WT <- NULL
  expect_error(
    nlmixr_data_simplify(data = bad_data_no_cov, object = model_simple),
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

test_that("re-estimating a model works with covariates (#9)", {
  bad_data_lower_case <- nlmixr2data::pheno_sd
  bad_data_lower_case$id <- bad_data_lower_case$ID
  fit_estimated <-
    suppressMessages(
      nlmixr2est::nlmixr(
        object = read_nlmixr2obj_indirect(model_simple),
        data = nlmixr2data::pheno_sd,
        est = "focei",
        control = list(eval.max = 1)
      )
    )
  expect_true(
    "WT" %in% names(nlmixr_data_simplify(data = nlmixr2data::pheno_sd, object = fit_estimated))
  )
})
