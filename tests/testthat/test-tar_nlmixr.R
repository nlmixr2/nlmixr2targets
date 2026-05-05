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
    c("pheno_model_tar_object_simple", "pheno_model_tar_data_simple", "pheno_model_tar_fit_simple", "pheno_model")
  )
  suppressMessages(suppressWarnings(
    targets::tar_make(callr_function = NULL)
  ))
  # A successful model estimation step should return an nlmixr2FitCore object
  # (testing of model results is outside the scope of nlmixr2targets)
  expect_type(targets::tar_read(pheno_model_tar_object_simple), "character")
  expect_s3_class(targets::tar_read(pheno_model_tar_data_simple), class = "data.frame")
  expect_s3_class(targets::tar_read(pheno_model_tar_fit_simple), "nlmixr2FitCore")
  expect_s3_class(targets::tar_read(pheno_model), "nlmixr2FitCore")
  # tar_nlmixr sets the original data back into the object (#17)
  expect_false(
    identical(
      tar_read(pheno_model_tar_fit_simple)$env$origData,
      tar_read(pheno_model)$env$origData
    )
  )
  expect_equal(
    tar_read(pheno_model)$env$origData,
    nlmixr2data::pheno_sd
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
targets::tar_test("tar_nlmixr handling with initial conditions central(0), with running the target", {
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
  })
  expect_equal(
    targets::tar_manifest()$name,
    c("pheno_model_tar_object_simple", "pheno_model_tar_data_simple", "pheno_model_tar_fit_simple", "pheno_model")
  )
  suppressWarnings(targets::tar_make(callr_function = NULL))
  # A successful model estimation step should return an nlmixr2FitCore object
  # (testing of model results is outside the scope of nlmixr2targets)
  expect_s3_class(tar_read(pheno_model), "nlmixr2FitCore")
})
