test_that("tar_nlmixr_multimodel", {
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

  pheno2 <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(2,
                          0.01, 2)
      cpaddSd <- 3.0; label("residual variability")
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

  target_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model" = pheno,
      "my second model" = pheno2
    )
  expect_true(inherits(target_list, "list"))
  # One for each model and then one for combining everything
  expect_length(target_list, 3)
  # Data and object simplification, then the fitting
  expect_length(target_list[[1]], 3)
  # Data and object simplification, then the fitting
  expect_length(target_list[[2]], 3)
  # Combine the fit models as a single step
  expect_s3_class(target_list[[3]], "tar_stem")
  expect_equal(target_list[[3]]$settings$name, "foo")

  # Verify the expression for collation is generated correctly
  collating_call <- target_list[[3]]$command$expr[[1]]
  expect_true(grepl(x = as.character(collating_call[[2]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_true(grepl(x = as.character(collating_call[[3]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_equal(names(collating_call), c("", "my first model", "my second model"))

  # Verify the targets created are the ones being collated
  expect_equal(collating_call[[2]], as.name(target_list[[1]][[3]]$settings$name))
  expect_equal(collating_call[[3]], as.name(target_list[[2]][[3]]$settings$name))
})

test_that("tar_nlmixr_multimodel works with long model names", {
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

  pheno2 <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      etalcl + etalvc ~ c(2,
                          0.01, 2)
      cpaddSd <- 3.0; label("residual variability")
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
  plan_model <-
    tar_nlmixr_multimodel(
      all_models,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      "Base one-compartment model; IIV in clearance and volume; additive residual error" = pheno,
      "Base one-compartment model; IIV in clearance and volume; additive residual error (estimate starting at 3)" = pheno2
    )
  expect_true(inherits(target_list, "list"))
})
