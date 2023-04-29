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
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_nlmixr execution", {
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
    targets::tar_outdated(callr_function = NULL),
    c("pheno_model_tar_object_simple", "pheno_model_tar_data_simple", "pheno_model")
  )
  suppressWarnings(targets::tar_make(callr_function = NULL))
  # A successful model estimation step should return an nlmixr2FitCore object
  # (testing of model results is outside the scope of nlmixr2targets)
  expect_true(
    inherits(tar_read(pheno_model), "nlmixr2FitCore")
  )
})
