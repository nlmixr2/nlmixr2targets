test_that("tar_nlmixr object generation", {
  pheno <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
      # interindividual variability on clearance and volume
      add.err <- 0.1    # residual variability
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(add.err)       # define error model
    })
  }
  target_list <- tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr2data::pheno_sd, est="saem")
  expect_true(inherits(target_list, "list"))
})

test_that("tar_nlmixr expected errors", {
  expect_error(
    tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr2data::pheno_sd),
    regexp="'est' must not be null",
    fixed=TRUE
  )
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_nlmixr execution", {
  targets::tar_script({
    pheno <- function() {
      ini({
        tcl <- log(0.008) # typical value of clearance
        tv <-  log(0.6)   # typical value of volume
        ## var(eta.cl)
        eta.cl + eta.v ~ c(1,
                           0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
        # interindividual variability on clearance and volume
        add.err <- 0.1    # residual variability
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ add(add.err)       # define error model
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
