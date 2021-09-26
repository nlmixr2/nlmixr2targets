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
  target_list <- tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr::pheno_sd, est="saem")
  expect_true(inherits(target_list, "list"))
})

test_that("tar_nlmixr execution", {
  skip("How do I test a tar_script when the package is not installed?")

  targets::tar_destroy(ask=FALSE)
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

    nlmixrtargets::tar_nlmixr(name=pheno_model, object=pheno, data=nlmixr::pheno_sd, est="saem", control=nlmixr::saemControl(nBurn=1, nEm=1))
  })
  targets::tar_make()
  expect_true(inherits(target_list, "list"))
  targets::tar_destroy(ask=FALSE)
})
