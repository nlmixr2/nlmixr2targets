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
    cl <- exp(tcl + eta.cl)*WT/70 # individual value of clearance
    v <- exp(tv + eta.v)    # individual value of volume
    ke <- cl / v            # elimination rate constant
    d/dt(A1) = - ke * A1    # model differential equation
    cp = A1 / v             # concentration in plasma
    cp ~ add(add.err)       # define error model
  })
}

test_that("nlmixr_object_simplify", {
  expect_equal(
    suppressMessages(suppressWarnings(
      nlmixr_object_simplify(pheno)
    ))$model.name,
    "object"
  )
})

test_that("nlmixr_data_simplify", {
  model_simple <-
    suppressMessages(suppressWarnings(
      nlmixr_object_simplify(pheno)
    ))
  # Columns are kept in the correct order
  expect_equal(
    names(nlmixr_data_simplify(data=nlmixr::pheno_sd, object=model_simple)),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT")
  )
})
