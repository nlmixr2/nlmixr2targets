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
  # control's 'keep' argument is respected
  expect_equal(
    names(nlmixr_data_simplify(
      data = nlmixr2data::pheno_sd,
      object = model_simple,
      table = nlmixr2est::tableControl(keep = "APGR")
    )),
    c("id", "time", "amt", "dv", "mdv", "evid", "WT", "APGR")
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
