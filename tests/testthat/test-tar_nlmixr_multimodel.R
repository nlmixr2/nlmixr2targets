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
  expect_length(target_list[[1]], 4)
  # Data and object simplification, then the fitting
  expect_length(target_list[[2]], 4)
  # Combine the fit models as a single step
  expect_s3_class(target_list[[3]], "tar_stem")
  expect_equal(target_list[[3]]$settings$name, "foo")

  # Verify the expression for collation is generated correctly
  collating_call <- target_list[[3]]$command$expr[[1]]
  expect_true(grepl(x = as.character(collating_call[[2]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_true(grepl(x = as.character(collating_call[[3]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_equal(names(collating_call), c("", "my first model", "my second model"))

  # Verify the targets created are the ones being collated
  expect_equal(collating_call[[2]], as.name(target_list[[1]][[4]]$settings$name))
  expect_equal(collating_call[[3]], as.name(target_list[[2]][[4]]$settings$name))
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
  target_list <-
    tar_nlmixr_multimodel(
      all_models,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      "Base one-compartment model; IIV in clearance and volume; additive residual error" = pheno,
      "Base one-compartment model; IIV in clearance and volume; additive residual error (estimate starting at 3)" = pheno2
    )
  expect_true(inherits(target_list, "list"))
})

test_that("tar_nlmixr_multimodel works with initial condition setting `central(0) <- 0`", {
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
      central(0) <- 0
      cp <- central/vc
      cp ~ add(cpaddSd)
    })
  }

  target_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model" = pheno
    )
  expect_s3_class(pheno, "rxUi")
})

targets::tar_test("tar_nlmixr_multimodel works with initial condition setting `central(0) <- 0`, running the targets", {
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
        central(0) <- 0
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }

    target_list <-
      tar_nlmixr_multimodel(
        name = foo, data = nlmixr2data::pheno_sd, est = "saem",
        "my first model" = pheno
      )
  })
  # This is really testing that there was no error when running the targets due
  # to the `central(0) <- 0` line
  expect_type(targets::tar_outdated(callr_function = NULL), "character")
})

test_that("tar_nlmixr_multimodel works for within-list model piping (#19), direct testing", {
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

  target_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model" = pheno,
      "my second model" = foo[["my first model"]] |> rxode2::ini(lcl = log(0.01))
    )
  expect_true(inherits(target_list, "list"))
  # One for each model and then one for combining everything
  expect_length(target_list, 3)
  # Data and object simplification, then the fitting
  expect_length(target_list[[1]], 4)
  # Data and object simplification, then the fitting
  expect_length(target_list[[2]], 4)
  # Combine the fit models as a single step
  expect_s3_class(target_list[[3]], "tar_stem")
  expect_equal(target_list[[3]]$settings$name, "foo")

  # Verify the expression for collation is generated correctly
  collating_call <- target_list[[3]]$command$expr[[1]]
  expect_true(grepl(x = as.character(collating_call[[2]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_true(grepl(x = as.character(collating_call[[3]]), pattern = "^foo_[0-9a-f]{8}$"))
  expect_equal(names(collating_call), c("", "my first model", "my second model"))

  # Verify the dependent target is created correctly
  expect_true(rxode2::.matchesLangTemplate(
    x = target_list[[2]]$object_simple$command$expr[[1]],
    template =
      str2lang(sprintf(
        "nlmixr_object_simplify(object = rxode2::ini(%s, lcl = log(0.01)))",
        target_list[[1]]$fit_simple$settings$name
      ))
  ))
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_nlmixr_multimodel works for within-list model piping (#19), testing via target creation", {
  targets::tar_script({
    # TODO: Remove load_all
    devtools::load_all("c:/git/nlmixr2/nlmixr2targets/")
    #library(nlmixr2targets)
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

    target_list <-
      tar_nlmixr_multimodel(
        name = foo, data = nlmixr2data::pheno_sd, est = "saem",
        "my first model" = pheno,
        "my second model" = foo[["my first model"]] |> rxode2::ini(lcl = log(0.01))
      )
  })
  dependencies <- targets::tar_network()$edges
  # There is one fitsimple object (estimated model result) that generates an
  # osimple (prepared model) object
  expect_equal(
    sum(
      grepl(x = dependencies$from, pattern = "foo_.{8}_fitsimple") &
        grepl(x = dependencies$to, pattern = "foo_.{8}_osimple")
    ),
    1
  )
})
