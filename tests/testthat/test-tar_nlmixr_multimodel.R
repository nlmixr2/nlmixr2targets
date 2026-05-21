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
  expect_type(pheno, "closure")
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
        central(initial) <- 0
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

  # Verify that circular references are caught
  expect_error(
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model" = pheno,
      "my second model" = foo[["my third model"]] |> rxode2::ini(lcl = log(0.01)),
      "my third model" = foo[["my second model"]] |> rxode2::ini(lcl = log(0.1))
    ),
    regexp = 'The following model\\(s\\) appear to have circular references to each other: "my second model", "my third model"'
  )

  # Verify that sequential references work
  target_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model" = pheno,
      "my second model" = foo[["my first model"]] |> rxode2::ini(lcl = log(0.01)),
      "my third model" = foo[["my second model"]] |> rxode2::ini(lcl = log(0.1))
    )
  expect_equal(length(target_list), 4)
  # The second model depends on the first
  expect_true(
    target_list[[1]]$fit_simple$settings$name %in%
      targets::tar_deps_raw(target_list[[2]]$object_simple$command$expr)
  )
  # The third model depends on the second
  expect_true(
    target_list[[2]]$fit_simple$settings$name %in%
      targets::tar_deps_raw(target_list[[3]]$object_simple$command$expr)
  )
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_nlmixr_multimodel works for within-list model piping (#19), testing via target creation", {
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

    target_list <-
      tar_nlmixr_multimodel(
        name = foo, data = nlmixr2data::pheno_sd, est = "saem",
        "my first model" = pheno,
        "my second model" = foo[["my first model"]] |> rxode2::ini(lcl = log(0.01))
      )
  })
  dependencies <- targets::tar_network()$edges
  # There is one fit_simple object (estimated model result) that generates an
  # object_simple (prepared model) object
  expect_equal(
    sum(
      grepl(x = dependencies$from, pattern = "foo_.{8}_fit_simple") &
        grepl(x = dependencies$to, pattern = "foo_.{8}_object_simple")
    ),
    1
  )
})

test_that("tar_nlmixr_multimodel_has_self_reference_single recognizes references", {
  # direct match: name[["..."]]
  expect_true(tar_nlmixr_multimodel_has_self_reference_single(quote(foo[["A"]]), name = "foo"))
  # nested via pipe / function call
  expect_true(tar_nlmixr_multimodel_has_self_reference_single(
    quote(foo[["A"]] |> rxode2::ini(x = 1)), name = "foo"
  ))
  # different list name -> not a match
  expect_false(tar_nlmixr_multimodel_has_self_reference_single(quote(bar[["A"]]), name = "foo"))
  # plain symbol (closure) -> not a match
  expect_false(tar_nlmixr_multimodel_has_self_reference_single(quote(my_model), name = "foo"))
  # atomic input -> not a match
  expect_false(tar_nlmixr_multimodel_has_self_reference_single(42, name = "foo"))
})

test_that("tar_nlmixr_multimodel_has_self_reference vectorizes over a list", {
  ml <- list(
    A = quote(my_model),
    B = quote(foo[["A"]] |> rxode2::ini(x = 1))
  )
  out <- tar_nlmixr_multimodel_has_self_reference(model_list = ml, name = "foo")
  expect_equal(out, c(A = FALSE, B = TRUE))
})

test_that("tar_nlmixr_multimodel_remove_self_reference_single rewrites references", {
  name_map <- c("foo[['A']]" = "foo_aaaaaaaa")
  # direct rewrite
  out <- tar_nlmixr_multimodel_remove_self_reference_single(
    model = quote(foo[["A"]]), name_map = name_map
  )
  expect_equal(out, quote(foo_aaaaaaaa_fit_simple))
  # nested rewrite (inside a pipe)
  out2 <- tar_nlmixr_multimodel_remove_self_reference_single(
    model = quote(foo[["A"]] |> rxode2::ini(x = 1)), name_map = name_map
  )
  expect_true(rxode2::.matchesLangTemplate(
    x = out2,
    template = str2lang("foo_aaaaaaaa_fit_simple |> rxode2::ini(x = 1)")
  ))
  # symbol input is returned unchanged (length <= 1 short-circuit)
  expect_equal(
    tar_nlmixr_multimodel_remove_self_reference_single(quote(my_model), name_map),
    quote(my_model)
  )
  # NULL is returned unchanged
  expect_null(tar_nlmixr_multimodel_remove_self_reference_single(NULL, name_map))
})

test_that("tar_nlmixr_multimodel_single returns hash-suffixed target name", {
  out <- tar_nlmixr_multimodel_single(
    object = quote(my_model),
    name = "foo",
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    env = environment()
  )
  expect_named(out, c("target", "name"))
  expect_match(out$name, "^foo_[0-9a-f]{8}$")
  expect_named(out$target, c("object_simple", "data_simple", "fit_simple", "fit"))
})

test_that("tar_nlmixr_multimodel_parse rejects bad name and env", {
  expect_error(
    tar_nlmixr_multimodel_parse(
      name = "", data = quote(d), est = "saem",
      control = quote(list()), table = quote(list()),
      model_list = list(A = quote(my_model)),
      env = environment()
    ),
    regexp = "name"
  )
  expect_error(
    tar_nlmixr_multimodel_parse(
      name = "foo", data = quote(d), est = "saem",
      control = quote(list()), table = quote(list()),
      model_list = list(A = quote(my_model)),
      env = "not an environment"
    ),
    regexp = "Must be an environment"
  )
})

test_that("tar_nlmixr_multimodel_parse rejects unnamed or duplicate-name model_list", {
  expect_error(
    tar_nlmixr_multimodel_parse(
      name = "foo", data = quote(d), est = "saem",
      control = quote(list()), table = quote(list()),
      model_list = list(quote(my_model)),
      env = environment()
    )
  )
  expect_error(
    tar_nlmixr_multimodel_parse(
      name = "foo", data = quote(d), est = "saem",
      control = quote(list()), table = quote(list()),
      model_list = list(A = quote(m1), A = quote(m2)),
      env = environment()
    )
  )
})
