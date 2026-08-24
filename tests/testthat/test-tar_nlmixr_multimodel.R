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

# Issue #37: a model function carrying cmt(0) initial conditions is shared
# between a plain entry and a piped entry (`mod |> ini(...)`). Processing the
# plain entry mutates `mod` in env from cmt(0) to cmt(initial); the piped
# entry then rewrites nothing, so before the fix its command was left
# unwrapped and evaluating the pipe hit the nlmixr2-invalid `cmt(initial)`
# form. Both entries' object-simplification commands must therefore be wrapped
# in nlmixr_object_zero_initial_eval() so cmt(0) is restored at runtime.
test_that("tar_nlmixr_multimodel wraps piped entries that reference a shared cmt(0) model (#37)", {
  mod <- function() {
    ini({
      a <- 1
      addSd <- 0.1
    })
    model({
      b <- a
      d/dt(central) <- -b*central
      central(0) <- 3
      cp <- central
      cp ~ add(addSd)
    })
  }

  target_list <-
    tar_nlmixr_multimodel(
      name = fit_pipe, data = nlmixr2data::pheno_sd, est = "saem",
      "myfit" = mod,
      "myfit pipe" = mod |> rxode2::ini(a <- 2)
    )
  # Both object-simplification commands must defer evaluation through
  # nlmixr_object_zero_initial_eval() (order of the two model entries is
  # irrelevant: whichever is processed second is the one previously left
  # unwrapped).
  is_wrapped <- function(single_target) {
    obj_arg <- single_target$object_simple$command$expr[[1]][["object"]]
    rxode2::.matchesLangTemplate(
      obj_arg,
      str2lang("nlmixr2targets::nlmixr_object_zero_initial_eval(.)")
    )
  }
  expect_true(is_wrapped(target_list[[1]]))
  expect_true(is_wrapped(target_list[[2]]))
})

# The `description` argument spliced into a model's nlmixr2_indirect() call,
# or NULL when the model does not announce itself.
fit_simple_description <- function(single_target) {
  single_target$fit_simple$command$expr[[1]][["description"]]
}

# Issue #37 x #19: a self-reference pipe (`fit_pipe[["myfit"]] |> ini(...)`)
# whose base model carries cmt(0) must NOT be wrapped in
# nlmixr_object_zero_initial_eval(). The self-reference resolves to the base
# model's `_fit_simple` *target* (a fitted object with cmt(0) already
# compiled in, so there is no cmt(initial) DSL to restore), and wrapping
# would bury that target name inside quote(), hiding it from targets'
# dependency graph. The base model itself (bare cmt(0) function) is still
# wrapped. This pins the boundary that separates "pipe over a source
# function" (wrap) from "pipe over a target result" (do not wrap).
test_that("tar_nlmixr_multimodel does not wrap a self-reference pipe over a cmt(0) model (#37/#19)", {
  mod <- function() {
    ini({
      a <- 1
      addSd <- 0.1
    })
    model({
      b <- a
      d/dt(central) <- -b*central
      central(0) <- 3
      cp <- central
      cp ~ add(addSd)
    })
  }

  target_list <-
    tar_nlmixr_multimodel(
      name = fit_pipe, data = nlmixr2data::pheno_sd, est = "saem",
      "myfit" = mod,
      "myfit pipe" = fit_pipe[["myfit"]] |> rxode2::ini(a <- 2)
    )
  is_wrapped <- function(single_target) {
    obj_arg <- single_target$object_simple$command$expr[[1]][["object"]]
    rxode2::.matchesLangTemplate(
      obj_arg,
      str2lang("nlmixr2targets::nlmixr_object_zero_initial_eval(.)")
    )
  }
  # Base model is wrapped (bare cmt(0) function).
  expect_true(is_wrapped(target_list[[1]]))
  # Self-reference pipe is NOT wrapped, and its base fit_simple target stays
  # a visible dependency.
  expect_false(is_wrapped(target_list[[2]]))
  expect_true(
    target_list[[1]]$fit_simple$settings$name %in%
      targets::tar_deps_raw(target_list[[2]]$object_simple$command$expr)
  )
  # Self-reference resolution rebuilds the affected targets; the descriptions
  # must survive that rebuild.
  expect_equal(fit_simple_description(target_list[[1]]), "myfit")
  expect_equal(fit_simple_description(target_list[[2]]), "myfit pipe")
})

# Without this, tar_make() reports only the hashed target names, which say
# nothing about which model is running.
test_that("tar_nlmixr_multimodel announces each model by its list name", {
  pheno <- function() {
    ini({
      lcl <- log(0.008)
      lvc <- log(0.6)
      cpaddSd <- 0.1
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      d/dt(central) <- -(cl/vc)*central
      cp <- central/vc
      cp ~ add(cpaddSd)
    })
  }
  pheno2 <- function() {
    ini({
      lcl <- log(0.008)
      lvc <- log(0.6)
      cpaddSd <- 3.0
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      d/dt(central) <- -(cl/vc)*central
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
  expect_equal(fit_simple_description(target_list[[1]]), "my first model")
  expect_equal(fit_simple_description(target_list[[2]]), "my second model")

  # Renaming the models must not re-run their fits: same target names, same
  # command hashes.
  renamed <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "my first model, renamed" = pheno,
      "my second model, renamed" = pheno2
    )
  expect_equal(fit_simple_description(renamed[[1]]), "my first model, renamed")
  for (idx in 1:2) {
    expect_identical(
      renamed[[idx]]$fit_simple$settings$name,
      target_list[[idx]]$fit_simple$settings$name
    )
    expect_identical(
      renamed[[idx]]$fit_simple$command$hash,
      target_list[[idx]]$fit_simple$command$hash
    )
  }
})

targets::tar_test("tar_nlmixr_multimodel announces the model when tar_make() runs it", {
  targets::tar_script({
    pheno <- function() {
      ini({
        lcl <- log(0.008)
        lvc <- log(0.6)
        etalcl ~ 1
        cpaddSd <- 0.1
      })
      model({
        cl <- exp(lcl + etalcl)
        vc <- exp(lvc)
        d/dt(central) <- -(cl/vc)*central
        cp <- central/vc
        cp ~ add(cpaddSd)
      })
    }
    nlmixr2targets::tar_nlmixr_multimodel(
      name = all_models, data = nlmixr2data::pheno_sd, est = "saem",
      control = nlmixr2est::saemControl(nBurn = 1, nEm = 1),
      "my first model" = pheno
    )
  })
  # `tar_test()` wraps its body in suppressMessages(), so capture with a
  # calling handler (which runs first) rather than by sinking the message
  # stream (which the outer muffle would have already emptied).
  output <-
    testthat::capture_messages(
      suppressWarnings(targets::tar_make(callr_function = NULL))
    )
  expect_true(any(grepl("Model description: my first model", output, fixed = TRUE)))
})

targets::tar_test("tar_nlmixr_multimodel fits a piped entry sharing a cmt(0) model end-to-end (#37)", {
  targets::tar_script({
    mod <- function() {
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

    nlmixr2targets::tar_nlmixr_multimodel(
      name = fit_pipe, data = nlmixr2data::pheno_sd, est = "saem",
      control = nlmixr2est::saemControl(nBurn = 1, nEm = 1),
      "myfit" = mod,
      "myfit pipe" = mod |> rxode2::ini(lcl = log(0.01))
    )
  })
  expect_no_error(targets::tar_outdated(callr_function = NULL))
  suppressWarnings(targets::tar_make(callr_function = NULL))
  fits <- targets::tar_read(fit_pipe)
  expect_named(fits, c("myfit", "myfit pipe"))
  expect_s3_class(fits[["myfit"]], "nlmixr2FitCore")
  expect_s3_class(fits[["myfit pipe"]], "nlmixr2FitCore")
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

  # Verify the dependent target is created correctly. The substituted
  # command now also threads `directory` through every call so the cache
  # path resolves against the user-chosen targets store at execution time.
  expect_true(rxode2::.matchesLangTemplate(
    x = target_list[[2]]$object_simple$command$expr[[1]],
    template =
      str2lang(sprintf(
        paste0(
          "nlmixr_object_simplify(object = rxode2::ini(%s, lcl = log(0.01)), ",
          "directory = file.path(targets::tar_config_get(\"store\"), \"user/nlmixr2\"))"
        ),
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

# Issue #35: the error mode must thread through the multimodel layers down to
# each model's fit_simple command.
test_that("tar_nlmixr_multimodel threads the error mode into every model's fit_simple command", {
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

  # Default is "stop".
  default_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      "m1" = pheno
    )
  cmd_default <- paste(deparse(default_list[[1]]$fit_simple$command$expr), collapse = " ")
  expect_match(cmd_default, 'error = "stop"', fixed = TRUE)

  continue_list <-
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      error = "continue",
      "m1" = pheno,
      "m2" = foo[["m1"]] |> rxode2::ini(lcl = log(0.01))
    )
  cmd1 <- paste(deparse(continue_list[[1]]$fit_simple$command$expr), collapse = " ")
  cmd2 <- paste(deparse(continue_list[[2]]$fit_simple$command$expr), collapse = " ")
  expect_match(cmd1, 'error = "continue"', fixed = TRUE)
  expect_match(cmd2, 'error = "continue"', fixed = TRUE)
})

test_that("tar_nlmixr_multimodel rejects an unknown error mode", {
  expect_error(
    tar_nlmixr_multimodel(
      name = foo, data = nlmixr2data::pheno_sd, est = "saem",
      error = "nope",
      "m1" = quote(my_model)
    ),
    regexp = "should be one of"
  )
})

test_that("tar_nlmixr_multimodel_single threads the error mode into the fit_simple command", {
  out <- tar_nlmixr_multimodel_single(
    object = quote(my_model),
    name = "foo",
    data = quote(my_data),
    est = "saem",
    control = quote(list()),
    table = quote(list()),
    env = environment(),
    error = "continue"
  )
  cmd <- paste(deparse(out$target$fit_simple$command$expr), collapse = " ")
  expect_match(cmd, 'error = "continue"', fixed = TRUE)
})

# `data`, `est`, `control`, and `table` are captured by `substitute()` and must
# reach the generated target commands as unevaluated language: `data` is
# normally the name of an upstream target, which does not exist as an object
# when the pipeline is built. Every other test in this file passes arguments
# that happen to resolve (`nlmixr2data::pheno_sd`), so they cannot tell a
# forwarded promise from an evaluated one. These use names that exist nowhere.
test_that("tar_nlmixr_multimodel does not evaluate data, control, or table", {
  pheno <- function() {
    ini({
      lcl <- log(0.008); label("Typical value of clearance")
      lvc <-  log(0.6); label("Typical value of volume of distribution")
      cpaddSd <- 0.1; label("residual variability")
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
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
      cpaddSd <- 3.0; label("residual variability")
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl/vc
      d/dt(central) <- -kel*central
      cp <- central/vc
      cp ~ add(cpaddSd)
    })
  }

  expect_false(exists("no_such_data_target"))
  expect_false(exists("no_such_control_fn"))
  expect_false(exists("no_such_table_fn"))

  target_list <-
    tar_nlmixr_multimodel(
      name = foo,
      data = no_such_data_target,
      est = "saem",
      control = no_such_control_fn(),
      table = no_such_table_fn(),
      "my first model" = pheno,
      "my second model" = pheno2
    )
  expect_length(target_list, 3)

  for (idx in 1:2) {
    data_cmd <- paste(deparse(target_list[[idx]]$data_simple$command$expr), collapse = " ")
    expect_match(data_cmd, "data = no_such_data_target", fixed = TRUE)
    expect_match(data_cmd, "table = no_such_table_fn()", fixed = TRUE)
    expect_match(data_cmd, "control = no_such_control_fn()", fixed = TRUE)

    fit_simple_cmd <- paste(deparse(target_list[[idx]]$fit_simple$command$expr), collapse = " ")
    expect_match(fit_simple_cmd, "control = no_such_control_fn()", fixed = TRUE)

    fit_cmd <- paste(deparse(target_list[[idx]]$fit$command$expr), collapse = " ")
    expect_match(fit_cmd, "data = no_such_data_target", fixed = TRUE)
  }

  # The description is still threaded to each model's estimation target.
  expect_match(
    paste(deparse(target_list[[1]]$fit_simple$command$expr), collapse = " "),
    'description = "my first model"',
    fixed = TRUE
  )
  expect_match(
    paste(deparse(target_list[[2]]$fit_simple$command$expr), collapse = " "),
    'description = "my second model"',
    fixed = TRUE
  )
})

test_that("tar_nlmixr_multimodel_prep forwards language arguments without evaluating them", {
  expect_false(exists("no_such_data_target"))
  expect_false(exists("no_such_control_fn"))

  ret <-
    tar_nlmixr_multimodel_prep(
      model_list = list(A = quote(my_model_a), B = quote(my_model_b)),
      name = "foo",
      data = quote(no_such_data_target),
      est = "saem",
      control = quote(no_such_control_fn()),
      table = quote(list()),
      env = environment()
    )
  expect_named(ret, c("A", "B"))
  expect_match(ret$A$name, "^foo_[0-9a-f]{8}$")
  expect_match(ret$B$name, "^foo_[0-9a-f]{8}$")
  # Distinct models hash to distinct target names.
  expect_false(ret$A$name == ret$B$name)
  expect_named(ret$A$target, c("object_simple", "data_simple", "fit_simple", "fit"))

  data_cmd <- paste(deparse(ret$A$target$data_simple$command$expr), collapse = " ")
  expect_match(data_cmd, "data = no_such_data_target", fixed = TRUE)
  expect_match(data_cmd, "control = no_such_control_fn()", fixed = TRUE)

  fit_simple_cmd <- paste(deparse(ret$A$target$fit_simple$command$expr), collapse = " ")
  expect_match(fit_simple_cmd, "control = no_such_control_fn()", fixed = TRUE)

  # Each model keeps its own list name as its description and its own model.
  expect_match(fit_simple_cmd, 'description = "A"', fixed = TRUE)
  expect_match(
    paste(deparse(ret$A$target$object_simple$command$expr), collapse = " "),
    "object = my_model_a",
    fixed = TRUE
  )
  expect_match(
    paste(deparse(ret$B$target$fit_simple$command$expr), collapse = " "),
    'description = "B"',
    fixed = TRUE
  )
  expect_match(
    paste(deparse(ret$B$target$object_simple$command$expr), collapse = " "),
    "object = my_model_b",
    fixed = TRUE
  )
})

test_that("tar_nlmixr_multimodel_prep threads the error mode to every model", {
  ret <-
    tar_nlmixr_multimodel_prep(
      model_list = list(A = quote(my_model_a), B = quote(my_model_b)),
      name = "foo",
      data = quote(no_such_data_target),
      est = "saem",
      control = quote(list()),
      table = quote(list()),
      env = environment(),
      error = "continue"
    )
  for (nm in c("A", "B")) {
    expect_match(
      paste(deparse(ret[[nm]]$target$fit_simple$command$expr), collapse = " "),
      'error = "continue"',
      fixed = TRUE
    )
  }
})
