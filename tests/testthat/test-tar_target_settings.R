# The generators forward a fixed set of targets::tar_target() settings to every
# target they build.  These tests pin the set, the defaults, and that each
# generated target actually receives them.

pheno_settings <- function() {
  ini({
    lcl <- log(0.008)
    lvc <- log(0.6)
    cpaddSd <- 0.1
  })
  model({
    cl <- exp(lcl)
    vc <- exp(lvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

test_that("the forwarded settings are exactly the tar_target arguments we intend", {
  expect_identical(
    nlmixr2targets:::tar_nlmixr_target_setting_names,
    c(
      "format", "repository", "library", "memory", "garbage_collection",
      "deployment", "resources_data", "resources_fit", "storage", "retrieval",
      "cue"
    )
  )
})

test_that("every forwarded name is a real tar_target argument, with resources split in two", {
  setting_names <- nlmixr2targets:::tar_nlmixr_target_setting_names
  resources_names <- c("resources_data", "resources_fit")
  expect_true(all(resources_names %in% setting_names))
  expect_true(
    all(setdiff(setting_names, resources_names) %in% names(formals(targets::tar_target_raw)))
  )
  expect_true("resources" %in% names(formals(targets::tar_target_raw)))
})

test_that("both generators take every forwarded setting and no plain resources", {
  for (generator in list(nlmixr2targets::tar_nlmixr, nlmixr2targets::tar_nlmixr_multimodel)) {
    generator_args <- names(formals(generator))
    expect_true(all(nlmixr2targets:::tar_nlmixr_target_setting_names %in% generator_args))
    expect_false("resources" %in% generator_args)
  }
})

test_that("priority is not forwarded, because targets deprecated it", {
  expect_false("priority" %in% nlmixr2targets:::tar_nlmixr_target_setting_names)
  expect_false("priority" %in% names(formals(nlmixr2targets::tar_nlmixr)))
  expect_false("priority" %in% names(formals(nlmixr2targets::tar_nlmixr_multimodel)))
})

test_that("the defaults match what tar_target itself would use", {
  defaults <- nlmixr2targets:::tar_nlmixr_collect_target_settings_default()
  expect_identical(names(defaults), nlmixr2targets:::tar_nlmixr_target_setting_names)
  expect_identical(defaults$resources_data, targets::tar_option_get("resources"))
  expect_identical(defaults$resources_fit, targets::tar_option_get("resources"))
  expect_identical(defaults$deployment, targets::tar_option_get("deployment"))
  expect_identical(defaults$garbage_collection, isTRUE(targets::tar_option_get("garbage_collection")))
})

test_that("tar_option_set(resources = ) reaches every target when neither resources_* is given", {
  resources <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "from_options"))
  targets::tar_option_set(resources = resources)
  on.exit(targets::tar_option_reset(), add = TRUE)
  targets_out <-
    tar_nlmixr(
      name = pheno_settings_model_opt,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem"
    )
  for (target in targets_out) {
    expect_identical(target$settings$resources, resources)
  }
})

test_that("resources_data reaches only the data target and resources_fit every other tar_nlmixr() target", {
  resources_data <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "local"))
  resources_fit <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
  targets_out <-
    tar_nlmixr(
      name = pheno_settings_model,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources_data = resources_data,
      resources_fit = resources_fit
    )
  expect_named(targets_out, c("object_simple", "covariates", "data_simple", "fit_simple", "fit"))
  expect_identical(targets_out$data_simple$settings$resources, resources_data)
  for (nm in c("object_simple", "covariates", "fit_simple", "fit")) {
    expect_identical(targets_out[[nm]]$settings$resources, resources_fit, label = nm)
  }
})

test_that("a plain resources argument is not accepted", {
  resources <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
  # It partially matches both resources_* arguments.
  expect_error(
    tar_nlmixr(
      name = pheno_settings_model,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources = resources
    ),
    regexp = "matches multiple formal arguments",
    fixed = TRUE
  )
  # Arguments after `...` do not partially match, so without the explicit
  # check it would be collected as a model named "resources".
  expect_error(
    tar_nlmixr_multimodel(
      name = pheno_settings_multi,
      "first" = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources = resources
    ),
    regexp = "use `resources_data` and `resources_fit` instead",
    fixed = TRUE
  )
})

test_that("tar_nlmixr_raw rejects target_settings that do not name exactly the forwarded settings", {
  raw_args <-
    list(
      name = "raw_settings_model",
      object = quote(pheno_settings),
      data = quote(nlmixr2data::pheno_sd),
      est = "saem",
      control = quote(list()),
      table = quote(nlmixr2est::tableControl()),
      object_simple_name = "raw_settings_object_simple",
      data_simple_name = "raw_settings_data_simple",
      fit_simple_name = "raw_settings_fit_simple",
      env = environment()
    )
  defaults <- nlmixr2targets:::tar_nlmixr_collect_target_settings_default()
  # A plain `resources` in place of the split pair would otherwise be dropped.
  unsplit <- defaults[setdiff(names(defaults), c("resources_data", "resources_fit"))]
  unsplit$resources <- targets::tar_option_get("resources")
  expect_error(
    do.call(nlmixr2targets::tar_nlmixr_raw, c(raw_args, list(target_settings = unsplit))),
    regexp = "target_settings"
  )
  missing_one <- defaults[setdiff(names(defaults), "cue")]
  expect_error(
    do.call(nlmixr2targets::tar_nlmixr_raw, c(raw_args, list(target_settings = missing_one))),
    regexp = "target_settings"
  )
})

test_that("other forwarded settings reach every generated target", {
  targets_out <-
    tar_nlmixr(
      name = pheno_settings_model2,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      deployment = "main",
      memory = "transient",
      storage = "main",
      retrieval = "main"
    )
  for (target in targets_out) {
    expect_identical(target$settings$deployment, "main")
    expect_identical(target$settings$memory, "transient")
    expect_identical(target$settings$storage, "main")
    expect_identical(target$settings$retrieval, "main")
  }
})

test_that("the per-target packages choices survive forwarding", {
  targets_out <-
    tar_nlmixr(
      name = pheno_settings_model3,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources_data = targets::tar_resources(crew = targets::tar_resources_crew(controller = "local")),
      resources_fit = targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
    )
  for (target in targets_out) {
    expect_identical(target$command$packages, c("nlmixr2targets", "nlmixr2est"))
  }
})

test_that("forwarding does not change the estimation command, so cached fits stand", {
  plain <-
    tar_nlmixr(
      name = pheno_settings_model4,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem"
    )
  with_resources <-
    tar_nlmixr(
      name = pheno_settings_model4,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources_data = targets::tar_resources(crew = targets::tar_resources_crew(controller = "local")),
      resources_fit = targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
    )
  for (nm in names(plain)) {
    expect_identical(plain[[nm]]$command$string, with_resources[[nm]]$command$string, label = nm)
  }
})

test_that("resources_data and resources_fit reach the right tar_nlmixr_multimodel() targets", {
  resources_data <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "local"))
  resources_fit <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
  targets_out <-
    tar_nlmixr_multimodel(
      name = pheno_settings_multi,
      "first" = pheno_settings,
      "second" = pheno_settings |> ini(lcl = log(0.01)),
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources_data = resources_data,
      resources_fit = resources_fit
    )
  # one five-target set per model, plus the combined list target
  flat <- unlist(targets_out, recursive = TRUE)
  expect_length(flat, 11L)
  target_names <- vapply(flat, \(x) x$settings$name, "")
  is_data <- endsWith(target_names, "_data_simple")
  expect_identical(sum(is_data), 2L)
  for (target in flat[is_data]) {
    expect_identical(target$settings$resources, resources_data, label = target$settings$name)
  }
  # the combined list target is among these
  expect_true("pheno_settings_multi" %in% target_names[!is_data])
  for (target in flat[!is_data]) {
    expect_identical(target$settings$resources, resources_fit, label = target$settings$name)
  }
})

test_that("tar_nlmixr_raw() called directly still defaults as before", {
  targets_out <-
    nlmixr2targets::tar_nlmixr_raw(
      name = "raw_settings_model",
      object = quote(pheno_settings),
      data = quote(nlmixr2data::pheno_sd),
      est = "saem",
      control = quote(list()),
      table = quote(nlmixr2est::tableControl()),
      object_simple_name = "raw_settings_object_simple",
      data_simple_name = "raw_settings_data_simple",
      fit_simple_name = "raw_settings_fit_simple",
      env = environment()
    )
  expect_named(targets_out, c("object_simple", "covariates", "data_simple", "fit_simple", "fit"))
  expect_identical(targets_out$covariates$settings$name, "raw_settings_model_covariates")
  for (target in targets_out) {
    expect_identical(target$settings$resources, targets::tar_option_get("resources"))
    expect_identical(target$settings$deployment, targets::tar_option_get("deployment"))
  }
})
