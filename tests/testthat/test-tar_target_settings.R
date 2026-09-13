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
      "deployment", "resources", "storage", "retrieval", "cue"
    )
  )
})

test_that("every forwarded name is a real tar_target argument", {
  expect_true(
    all(
      nlmixr2targets:::tar_nlmixr_target_setting_names %in%
        names(formals(targets::tar_target_raw))
    )
  )
})

test_that("priority is not forwarded, because targets deprecated it", {
  expect_false("priority" %in% nlmixr2targets:::tar_nlmixr_target_setting_names)
  expect_false("priority" %in% names(formals(nlmixr2targets::tar_nlmixr)))
  expect_false("priority" %in% names(formals(nlmixr2targets::tar_nlmixr_multimodel)))
})

test_that("the defaults match what tar_target itself would use", {
  defaults <- nlmixr2targets:::tar_nlmixr_collect_target_settings_default()
  expect_identical(names(defaults), nlmixr2targets:::tar_nlmixr_target_setting_names)
  expect_identical(defaults$resources, targets::tar_option_get("resources"))
  expect_identical(defaults$deployment, targets::tar_option_get("deployment"))
  expect_identical(defaults$garbage_collection, isTRUE(targets::tar_option_get("garbage_collection")))
})

test_that("resources reach every target tar_nlmixr() generates", {
  resources <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
  targets_out <-
    tar_nlmixr(
      name = pheno_settings_model,
      object = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources = resources
    )
  expect_length(targets_out, 4L)
  for (target in targets_out) {
    expect_identical(target$settings$resources, resources)
  }
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
      resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
    )
  expect_setequal(targets_out$object_simple$command$packages, c("nlmixr2targets", "nlmixr2est"))
  expect_setequal(targets_out$data_simple$command$packages, c("nlmixr2targets", "nlmixr2est"))
  expect_identical(targets_out$fit_simple$command$packages, "nlmixr2est")
  expect_identical(targets_out$fit$command$packages, "nlmixr2targets")
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
      resources = targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
    )
  expect_identical(
    plain$fit_simple$command$string,
    with_resources$fit_simple$command$string
  )
})

test_that("resources reach every target tar_nlmixr_multimodel() generates", {
  resources <- targets::tar_resources(crew = targets::tar_resources_crew(controller = "remote"))
  targets_out <-
    tar_nlmixr_multimodel(
      name = pheno_settings_multi,
      "first" = pheno_settings,
      data = nlmixr2data::pheno_sd,
      est = "saem",
      resources = resources
    )
  # one four-target set per model, plus the combined list target
  flat <- unlist(targets_out, recursive = TRUE)
  for (target in flat) {
    expect_identical(target$settings$resources, resources)
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
  for (target in targets_out) {
    expect_identical(target$settings$resources, targets::tar_option_get("resources"))
    expect_identical(target$settings$deployment, targets::tar_option_get("deployment"))
  }
})
