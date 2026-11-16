# The old indirect cache ----
#
# A store written before the cache was removed records every fit against
# commands and dependency hashes this version no longer produces, so building
# on top of it re-runs every fit.  The check fires while `_targets.R` is being
# sourced, which is the last moment before targets starts comparing hashes.

test_that("the cache check passes when there is no cache", {
  directory <- withr::local_tempdir()
  expect_silent(nlmixr2targets_assert_no_indirect_cache(store = directory))
  expect_null(nlmixr2targets_assert_no_indirect_cache(store = directory))
})

test_that("the cache check refuses a store that still holds the cache", {
  store <- withr::local_tempdir()
  directory <- file.path(store, "user", "nlmixr2")
  dir.create(directory, recursive = TRUE)
  expect_error(
    nlmixr2targets_assert_no_indirect_cache(store = store),
    class = "nlmixr2targets_indirect_cache"
  )
  # The message has to name the directory, say what would happen, and give both
  # ways out, because it is the only place a user meets this.
  message <- tryCatch(
    nlmixr2targets_assert_no_indirect_cache(store = store),
    nlmixr2targets_indirect_cache = conditionMessage
  )
  expect_match(message, directory, fixed = TRUE)
  expect_match(message, "re-run every fit")
  expect_match(message, "convert-indirect-cache.R", fixed = TRUE)
  expect_match(message, "unlink(", fixed = TRUE)
  expect_match(
    message,
    "options(nlmixr2targets.allow_indirect_cache = TRUE)",
    fixed = TRUE
  )
})

test_that("the cache check stands down when the option is set", {
  store <- withr::local_tempdir()
  dir.create(file.path(store, "user", "nlmixr2"), recursive = TRUE)
  withr::local_options(nlmixr2targets.allow_indirect_cache = TRUE)
  expect_null(nlmixr2targets_assert_no_indirect_cache(store = store))
})

test_that("the cache check ignores an unrelated user directory", {
  store <- withr::local_tempdir()
  dir.create(file.path(store, "user", "something_else"), recursive = TRUE)
  expect_null(nlmixr2targets_assert_no_indirect_cache(store = store))
})

targets::tar_test("tar_nlmixr_raw refuses to build over the old cache", {
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  model <- function() {
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
  # Both generators come through tar_nlmixr_raw(), so one check covers them.
  expect_error(
    tar_nlmixr(
      name = pheno,
      object = model,
      data = nlmixr2data::theo_sd,
      est = "focei"
    ),
    class = "nlmixr2targets_indirect_cache"
  )
})
