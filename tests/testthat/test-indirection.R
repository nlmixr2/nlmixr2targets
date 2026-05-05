# Tests for the indirect cache layer (R/indirection.R).
#
# These functions normally store under targets::tar_config_get("store"),
# but accept a `directory` arg, so we can exercise them against a tmp
# directory without needing a full targets pipeline.

test_that("save_nlmixr2obj_indirect writes to <directory>/<md5> and returns md5", {
  tmp <- withr::local_tempdir()
  obj <- list(md5 = "abc123", payload = "hello")
  ret <- save_nlmixr2obj_indirect(obj, directory = tmp)
  expect_identical(ret, "abc123")
  expect_true(file.exists(file.path(tmp, "abc123")))
})

test_that("save_nlmixr2obj_indirect creates the directory if missing", {
  tmp <- withr::local_tempdir()
  nested <- file.path(tmp, "deep", "nested", "dir")
  obj <- list(md5 = "deadbeef", payload = 42L)
  expect_false(dir.exists(nested))
  save_nlmixr2obj_indirect(obj, directory = nested)
  expect_true(dir.exists(nested))
  expect_true(file.exists(file.path(nested, "deadbeef")))
})

test_that("read_nlmixr2obj_indirect round-trips a saved object", {
  tmp <- withr::local_tempdir()
  obj <- list(md5 = "h", values = 1:5)
  save_nlmixr2obj_indirect(obj, directory = tmp)
  loaded <- read_nlmixr2obj_indirect(hash = "h", directory = tmp)
  expect_identical(loaded, obj)
})

test_that("read_nlmixr2obj_indirect errors clearly on cache miss", {
  tmp <- withr::local_tempdir()
  expect_error(
    read_nlmixr2obj_indirect(hash = "missing_hash", directory = tmp),
    regexp = "indirect cache miss",
    fixed = TRUE
  )
  expect_error(
    read_nlmixr2obj_indirect(hash = "missing_hash", directory = tmp),
    regexp = "missing_hash",
    fixed = TRUE
  )
})

test_that("read_nlmixr2obj_indirect rejects bad hashes", {
  tmp <- withr::local_tempdir()
  expect_error(read_nlmixr2obj_indirect(hash = NULL,    directory = tmp))
  expect_error(read_nlmixr2obj_indirect(hash = "",      directory = tmp))
  expect_error(read_nlmixr2obj_indirect(hash = NA,      directory = tmp))
  expect_error(read_nlmixr2obj_indirect(hash = c("a", "b"), directory = tmp))
})

# tar_test() cd's into an isolated tempdir and resets targets config on
# exit, so the default `tar_config_get("store")` resolves to that dir.
targets::tar_test("nlmixr2_indirect loads from cache and forwards to nlmixr2est::nlmixr", {
  obj <- list(md5 = "hh", marker = "the-loaded-object")
  # Save under the default cache location relative to tar_config_get("store")
  save_nlmixr2obj_indirect(obj)

  captured <- NULL
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control) {
      captured <<- list(object = object, data = data, est = est, control = control)
      "fit-result"
    },
    .package = "nlmixr2est"
  )

  ret <- nlmixr2_indirect(object = "hh", data = mtcars, est = "saem", control = list(n = 1))
  expect_identical(ret, "fit-result")
  expect_identical(captured$object, obj)
  expect_identical(captured$data, mtcars)
  expect_identical(captured$est, "saem")
  expect_identical(captured$control, list(n = 1))
})
