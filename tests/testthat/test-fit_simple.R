# Tests for the estimation command (R/fit_simple.R).

test_that("nlmixr_fit_simple forwards the object to nlmixr2est::nlmixr", {
  obj <- list(marker = "the-forwarded-object")

  captured <- NULL
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control, table) {
      captured <<- list(
        object = object, data = data, est = est, control = control,
        tableMissing = missing(table),
        table = if (missing(table)) NULL else table
      )
      "fit-result"
    },
    .package = "nlmixr2est"
  )

  ret <- nlmixr_fit_simple(object = obj, data = mtcars, est = "saem", control = list(n = 1))
  expect_identical(ret, "fit-result")
  expect_identical(captured$object, obj)
  expect_identical(captured$data, mtcars)
  expect_identical(captured$est, "saem")
  expect_identical(captured$control, list(n = 1))
  # An omitted `table` must stay omitted rather than becoming an explicit
  # tableControl(): nlmixr2est merges ui$meta's addDosing/subsetNonmem/cores/
  # keep/drop into the table control only when its `table` argument is
  # missing, so supplying a default would switch that merge off.
  expect_true(captured$tableMissing)

  # A supplied table reaches nlmixr2est::nlmixr() unchanged.  Before this was
  # fixed, tar_nlmixr()/tar_nlmixr_multimodel() accepted `table`, used it to
  # pick the retained data columns, and then dropped it before estimation.
  ret2 <- nlmixr_fit_simple(
    object = obj, data = mtcars, est = "saem", control = list(n = 1),
    table = nlmixr2est::tableControl(keep = "WT")
  )
  expect_identical(ret2, "fit-result")
  expect_false(captured$tableMissing)
  expect_identical(captured$table, nlmixr2est::tableControl(keep = "WT"))
})

# Issue #35: error = "continue" should catch an estimation failure and return
# a failure sentinel instead of propagating the error.
test_that("nlmixr_fit_simple returns a failure sentinel when error = 'continue'", {
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control, table) stop("synthetic estimation failure"),
    .package = "nlmixr2est"
  )

  ret <- nlmixr_fit_simple(
    object = list(marker = "x"), data = mtcars, est = "saem",
    control = list(), error = "continue"
  )
  expect_s3_class(ret, "nlmixr2targetsError")
  expect_s3_class(ret, "try-error")
  # A failed fit is clearly not an nlmixr2 object.
  expect_false(inherits(ret, "nlmixr2FitCore"))
  # The error message and the originating condition are preserved.
  expect_match(as.character(ret), "synthetic estimation failure", fixed = TRUE)
  expect_s3_class(attr(ret, "condition"), "condition")
})

# error = "stop" (the default) must let the error propagate so tar_make()
# halts exactly as it did before this option existed.
test_that("nlmixr_fit_simple re-throws when error = 'stop' (the default)", {
  obj <- list(marker = "x")
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control, table) stop("synthetic estimation failure"),
    .package = "nlmixr2est"
  )

  expect_error(
    nlmixr_fit_simple(object = obj, data = mtcars, est = "saem", control = list()),
    regexp = "synthetic estimation failure", fixed = TRUE
  )
  # Explicit error = "stop" behaves the same as the default.
  expect_error(
    nlmixr_fit_simple(
      object = obj, data = mtcars, est = "saem", control = list(), error = "stop"
    ),
    regexp = "synthetic estimation failure", fixed = TRUE
  )
})

# When a prior step failed under error = "continue", the sentinel reaches the
# fit command in place of the simplified model or the simplified data. It must
# be passed straight through: there is nothing to fit.
test_that("nlmixr_fit_simple passes an upstream failure sentinel through", {
  sentinel <- nlmixr2targets_error_object(simpleError("upstream step failed"))
  # A sentinel object short-circuits before the fit.
  expect_identical(
    nlmixr_fit_simple(object = sentinel, data = mtcars, est = "saem", control = list()),
    sentinel
  )
  # A sentinel in the data position does the same.
  expect_identical(
    nlmixr_fit_simple(object = list(marker = "x"), data = sentinel, est = "saem", control = list()),
    sentinel
  )
})

# tar_nlmixr_multimodel() names its targets after a hash of the model, so the
# announcement is the only way to tell which model tar_make() is running.
test_that("nlmixr_fit_simple announces the model description before estimating", {
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control, table) {
      message("estimating")
      "fit"
    },
    .package = "nlmixr2est"
  )

  expect_message(
    nlmixr_fit_simple(
      object = list(marker = "x"), data = mtcars, est = "saem", control = list(),
      description = "my first model"
    ),
    regexp = "Model description: my first model", fixed = TRUE
  )
  # The announcement comes before estimation starts, not after it finishes.
  messages <-
    conditionMessage(
      testthat::capture_message(
        nlmixr_fit_simple(
          object = list(marker = "x"), data = mtcars, est = "saem", control = list(),
          description = "my first model"
        )
      )
    )
  expect_identical(messages, "Model description: my first model\n")

  # No description (e.g. from tar_nlmixr()) announces nothing.
  quiet <-
    testthat::capture_messages(
      nlmixr_fit_simple(object = list(marker = "x"), data = mtcars, est = "saem", control = list())
    )
  expect_identical(quiet, "estimating\n")
})

test_that("nlmixr_fit_simple rejects an unknown error mode", {
  expect_error(
    nlmixr_fit_simple(
      object = list(marker = "x"), data = mtcars, est = "saem", control = list(),
      error = "nonsense"
    ),
    regexp = "should be one of"
  )
})

test_that("the failure sentinel prints its captured message", {
  sentinel <-
    nlmixr2targets_error_object(simpleError("model blew up"))
  expect_output(print(sentinel), "failed estimation", fixed = TRUE)
  expect_output(print(sentinel), "model blew up", fixed = TRUE)
  # print() returns its argument invisibly. capture.output() keeps the
  # printed lines from leaking into the test log while we check visibility.
  vis <- NULL
  invisible(capture.output(vis <- withVisible(print(sentinel))))
  expect_identical(vis$value, sentinel)
  expect_false(vis$visible)
})

# nlmixr2est returns the bare fit environment (class "nlmixr2FitCore") rather
# than the nlmixr2FitData tibble whenever the residual/table step did not run --
# either because calcTables = FALSE was asked for, or because addTable() failed
# and nlmixr2est downgraded that to a warning.  Either way nlmixr_fit_simple()
# passes the estimation result through untouched; deciding what a missing table
# step means is not this layer's job.
test_that("nlmixr_fit_simple returns a fit with no table step unchanged", {
  untabled <- new.env(parent = emptyenv())
  untabled$control <- list(calcTables = FALSE)
  class(untabled) <- "nlmixr2FitCore"
  testthat::local_mocked_bindings(
    nlmixr = function(object, data, est, control, table) untabled,
    .package = "nlmixr2est"
  )
  ret <- nlmixr_fit_simple(object = list(marker = "x"), data = mtcars, est = "saem", control = list())
  expect_identical(ret, untabled)
  expect_identical(class(ret), "nlmixr2FitCore")
})
