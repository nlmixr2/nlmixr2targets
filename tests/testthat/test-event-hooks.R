skip_if_not(exists("rxEventEmit", envir = asNamespace("rxode2"), inherits = FALSE),
            "rxode2 has no event bus")

test_that("a tar_nlmixr() model emits fitUpdate(ui) then assign with the target name", {
  skip_on_cran()
  .rec <- new.env()
  .rec$ev <- list()
  rxode2::rxEventListen("nlmixr2targets-test", function(event, ...) {
    .rec$ev[[length(.rec$ev) + 1L]] <- list(event = event, p = list(...))
  })
  withr::defer(rxode2::rxEventUnlisten("nlmixr2targets-test"))
  targets::tar_dir({
    targets::tar_script({
      library(nlmixr2targets)
      pheno <- function() {
        ini({
          lcl <- log(0.008); label("Typical value of clearance")
          lvc <- log(0.6)
          etalcl ~ 1
          cpaddSd <- 0.1
        })
        model({
          cl <- exp(lcl + etalcl)
          vc <- exp(lvc)
          kel <- cl / vc
          d/dt(central) <- -kel * central
          cp <- central / vc
          cp ~ add(cpaddSd)
        })
      }
      list(tar_nlmixr(name = pheno_model, object = pheno,
                      data = nlmixr2data::pheno_sd, est = "posthoc"))
    }, ask = FALSE)
    suppressMessages(suppressWarnings(
      targets::tar_make(callr_function = NULL, reporter = "silent")
    ))
  })
  .events <- vapply(.rec$ev, function(e) e$event, character(1))
  expect_identical(tail(.events, 2L), c("fitUpdate", "assign"))
  .up <- .rec$ev[[length(.events) - 1L]]$p
  expect_identical(.up$what, "ui")
  expect_true(isTRUE(.up$inPlace))
  .as <- .rec$ev[[length(.events)]]$p
  expect_identical(.as$name, "pheno_model")
  expect_s3_class(.as$value, "nlmixr2FitCore")
  ## the final fit carries the restored label
  expect_true("Typical value of clearance" %in% .as$value$ui$iniDf$label)
  ## the fit itself was estimated (and announced) by the *_fit_simple target
  expect_true("fitComplete" %in% .events)
})

test_that("outside a target only fitUpdate is emitted", {
  .rec <- new.env()
  .rec$ev <- character(0)
  rxode2::rxEventListen("nlmixr2targets-test", function(event, ...) {
    .rec$ev <- c(.rec$ev, event)
  })
  withr::defer(rxode2::rxEventUnlisten("nlmixr2targets-test"))
  fake <- structure(list(env = new.env()), class = c("nlmixr2FitCore", "list"))
  nlmixr2targets_event_fit(fake)
  expect_identical(.rec$ev, "fitUpdate")
  nlmixr2targets_event_fit(list(env = new.env()))
  expect_identical(.rec$ev, "fitUpdate")
})

test_that("the logger is loaded only when the project configured one", {
  withr::local_dir(withr::local_tempdir())
  withr::local_envvar(NLMIXR2LOG_CONFIG = "")
  expect_false(nlmixr2targets_event_load_logger())
  file.create("_nlmixr2log.rds")
  ## TRUE only if nlmixr2log is installed; never an error either way
  expect_identical(nlmixr2targets_event_load_logger(),
                   requireNamespace("nlmixr2log", quietly = TRUE))
})
