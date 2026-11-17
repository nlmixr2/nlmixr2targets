# The conversion lives in inst/scripts/ rather than R/ because it needs
# secretbase for the hash targets uses, and secretbase is not an import of this
# package.  Sourcing the file from inside a test defines its functions without
# running its driver, which only fires at top level.
convert_source <- function() {
  path <-
    system.file(
      "scripts",
      "convert-indirect-cache.R",
      package = "nlmixr2targets"
    )
  testthat::skip_if(
    !nzchar(path) || !file.exists(path),
    "The conversion script is not installed."
  )
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

# A pipeline small enough to run in a second, with one target that has no
# dependencies, one with a single dependency, and one with two.
convert_write_pipeline <- function() {
  writeLines(
    c(
      "library(targets)",
      "list(",
      "  targets::tar_target(alpha, 1L),",
      "  targets::tar_target(beta, alpha + 1L),",
      "  targets::tar_target(gamma, alpha + beta)",
      ")"
    ),
    "_targets.R"
  )
}

# This is the gate on the one thing the conversion reimplements.  targets
# computes a target's dependency hash from internals, and the conversion has to
# produce the same value from the outside; if targets changes how, this fails
# here rather than silently writing a wrong hash into somebody's store.
targets::tar_test("the dependency hash matches the one targets records", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  convert_write_pipeline()
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  meta <- targets::tar_meta(targets_only = FALSE)
  pipeline <- env$convert_pipeline_targets("_targets.R")
  for (name in c("beta", "gamma")) {
    expect_identical(
      env$convert_depend_hash(pipeline[[name]]$deps, meta),
      meta$depend[match(name, meta$name)]
    )
  }
})

# The other hash is read off the target object rather than recomputed, so what
# needs pinning is that the object carries the value targets records.
targets::tar_test("the command hash on the target object is the recorded one", {
  env <- convert_source()
  convert_write_pipeline()
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  meta <- targets::tar_meta(targets_only = FALSE)
  pipeline <- env$convert_pipeline_targets("_targets.R")
  for (name in c("alpha", "beta", "gamma")) {
    expect_identical(
      pipeline[[name]]$command$hash,
      meta$command[match(name, meta$name)]
    )
  }
})

test_that("convert_depend_hash drops a dependency with no record", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  meta <-
    data.frame(
      name = c("alpha", "beta"),
      data = c("hash_alpha", "hash_beta"),
      stringsAsFactors = FALSE
    )
  # "+" is a dependency of any arithmetic command and never has a record, so
  # the hash has to come out the same with and without it.
  expect_identical(
    env$convert_depend_hash(c("+", "alpha", "beta"), meta),
    env$convert_depend_hash(c("alpha", "beta"), meta)
  )
  # Order is the target's, not sorted, so it changes the hash.
  expect_false(identical(
    env$convert_depend_hash(c("alpha", "beta"), meta),
    env$convert_depend_hash(c("beta", "alpha"), meta)
  ))
})

test_that("convert_depend_hash handles a command with no recorded dependency", {
  env <- convert_source()
  meta <- data.frame(name = "alpha", data = "hash_alpha", stringsAsFactors = FALSE)
  expect_identical(env$convert_depend_hash(character(0), meta), NA_character_)
  expect_identical(env$convert_depend_hash("+", meta), NA_character_)
})

test_that("convert_depend_hash refuses a dependency with no stored value", {
  env <- convert_source()
  meta <-
    data.frame(
      name = c("alpha", "beta"),
      data = c("hash_alpha", NA_character_),
      stringsAsFactors = FALSE
    )
  expect_error(
    env$convert_depend_hash(c("alpha", "beta"), meta),
    "no stored value"
  )
})

test_that("convert_write_meta rewrites two fields of the last row only", {
  env <- convert_source()
  path <- withr::local_tempfile()
  writeLines(
    c(
      "name|type|data|command|depend|seed",
      "fit_a|stem|DATA_A|OLDCMD_A|OLDDEP_A|1",
      "other|stem|DATA_O|CMD_O|DEP_O|2",
      "fit_a|stem|DATA_A2|OLDCMD_A2|OLDDEP_A2|3"
    ),
    path
  )
  report <-
    data.frame(
      name = "fit_a",
      command_old = "OLDCMD_A2",
      command_new = "NEWCMD",
      depend_old = "OLDDEP_A2",
      depend_new = "NEWDEP",
      stringsAsFactors = FALSE
    )
  expect_identical(env$convert_write_meta(path, report), 1L)
  got <- readLines(path)
  expect_identical(got[[1L]], "name|type|data|command|depend|seed")
  # An earlier row for the same target, which targets no longer reads, and an
  # unrelated row are both left alone.
  expect_identical(got[[2L]], "fit_a|stem|DATA_A|OLDCMD_A|OLDDEP_A|1")
  expect_identical(got[[3L]], "other|stem|DATA_O|CMD_O|DEP_O|2")
  expect_identical(got[[4L]], "fit_a|stem|DATA_A2|NEWCMD|NEWDEP|3")
  expect_length(got, 4L)
})

test_that("convert_write_meta keeps a separator inside a later field", {
  env <- convert_source()
  path <- withr::local_tempfile()
  writeLines(
    c(
      "name|type|data|command|depend|warnings",
      "fit_a|stem|DATA|OLD|OLDDEP|a | b"
    ),
    path
  )
  report <-
    data.frame(
      name = "fit_a",
      command_old = "OLD",
      command_new = "NEW",
      depend_old = "OLDDEP",
      depend_new = "NEWDEP",
      stringsAsFactors = FALSE
    )
  env$convert_write_meta(path, report)
  expect_identical(readLines(path)[[2L]], "fit_a|stem|DATA|NEW|NEWDEP|a | b")
})

test_that("convert_write_meta refuses a row that moved under it", {
  env <- convert_source()
  path <- withr::local_tempfile()
  original <- c(
    "name|type|data|command|depend",
    "fit_a|stem|DATA|SOMETHING_ELSE|OLDDEP"
  )
  writeLines(original, path)
  report <-
    data.frame(
      name = "fit_a",
      command_old = "OLD",
      command_new = "NEW",
      depend_old = "OLDDEP",
      depend_new = "NEWDEP",
      stringsAsFactors = FALSE
    )
  expect_error(env$convert_write_meta(path, report), "changed while converting")
  expect_identical(readLines(path), original)
})

test_that("convert_write_meta refuses metadata it does not recognise", {
  env <- convert_source()
  path <- withr::local_tempfile()
  writeLines(c("name|type|data", "fit_a|stem|DATA"), path)
  report <-
    data.frame(
      name = "fit_a",
      command_old = "OLD",
      command_new = "NEW",
      depend_old = "OLDDEP",
      depend_new = "NEWDEP",
      stringsAsFactors = FALSE
    )
  expect_error(env$convert_write_meta(path, report), "no name, command or depend")
})

test_that("convert_report skips an estimation target with no stored fit", {
  env <- convert_source()
  pipeline <-
    list(
      fit_a_fit_simple = list(
        command = list(hash = "NEWCMD_A"),
        deps = character(0)
      ),
      fit_b_fit_simple = list(
        command = list(hash = "NEWCMD_B"),
        deps = character(0)
      )
    )
  meta <-
    data.frame(
      name = c("fit_a_fit_simple", "fit_b_fit_simple"),
      data = c("DATA_A", NA_character_),
      command = c("OLDCMD_A", "OLDCMD_B"),
      depend = c("OLDDEP_A", "OLDDEP_B"),
      stringsAsFactors = FALSE
    )
  out <-
    env$convert_report(
      pipeline = pipeline,
      fits = names(pipeline),
      meta = meta
    )
  # fit_b errored or was never built, so there is no fit to carry over.
  expect_identical(out$name, "fit_a_fit_simple")
  expect_identical(out$command_old, "OLDCMD_A")
  expect_identical(out$command_new, "NEWCMD_A")
})

test_that("convert_report returns the documented shape when nothing applies", {
  env <- convert_source()
  out <-
    env$convert_report(
      pipeline = list(),
      fits = character(0),
      meta = env$convert_empty()
    )
  expect_identical(nrow(out), 0L)
  expect_identical(
    names(out),
    c("name", "command_old", "command_new", "depend_old", "depend_new")
  )
})

targets::tar_test("convert_indirect_cache does nothing without the old cache", {
  env <- convert_source()
  expect_message(
    out <- env$convert_indirect_cache(store = "_targets", script = "_targets.R"),
    "nothing to do"
  )
  expect_identical(nrow(out), 0L)
})

targets::tar_test("convert_indirect_cache refuses a cache with no metadata", {
  env <- convert_source()
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  expect_error(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R"),
    "no fits to keep"
  )
})

targets::tar_test("convert_indirect_cache refuses a pipeline with no fits", {
  env <- convert_source()
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  convert_write_pipeline()
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  expect_error(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R"),
    "no estimation targets"
  )
})

# Put a store into the state an older version left: the fit recorded against a
# command and a dependency hash this version does not produce.
convert_scramble <- function(name,
                             path = file.path("_targets", "meta", "meta")) {
  lines <- readLines(path, warn = FALSE)
  header <- strsplit(lines[[1L]], "|", fixed = TRUE)[[1L]]
  index <- match(c("name", "command", "depend"), header)
  for (row in seq_along(lines)[-1L]) {
    fields <- strsplit(lines[[row]], "|", fixed = TRUE)[[1L]]
    if (identical(fields[[index[[1L]]]], name)) {
      fields[[index[[2L]]]] <- "stale_command00"
      fields[[index[[3L]]]] <- "stale_depend000"
      lines[[row]] <- paste(fields, collapse = "|")
    }
  }
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(lines, con = connection)
  invisible()
}

# The whole point of the script, end to end: a fit recorded under the old
# scheme is outdated before and current after, and its stored value never
# moves.  The estimation target is matched by its name, so this needs no model
# and no fit, only the shape.
targets::tar_test("an adopted fit is current afterwards and never rebuilt", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  writeLines(
    c(
      "library(targets)",
      "list(",
      "  targets::tar_target(upstream, 1L),",
      "  targets::tar_target(demo_fit_simple, upstream + 41L)",
      ")"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  before <- targets::tar_read(demo_fit_simple)
  expect_identical(before, 42L)
  directory <- file.path("_targets", "user", "nlmixr2")
  dir.create(directory, recursive = TRUE)
  writeLines("not read by the conversion", file.path(directory, "0cafe"))
  convert_scramble("demo_fit_simple")
  expect_true("demo_fit_simple" %in% targets::tar_outdated(
    callr_function = NULL,
    reporter = "silent"
  ))
  out <- suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  expect_identical(out$name, "demo_fit_simple")
  expect_identical(out$command_old, "stale_command00")
  expect_identical(out$depend_old, "stale_depend000")
  # Current again, so a build would skip it.
  expect_false("demo_fit_simple" %in% targets::tar_outdated(
    callr_function = NULL,
    reporter = "silent"
  ))
  # The fit itself was neither read nor rewritten.
  expect_identical(targets::tar_read(demo_fit_simple), before)
  # And the cache is gone, so the check stops firing.
  expect_false(dir.exists(directory))
  expect_silent(nlmixr2targets_assert_no_indirect_cache(store = "_targets"))
})

targets::tar_test("the conversion backs the metadata up before writing", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  writeLines(
    c(
      "library(targets)",
      "list(targets::tar_target(demo_fit_simple, 1L))"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  original <- readLines(file.path("_targets", "meta", "meta"), warn = FALSE)
  convert_scramble("demo_fit_simple")
  suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  backups <- list.files(
    file.path("_targets", "meta"),
    pattern = "^meta\\.before-conversion-",
    full.names = TRUE
  )
  expect_length(backups, 1L)
  # The copy is of the store as it was, scrambled, not of the rewrite.
  expect_identical(
    readLines(backups[[1L]], warn = FALSE)[[1L]],
    original[[1L]]
  )
  expect_match(
    paste(readLines(backups[[1L]], warn = FALSE), collapse = "\n"),
    "stale_command00"
  )
})

targets::tar_test("a dry run changes nothing", {
  env <- convert_source()
  writeLines(
    c(
      "library(targets)",
      "list(targets::tar_target(demo_fit_simple, 1L))"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  directory <- file.path("_targets", "user", "nlmixr2")
  dir.create(directory, recursive = TRUE)
  convert_scramble("demo_fit_simple")
  meta_path <- file.path("_targets", "meta", "meta")
  before <- readLines(meta_path, warn = FALSE)
  out <- suppressMessages(
    env$convert_indirect_cache(
      store = "_targets",
      script = "_targets.R",
      dry_run = TRUE
    )
  )
  expect_identical(out$name, "demo_fit_simple")
  # The new dependency hash needs the rebuilt values, so a dry run cannot
  # report one.
  expect_true(is.na(out$depend_new))
  expect_identical(readLines(meta_path, warn = FALSE), before)
  expect_true(dir.exists(directory))
  expect_length(
    list.files(file.path("_targets", "meta"), pattern = "before-conversion"),
    0L
  )
})

# The branch that runs for a real pipeline: a generated helper whose command
# this version writes differently is rebuilt, and the fit's new dependency hash
# is taken over the rebuilt value.  A stand-in whose name contains one of the
# generators is enough to reach it without a model or a fit.
targets::tar_test("a stale generated helper is rebuilt before the fit is adopted", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  writeLines(
    c(
      "library(targets)",
      "nlmixr_object_simplify_stub <- function() 7L",
      "list(",
      "  targets::tar_target(demo_object_simple, nlmixr_object_simplify_stub()),",
      "  targets::tar_target(demo_fit_simple, demo_object_simple + 35L)",
      ")"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  expect_identical(targets::tar_read(demo_fit_simple), 42L)
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  # Both rows look as an older version would have left them.
  convert_scramble("demo_object_simple")
  convert_scramble("demo_fit_simple")
  out <- suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  expect_identical(out$name, "demo_fit_simple")
  # The helper was rebuilt, so its row is correct rather than scrambled, and
  # the fit's new dependency hash was taken over that rebuilt value.
  meta <- targets::tar_meta(targets_only = FALSE)
  expect_false(identical(
    meta$command[match("demo_object_simple", meta$name)],
    "stale_command00"
  ))
  expect_identical(
    out$depend_new,
    env$convert_depend_hash(
      env$convert_pipeline_targets("_targets.R")$demo_fit_simple$deps,
      meta
    )
  )
  expect_false("demo_fit_simple" %in% targets::tar_outdated(
    callr_function = NULL,
    reporter = "silent"
  ))
  expect_identical(targets::tar_read(demo_fit_simple), 42L)
})

# The gate on the mechanism that lets the conversion read a pipeline at all.
# The check that sends people here fires while `_targets.R` is sourced, so the
# conversion stands it down with an option for the duration, and must put it
# back.  An option does not cross into a callr subprocess, which is why the
# rebuild runs in this process.
targets::tar_test("sourcing the pipeline stands the cache check down", {
  env <- convert_source()
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  writeLines(
    c(
      "library(targets)",
      "stopifnot(isTRUE(getOption('nlmixr2targets.allow_indirect_cache')))",
      "list(targets::tar_target(demo_fit_simple, 1L))"
    ),
    "_targets.R"
  )
  expect_named(env$convert_pipeline_targets("_targets.R"), "demo_fit_simple")
  expect_null(getOption("nlmixr2targets.allow_indirect_cache"))
})

targets::tar_test("the option goes back to what it was, not to unset", {
  env <- convert_source()
  withr::local_options(nlmixr2targets.allow_indirect_cache = FALSE)
  writeLines(
    c("library(targets)", "list(targets::tar_target(demo_fit_simple, 1L))"),
    "_targets.R"
  )
  env$convert_pipeline_targets("_targets.R")
  expect_false(getOption("nlmixr2targets.allow_indirect_cache"))
})

# The rebuild's environment ----
#
# tar_make() defaults envir to parent.frame(), so a rebuild driven from inside
# a function hands targets that frame as the pipeline's environment. For a
# target bound for a worker, targets then ships the environment itself instead
# of the objects of the global one, and a `_targets.R` that source()s its model
# functions -- which land in the global environment -- leaves the worker
# without them: "object ... not found", far from the cause. The conversion
# therefore passes envir = globalenv().

targets::tar_test("the rebuild runs with the global environment", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  # The stub reports the environment targets is using while it builds, which
  # is the only way to see it: tar_make() puts the old one back on exit.
  writeLines(
    c(
      "the_model <- 41L",
      "nlmixr_object_simplify_probe <- function(x) {",
      "  identical(targets::tar_option_get('envir'), globalenv())",
      "}"
    ),
    "helpers.R"
  )
  writeLines(
    c(
      "library(targets)",
      "source('helpers.R')",
      "list(",
      "  targets::tar_target(",
      "    demo_object_simple,",
      "    nlmixr_object_simplify_probe(the_model)",
      "  ),",
      "  targets::tar_target(demo_fit_simple, isTRUE(demo_object_simple))",
      ")"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent")
  )
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  convert_scramble("demo_fit_simple")
  suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  expect_true(targets::tar_read(demo_object_simple))
})

test_that("convert_generated_helpers finds them by what the command calls", {
  env <- convert_source()
  pipeline <- list(
    demo_object_simple = list(
      command = list(string = "nlmixr_object_simplify(mod)"),
      deps = character(0)
    ),
    demo_covariates = list(
      command = list(string = "nlmixr_object_covariates(demo_object_simple)"),
      deps = character(0)
    ),
    d_mod = list(
      command = list(string = "make_the_data(raw)"),
      deps = character(0)
    ),
    demo_fit_simple = list(
      command = list(string = "nlmixr_fit_simple(demo_object_simple, d_mod)"),
      deps = c("demo_object_simple", "demo_covariates", "d_mod")
    )
  )
  out <- env$convert_generated_helpers(
    pipeline = pipeline,
    fits = "demo_fit_simple"
  )
  # The user's own data target is a dependency too and must not be rebuilt.
  expect_identical(out$name, c("demo_object_simple", "demo_covariates"))
  expect_identical(out$simplifies, c(TRUE, FALSE))
})

test_that("convert_generated_helpers returns the shape when nothing matches", {
  env <- convert_source()
  out <- env$convert_generated_helpers(
    pipeline = list(
      demo_fit_simple = list(
        command = list(string = "nlmixr_fit_simple(a, b)"),
        deps = character(0)
      )
    ),
    fits = "demo_fit_simple"
  )
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), c("name", "simplifies"))
})

targets::tar_test("the conversion stops if a worker returns the old type", {
  env <- convert_source()
  # What a version from before the cache was removed gives back: the md5 of a
  # cache file rather than the model.
  writeLines(
    c(
      "library(targets)",
      "nlmixr_object_simplify_oldstyle <- function() {",
      "  'add26df84bb8f731decbc2517b9cbb63'",
      "}",
      "list(",
      "  targets::tar_target(",
      "    demo_object_simple,",
      "    nlmixr_object_simplify_oldstyle()",
      "  )",
      ")"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(callr_function = NULL, reporter = "silent",
                      envir = globalenv())
  )
  expect_error(
    env$convert_check_simplified(
      helpers = data.frame(
        name = "demo_object_simple",
        simplifies = TRUE,
        stringsAsFactors = FALSE
      ),
      store = "_targets"
    ),
    "came back as a character"
  )
})

# The check stands down for the rebuild too ----
#
# `_targets.R` is sourced twice by one conversion: once to read the pipeline,
# and again inside tar_make() when the helpers are rebuilt. Standing the check
# down only for the read leaves the rebuild stopping at the very check the
# conversion exists to satisfy -- and no pipeline in these tests calls
# tar_nlmixr_raw(), so nothing else here would notice.
targets::tar_test("the cache check stays down for the rebuild, not just the read", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  pipeline <- function(guard) {
    writeLines(
      c(
        "library(targets)",
        if (guard) {
          "stopifnot(isTRUE(getOption('nlmixr2targets.allow_indirect_cache')))"
        },
        "nlmixr_object_simplify_probe <- function() 1L",
        "list(",
        "  targets::tar_target(",
        "    demo_object_simple,",
        "    nlmixr_object_simplify_probe()",
        "  ),",
        "  targets::tar_target(demo_fit_simple, demo_object_simple + 1L)",
        ")"
      ),
      "_targets.R"
    )
  }
  # Built without the guard, because the check is legitimately not standing
  # down for an ordinary build.
  pipeline(guard = FALSE)
  suppressMessages(
    targets::tar_make(
      callr_function = NULL,
      reporter = "silent",
      envir = globalenv()
    )
  )
  # From here on, every source of the script insists the check is down.
  pipeline(guard = TRUE)
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  convert_scramble("demo_fit_simple")
  out <- suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  expect_identical(out$name, "demo_fit_simple")
  expect_false(dir.exists(file.path("_targets", "user", "nlmixr2")))
})

# A conversion that stopped part way leaves its helpers invalidated and its
# fits not yet adopted. tar_invalidate() refuses a name the metadata no longer
# has, so the second attempt must not hand it the whole set.
targets::tar_test("a conversion restarts after one that stopped part way", {
  skip_if_not_installed("secretbase")
  env <- convert_source()
  writeLines(
    c(
      "library(targets)",
      "nlmixr_object_simplify_probe <- function() 1L",
      "list(",
      "  targets::tar_target(",
      "    demo_object_simple,",
      "    nlmixr_object_simplify_probe()",
      "  ),",
      "  targets::tar_target(demo_fit_simple, demo_object_simple + 1L)",
      ")"
    ),
    "_targets.R"
  )
  suppressMessages(
    targets::tar_make(
      callr_function = NULL,
      reporter = "silent",
      envir = globalenv()
    )
  )
  dir.create(file.path("_targets", "user", "nlmixr2"), recursive = TRUE)
  convert_scramble("demo_fit_simple")
  # Exactly what the earlier attempt left behind.
  targets::tar_invalidate(names = "demo_object_simple", store = "_targets")
  expect_false("demo_object_simple" %in% targets::tar_meta(store = "_targets")$name)
  out <- suppressMessages(
    env$convert_indirect_cache(store = "_targets", script = "_targets.R")
  )
  expect_identical(out$name, "demo_fit_simple")
  expect_false("demo_fit_simple" %in% targets::tar_outdated(
    store = "_targets",
    script = "_targets.R",
    callr_function = NULL,
    envir = globalenv()
  ))
})
