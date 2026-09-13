# Walk a captured expression and collect symbol names that could refer
# to user-defined functions. Skips the head of every call (`f(x)` -> we
# do not add `f`), since the head is the function being called, not a
# candidate model function. The model function appears either as the
# whole expression (`object = pheno`), as a `|>` desugared first arg
# (`object = pheno |> model({...})` -> `model(pheno, ...)`, first arg
# is `pheno`), or as a nested non-head symbol.
#
# Shared by `tar_nlmixr_protect_zero_initial()` (construction time) and
# `nlmixr_object_zero_initial_eval()` (runtime), which both need to
# locate user functions referenced by a captured expression.
.collect_top_symbols <- function(expr) {
  out <- character()
  visit <- function(e, is_head = FALSE) {
    if (is.symbol(e) && !is_head) {
      out[[length(out) + 1L]] <<- as.character(e)
    } else if (is.call(e)) {
      visit(e[[1L]], is_head = TRUE)
      for (idx in seq_along(e)[-1L]) visit(e[[idx]])
    }
  }
  visit(expr)
  unique(out)
}

# Settings that `tar_nlmixr()` and friends hand to every target they generate.
# The names and defaults track `targets::tar_target()` exactly, so
# `targets::tar_option_set()` reaches the generated targets the same way it
# reaches hand-written ones.
tar_nlmixr_target_setting_names <-
  c(
    "format", "repository", "library", "memory", "garbage_collection",
    "deployment", "resources", "storage", "retrieval", "cue"
  )

#' Collect the forwarded `targets::tar_target()` settings from a caller's frame
#'
#' Read out of the frame rather than listed argument by argument so that a
#' generator's signature and what it forwards cannot drift apart.
#'
#' @param env The generator's own frame.
#' @returns A named list ready to splice into `targets::tar_target_raw()`.
#' @noRd
tar_nlmixr_collect_target_settings <- function(env = parent.frame()) {
  mget(tar_nlmixr_target_setting_names, envir = env)
}

#' Build one generated target with the forwarded settings applied
#'
#' @param settings The list from `tar_nlmixr_collect_target_settings()`.
#' @param ... Arguments specific to this target (`name`, `command`, and where
#'   relevant `packages` and `string`).
#' @returns A `tar_target` object.
#' @noRd
tar_nlmixr_target_raw <- function(settings, ...) {
  # `quote = TRUE` is load-bearing.  `command` arrives as a language object;
  # without it `do.call()` evaluates that object rather than handing it over to
  # be stored, which runs the target's own work while the pipeline is still
  # being built.
  do.call(targets::tar_target_raw, c(list(...), settings), quote = TRUE)
}

#' The forwarded settings at their `targets::tar_target()` defaults
#'
#' `tar_nlmixr_raw()` is exported and may be called on its own, so it needs the
#' same starting point `targets::tar_target()` would have used.  The values are
#' read at call time, which is when `_targets.R` is sourced, so a
#' `targets::tar_option_set()` earlier in that file is respected.
#'
#' @returns A named list matching `tar_nlmixr_target_setting_names`.
#' @noRd
tar_nlmixr_collect_target_settings_default <- function() {
  ret <-
    list(
      format = targets::tar_option_get("format"),
      repository = targets::tar_option_get("repository"),
      library = targets::tar_option_get("library"),
      memory = targets::tar_option_get("memory"),
      garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
      deployment = targets::tar_option_get("deployment"),
      resources = targets::tar_option_get("resources"),
      storage = targets::tar_option_get("storage"),
      retrieval = targets::tar_option_get("retrieval"),
      cue = targets::tar_option_get("cue")
    )
  # The defaults and the forwarded names are written out separately; this keeps
  # them from drifting apart silently.
  stopifnot(identical(names(ret), tar_nlmixr_target_setting_names))
  ret
}
