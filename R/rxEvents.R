## Emitting events on the rxode2 event bus (see rxode2::rxEventListen()), so
## loggers such as nlmixr2log can follow fits made by targets pipelines.
## These do nothing when rxode2 has no event bus.

#' Emit an rxode2 bus event, if rxode2 has a bus
#' @noRd
nlmixr2targets_event_emit <- function(event, ...) {
  rx <- asNamespace("rxode2")
  if (exists("rxEventEmit", envir = rx, inherits = FALSE)) {
    get("rxEventEmit", envir = rx)(event, ...)
  }
  invisible()
}

#' Tell loggers the final fit of a `tar_nlmixr()` model is ready
#'
#' The fit was logged when the `*_fit_simple` target estimated it; restoring
#' the labels and data changed it in place (a new version of the same run),
#' and the `*_fit` target's name is the name the user will use for it.
#' @noRd
nlmixr2targets_event_fit <- function(fit) {
  if (!inherits(fit, "nlmixr2FitCore")) {
    return(invisible())
  }
  nlmixr2targets_event_emit(
    "fitUpdate", fit = fit, original = fit, name = NULL, what = "ui", inPlace = TRUE,
    fun = "nlmixr_object_complicate"
  )
  if (isTRUE(targets::tar_active())) {
    nlmixr2targets_event_emit(
      "assign", name = targets::tar_name(), value = fit, call = NULL,
      envir = NULL, cached = FALSE, fun = "nlmixr_object_complicate"
    )
  }
  invisible()
}
