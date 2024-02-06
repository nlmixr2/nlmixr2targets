#' Generate a list of models based on a single dataset and estimation method
#'
#' @param ... Named arguments with the format `"Model description" =
#'   modelFunction`
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @return A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @export
tar_nlmixr_multimodel <- function(name, ..., data, est, control = list(), table = nlmixr2est::tableControl()) {
  if (is.null(est)) {
    stop("'est' must not be null")
  }
  tar_nlmixr_multimodel_parse(
    name = targets::tar_deparse_language(substitute(name)),
    data = data,
    est = est,
    control = control,
    table = table
  )
}

tar_nlmixr_multimodel_parse <- function(name, data, est, control, table, ...) {
  args <- list(...)
  checkmate::assert_named(args, type = "unique")
  lapply(
    X = seq_along(args),
    FUN = \(idx, data) {
      tar_nlmixr_multimodel_single(
        name = name,
        object = args[[idx]],
        data = data,
        description = names(args)[[idx]],
        est = est,
        control = control,
        table = table
      )
    }
  )
}

tar_nlmixr_multimodel_single <- function(name, object, data, description, est, control, table) {
  tar_nlmixr(
  )
}
