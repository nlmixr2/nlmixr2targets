#' Generate a list of models based on a single dataset and estimation method
#'
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @return A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @export
tar_nlmixr_multimodel <- function(name, data, est, control = list(), table = nlmixr2est::tableControl()) {
  if (is.null(est)) {
    stop("'est' must not be null")
  }
  name <- targets::tar_deparse_language(substitute(name))
  name_obj_simple <- paste(name, "object_simple", sep = "_tar_")
  name_data_simple <- paste(name, "data_simple", sep = "_tar_")

}
