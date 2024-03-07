#' Generate a list of models based on a single dataset and estimation method
#'
#' @param ... Named arguments with the format `"Model description" =
#'   modelFunction`
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @param envir The environment where models are defined (usually doesn't need
#'   to be modified)
#' @return A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @export
tar_nlmixr_multimodel <- function(name, ..., data, est, control = list(), table = nlmixr2est::tableControl(), envir = parent.frame()) {
  if (is.null(est)) {
    stop("'est' must not be null")
  }

  tar_nlmixr_multimodel_parse(
    name = targets::tar_deparse_language(substitute(name)),
    data = substitute(data),
    est = substitute(est),
    control = substitute(control),
    table = substitute(table),
    # This extracts the ... argument similarly to using `substitute()`.  From
    # https://stackoverflow.com/questions/55019441/deparse-substitute-with-three-dots-arguments
    model_list = match.call(expand.dots = FALSE)$...,
    envir = envir
  )
}

tar_nlmixr_multimodel_parse <- function(name, data, est, control, table, model_list, envir) {
  checkmate::assert_named(model_list, type = "unique")
  ret_prep <-
    lapply(
      X = model_list,
      FUN = tar_nlmixr_multimodel_single,
      name = name,
      data = data,
      est = est,
      control = control,
      table = table,
      envir = envir
    )
  # Extract the targets to fit.  This will be a list of lists.  The inner list
  # will have the three targets for fitting the model, and the outer list will
  # be one element per model fit.
  target_model_fitting <- lapply(X = unname(ret_prep), FUN = \(x) x[["target"]])
  # Generate the combined list with names
  combined_list <- lapply(X = ret_prep, FUN = \(x) as.name(x$name))
  target_combined_list <- targets::tar_target_raw(name = name, command = str2lang(deparse(combined_list)))
  # Return the models to fit and the list-combining target
  append(
    target_model_fitting,
    target_combined_list
  )
}

tar_nlmixr_multimodel_single <- function(object, name, data, est, control, table, envir) {
  # Hash the model itself without its description.  Then, if the description
  # changes, the model will not need to rerun.
  hash_long <- digest::digest(eval(object, envir = envir))
  hash <- substr(hash_long, 1, 8)
  name_hash <- paste(name, hash, sep = "_")
  tar_prep <-
    tar_nlmixr_raw(
      name = name_hash,
      object = object,
      data = data,
      est = est,
      control = control,
      table = table,
      object_simple_name = paste0(name_hash, "_osimple"),
      data_simple_name = paste0(name_hash, "_dsimple")
    )
  list(
    target = tar_prep,
    name = name_hash
  )
}
