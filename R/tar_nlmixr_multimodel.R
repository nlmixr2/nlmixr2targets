#' Generate a list of models based on a single dataset and estimation method
#'
#' @param ... Named arguments with the format `"Model description" =
#'   modelFunction`
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @inheritParams tar_nlmixr
#' @returns A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @export
tar_nlmixr_multimodel <- function(name, ..., data, est, control = list(), table = nlmixr2est::tableControl(), env = parent.frame()) {
  tar_nlmixr_multimodel_parse(
    name = targets::tar_deparse_language(substitute(name)),
    data = substitute(data),
    est = substitute(est),
    control = substitute(control),
    table = substitute(table),
    # This extracts the ... argument similarly to using `substitute()`.  From
    # https://stackoverflow.com/questions/55019441/deparse-substitute-with-three-dots-arguments
    model_list = match.call(expand.dots = FALSE)$...,
    env = env
  )
}

#' Generate nlmixr multimodel target set for all models in one call to
#' `tar_nlmixr_multimodel()`
#'
#' @inheritParams tar_nlmixr_multimodel
#' @inheritParams tar_nlmixr
#' @param model_list A named list of calls for model targets to be created
#' @keywords Internal
tar_nlmixr_multimodel_parse <- function(name, data, est, control, table, model_list, env) {
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
      env = env
    )
  mask_self_referential <- tar_nlmixr_multimodel_has_self_reference(model_list = model_list, name = name)
  while (any(mask_self_referential)) {
    mask_self_referential_orig <- mask_self_referential
    model_list_self_reference <- model_list[mask_self_referential]
    # Generate a mapping of names to their target names, only for
    # non-self-referential models.
    name_map <-
      stats::setNames(
        vapply(X = ret_prep, FUN = \(x) x$name, FUN.VALUE = ""),
        # rxode2::.matchesLangTemplate() treats single vs double quotes in a
        # call the same.
        sprintf("%s[['%s']]", name, names(ret_prep))
      )[!mask_self_referential]
    model_list_fewer_self_ref <-
      tar_nlmixr_multimodel_remove_self_reference(model_list = model_list[mask_self_referential], name_map = name_map)
    # Replace self-referential models with possibly-not-self-referential models
    model_list[names(model_list_fewer_self_ref)] <- model_list_fewer_self_ref
    # Update the possibly-not-self-referential models
    ret_prep[names(model_list_fewer_self_ref)] <-
      lapply(
        X = model_list[names(model_list_fewer_self_ref)],
        FUN = tar_nlmixr_multimodel_single,
        name = name,
        data = data,
        est = est,
        control = control,
        table = table,
        env = env
      )

    mask_self_referential <- tar_nlmixr_multimodel_has_self_reference(model_list = model_list, name = name)
    if (sum(mask_self_referential) >= sum(mask_self_referential_orig)) {
      # The number of models which are self-referential should consistently
      # decrease as dependencies are removed.  If this doesn't happen, then
      # there is a circular reference somewhere.
      stop(
        "The following model(s) appear to have circular references to each other: ",
        paste0('"', names(mask_self_referential)[mask_self_referential], '"', collapse = ", ")
      )
    }
  }
  # Extract the targets to fit.  This will be a list of lists.  The inner list
  # will have the three targets for fitting the model, and the outer list will
  # be one element per model fit.
  target_model_fitting <- lapply(X = unname(ret_prep), FUN = \(x) x[["target"]])
  # Generate the combined list with names
  combined_list <- lapply(X = ret_prep, FUN = \(x) as.name(x$name))
  call_list <- str2lang("list()")
  for (idx in seq_along(combined_list)) {
    call_list[[idx + 1]] <- combined_list[[idx]]
  }
  names(call_list) <- c("", names(combined_list))
  target_combined_list <- targets::tar_target_raw(name = name, command = call_list)
  # Return the models to fit and the list-combining target
  append(
    target_model_fitting,
    target_combined_list
  )
}

#' Does the model list refer to another model in the model list?
#'
#' @inheritParams tar_nlmixr_multimodel_parse
#' @returns A logical vector the same length as `model_list` indicating if the
#'   model is self-referential to another model in the list
#' @keywords Internal
tar_nlmixr_multimodel_has_self_reference <- function(model_list, name) {
  sapply(X = model_list, FUN = tar_nlmixr_multimodel_has_self_reference_single, name = name)
}
#' @describeIn tar_nlmixr_multimodel_has_self_reference A helper function to
#'   look at each call for each model separately
#' @param model A single model call for the model target to be created
tar_nlmixr_multimodel_has_self_reference_single <- function(model, name) {
  if (rxode2::.matchesLangTemplate(model, str2lang(sprintf("%s[[.]]", name)))) {
    TRUE
  } else if (length(model) > 1) {
    any(vapply(X = model, FUN = tar_nlmixr_multimodel_has_self_reference_single, FUN.VALUE = TRUE, name = name))
  } else {
    FALSE
  }
}

tar_nlmixr_multimodel_remove_self_reference <- function(model_list, name_map) {
  lapply(X = model_list, FUN = tar_nlmixr_multimodel_remove_self_reference_single, name_map = name_map)
}

tar_nlmixr_multimodel_remove_self_reference_single <- function(model, name_map) {
  if (length(model) <= 1) {
    # Do not modify it or recurse, return `model` unchanged.  Use less than or
    # equal to in case of NULL or another zero-length object.
  } else {
    mask_template_match <-
      vapply(
        X = lapply(X = names(name_map), FUN = str2lang),
        FUN = rxode2::.matchesLangTemplate,
        FUN.VALUE = TRUE,
        x = model
      )
    if (any(mask_template_match)) {
      # Use the fitsimple version of the model fitting so that it is not
      # dependent on data changes.
      model <- str2lang(paste0(name_map[[which(mask_template_match)]], "_fitsimple"))
    } else {
      for (idx in seq_along(model)) {
        model[[idx]] <-
          tar_nlmixr_multimodel_remove_self_reference_single(
            model = model[[idx]],
            name_map = name_map
          )
      }
    }
  }
  model
}

#' Generate a single nlmixr multimodel target set for one model
#'
#' @inheritParams tar_nlmixr_multimodel
#' @inheritParams tar_nlmixr
#' @keywords Internal
tar_nlmixr_multimodel_single <- function(object, name, data, est, control, table, env) {
  # Trade-off: Running digest() on the call (object) will rerun the model if the
  # function name changes even if the underlying model does not change.  Running
  # digest on the evaluated call (eval(object, envir = env)) will not rerun if
  # the function name changes, but the cost of evaluation could be large if the
  # user puts a lot of information into generating the object (e.g. lots of
  # model piping).
  #
  # Choice: Use the computationally-cheap option here.
  hash_long <- digest::digest(object)
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
      data_simple_name = paste0(name_hash, "_dsimple"),
      fit_simple_name = paste0(name_hash, "_fitsimple"),
      env = env
    )
  list(
    target = tar_prep,
    name = name_hash
  )
}
