#' Generate a list of models based on a single dataset and estimation method
#'
#' @param ... Named arguments with the format `"Model description" =
#'   modelFunction`
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @inheritParams tar_nlmixr
#' @returns A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @seealso [tar_nlmixr()] for fitting a single model.
#' @examples
#' \dontrun{
#' library(targets)
#' targets::tar_script({
#' pheno <- function() {
#'   ini({
#'     lcl <- log(0.008); label("Typical value of clearance")
#'     lvc <-  log(0.6); label("Typical value of volume of distribution")
#'     etalcl + etalvc ~ c(1,
#'                         0.01, 1)
#'     cpaddSd <- 0.1; label("residual variability")
#'   })
#'   model({
#'     cl <- exp(lcl + etalcl)
#'     vc <- exp(lvc + etalvc)
#'     kel <- cl/vc
#'     d/dt(central) <- -kel*central
#'     cp <- central/vc
#'     cp ~ add(cpaddSd)
#'   })
#' }
#' pheno2 <- function() {
#'   ini({
#'     lcl <- log(0.008); label("Typical value of clearance")
#'     lvc <-  log(0.6); label("Typical value of volume of distribution")
#'     etalcl + etalvc ~ c(2,
#'                         0.01, 2)
#'     cpaddSd <- 3.0; label("residual variability")
#'   })
#'   model({
#'     cl <- exp(lcl + etalcl)
#'     vc <- exp(lvc + etalvc)
#'     kel <- cl/vc
#'     d/dt(central) <- -kel*central
#'     cp <- central/vc
#'     cp ~ add(cpaddSd)
#'   })
#' }
#' list(
#'   tar_nlmixr_multimodel(
#'     name = all_models,
#'     data = nlmixr2data::pheno_sd,
#'     est = "saem",
#'     "Base model" = pheno,
#'     "Alternative residual error" = pheno2
#'   )
#' )
#' })
#' targets::tar_make()
#' }
#' @export
tar_nlmixr_multimodel <- function(name, ..., data, est, control = list(),
                                  table = nlmixr2est::tableControl(), env = parent.frame()) {
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
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_named(model_list, type = "unique")
  checkmate::assert_environment(env)

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
  # Within-list piping (e.g. `models[["A"]] |> ini(...)` referenced from
  # another list entry) is resolved iteratively: each pass rewrites
  # references to already-resolved models with their `_fit_simple` target
  # names, then re-checks. The number of self-referential models must
  # strictly decrease each pass; otherwise there is a circular reference
  # and we stop. The loop runs at most `length(model_list)` times in the
  # worst case (one model resolved per pass), so we cap iterations as
  # belt-and-braces against any pathological input we have not anticipated.
  mask_self_referential <- tar_nlmixr_multimodel_has_self_reference(model_list = model_list, name = name)
  max_iter <- length(model_list)
  iter <- 0L
  while (any(mask_self_referential)) {
    iter <- iter + 1L
    if (iter > max_iter) { # nocov start
      # Unreachable through any model_list shape we have constructed: the
      # circular-reference check below fires before this iteration cap can
      # ever trigger. Kept as a defensive guard against future input we
      # have not anticipated.
      stop(
        "Internal error: self-reference resolution did not terminate after ",
        max_iter, " iterations. Please report this with a reproducible example."
      )
    } # nocov end
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
  vapply(
    X = model_list,
    FUN = tar_nlmixr_multimodel_has_self_reference_single,
    FUN.VALUE = TRUE,
    name = name
  )
}
#' @describeIn tar_nlmixr_multimodel_has_self_reference A helper function to
#'   look at each call for each model separately
#' @param model A single model call for the model target to be created
tar_nlmixr_multimodel_has_self_reference_single <- function(model, name) {
  # rxode2::.matchesLangTemplate() is exported from rxode2 but its dotted
  # name signals it is conventionally internal. If rxode2 ever renames it,
  # this and the call sites in simplify.R must be updated.
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
      # Use the fit_simple version of the model fitting so that it is not
      # dependent on data changes. The model can match at most one template
      # per pass; guard against the assumption silently breaking.
      if (sum(mask_template_match) > 1L) {
        stop(
          "Internal error: model references multiple templates simultaneously. ",
          "Please report with a reproducible example."
        )
      }
      model <- str2lang(paste0(name_map[[which(mask_template_match)]], "_fit_simple"))
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
  # 8-char prefix; collision probability ~10^-9 for 1000 distinct models.
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
      object_simple_name = paste0(name_hash, "_object_simple"),
      data_simple_name = paste0(name_hash, "_data_simple"),
      fit_simple_name = paste0(name_hash, "_fit_simple"),
      env = env
    )
  list(
    target = tar_prep,
    name = name_hash
  )
}
