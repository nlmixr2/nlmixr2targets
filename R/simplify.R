#' Simplify an nlmixr object
#'
#' This function is typically not needed by end users.
#'
#' The object simplification removes comments (so please use \code{label()}
#' instead of comments to label parameters) and then converts the \code{object}
#' to a "nlmixrui" object.
#'
#' Object metadata (`ui$meta`) and parameter labels (`ui$iniDf$label`) are
#' also stripped from the simplified object before it is written to the
#' indirect cache. They do not affect estimation, and stripping them keeps
#' the cache hash stable across edits to either, so editing only labels or
#' metadata will not invalidate the cached fit. The stripped values are
#' restored on the final fit by [nlmixr_object_complicate()], which reads
#' them straight back off the original model.
#'
#' Since setting initial conditions with `cmt(0)` does not work with `targets`,
#' the function definition of the object must set it with `cmt(initial)`.
#' `cmt(initial)` will be converted to `cmt(0)` before passing to nlmixr2.
#'
#' The simplified model's `model.name` is always set to `"object"`. This keeps
#' the simplified output stable so that the MD5 hash used by the `targets`
#' indirect cache is independent of the symbol the caller bound the model
#' function to.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @returns The MD5 hash used to load the simplified `nlmixrui` object back
#'   from the `nlmixr2targets` indirect cache.
#' @family Simplifiers
#' @seealso [nlmixr_object_complicate()] for the inverse operation that
#'   re-attaches labels, metadata, and the original data on the final fit.
#' @export
nlmixr_object_simplify <- function(object) {
  # drop comments since they won't affect most outputs and typically change
  # between interactive (when srcref is available) to batch (when srcref is not
  # available) mode.
  attr(object, "srcref") <- NULL
  object <- nlmixr_object_simplify_zero_initial(object)
  ret <- nlmixr2est::nlmixr(object)
  # Strip parameter labels and the meta environment so they do not contribute
  # to the cache hash. The rxUi's iniDf setter is an active binding, so
  # writing NA_character_ here also refreshes ret$md5 (which is what
  # save_nlmixr2obj_indirect() keys the cache file by).
  ret$iniDf$label <- NA_character_
  rm(list = ls(envir = ret$meta, all.names = TRUE), envir = ret$meta)
  save_nlmixr2obj_indirect(ret)
}

#' Re-attach labels, metadata, and original data to a simplified fit
#'
#' The inverse of [nlmixr_object_simplify()]. Given a fit produced from the
#' simplified, label-and-meta-stripped model, plus the original model
#' function and the original data, this function:
#'
#' \itemize{
#'   \item re-derives parameter labels and the metadata environment from
#'   `object` and writes them back onto `fit$ui$iniDf$label` and
#'   `fit$ui$meta`, and
#'   \item replaces `fit$env$origData` with the original `data`.
#' }
#'
#' This is what makes label/meta edits in the source model cheap under
#' `targets`: the cache hash for the simplified model object is independent
#' of labels and metadata, so `tar_make()` only re-runs this re-attachment
#' step (and the cheap `_object_simple` step) when only labels or metadata
#' change.
#'
#' This function is typically not invoked directly by end users; it is the
#' command for the final target produced by [tar_nlmixr()].
#'
#' @param fit An estimated nlmixr2 fit produced from the simplified model.
#' @param object The original model function (or object) the fit was
#'   derived from. Its labels and metadata are read back onto `fit`.
#' @param data The original data the fit corresponds to (before
#'   [nlmixr_data_simplify()] reduced its column set).
#'   Must have the same number of rows as the data currently stored on the
#'   fit, mirroring [assign_origData()].
#' @returns The modified `fit`.
#' @family Simplifiers
#' @seealso [nlmixr_object_simplify()], [assign_origData()].
#' @export
nlmixr_object_complicate <- function(fit, object, data) {
  # On a real nlmixr2 fit, `fit` is a tibble whose `ui` slot is a virtual
  # accessor backed by `fit$env$ui`. We must read/write through `fit$env`
  # to avoid the tibble column-setter (which validates row counts).
  ui_in_env <-
    is.environment(fit$env) &&
    exists("ui", envir = fit$env, inherits = FALSE)
  ui <- if (ui_in_env) get("ui", envir = fit$env) else fit$ui
  if (is.null(ui)) {
    stop("Could not find a ui on the fit (neither fit$env$ui nor fit$ui)")
  }

  # Read the labels and meta back off the original model. The same
  # preprocessing nlmixr_object_simplify() applies must happen here so
  # that the iniDf$name keys we look up actually match.
  attr(object, "srcref") <- NULL
  object <- nlmixr_object_simplify_zero_initial(object)
  source_ui <- nlmixr2est::nlmixr(object)
  labels <- stats::setNames(source_ui$iniDf$label, source_ui$iniDf$name)
  meta_list <- as.list(source_ui$meta, all.names = TRUE)

  iniDf <- ui$iniDf
  checkmate::assert_data_frame(iniDf)
  for (n in names(labels)) {
    idx <- which(iniDf$name == n)
    if (length(idx) == 1) {
      iniDf$label[idx] <- labels[[n]]
    }
  }
  # Bypass rxUi's `$<-` / `ini<-` dispatch: those setters re-derive the ui
  # from the ini and would reject a partial iniDf. Direct field assignment
  # (env via `assign`, list via `unclass`/reclass) keeps the round trip
  # idempotent.
  ui <- nlmixr_object_complicate_assign(ui, "iniDf", iniDf)
  if (is.environment(ui$meta)) {
    rm(list = ls(envir = ui$meta, all.names = TRUE), envir = ui$meta)
    for (n in names(meta_list)) {
      assign(n, meta_list[[n]], envir = ui$meta)
    }
  } else {
    ui <- nlmixr_object_complicate_assign(ui, "meta", meta_list)
  }
  if (ui_in_env) {
    assign("ui", ui, envir = fit$env)
  } else if (!is.environment(fit$ui)) {
    fit$ui <- ui
  }
  # If fit$ui is itself an environment (fresh rxUi), the env-mode branches
  # have already mutated it in place; nothing more to write back.

  # Finally swap in the original data, reusing the existing helper for
  # validation and in-place mutation of fit$env$origData.
  assign_origData(fit = fit, data = data)
}

#' Assign a field on an rxUi-shaped object, bypassing S3 method dispatch
#'
#' @param ui An rxUi (an environment or list with class "rxUi").
#' @param field The name of the field to overwrite.
#' @param value The replacement value.
#' @returns `ui`, with `field` set to `value`. For environments the
#'   mutation is in place; for lists, a new list is returned.
#' @noRd
nlmixr_object_complicate_assign <- function(ui, field, value) {
  if (is.environment(ui)) {
    assign(field, value, envir = ui)
    ui
  } else {
    cls <- class(ui)
    class(ui) <- NULL
    ui[[field]] <- value
    class(ui) <- cls
    ui
  }
}

#' Convert initial conditions from cmt(initial) to cmt(0) to work with `targets`
#' parser.
#'
#' @inheritParams nlmixr_object_simplify
#' @noRd
nlmixr_object_simplify_zero_initial <- function(object) {
  if (is.function(object)) {
    # simplification is only required for functions because once they are rxode2
    # or nlmixr2 objects, the code has been transformed
    base::body(object) <- nlmixr_object_simplify_zero_initial_helper(base::body(object))
  }
  object
}

nlmixr_object_simplify_zero_initial_helper <- function(object) {
  # rxode2::.matchesLangTemplate() is exported from rxode2 but its dotted
  # name signals it is conventionally internal. If rxode2 ever renames it,
  # this and the call sites in tar_nlmixr_multimodel.R must be updated.
  if (rxode2::.matchesLangTemplate(object, str2lang(".name(initial) <- ."))) {
    object[[2]][[2]] <- 0
  } else if (is.call(object)) {
    for (idx in seq_along(object)) {
      object[[idx]] <- nlmixr_object_simplify_zero_initial_helper(object[[idx]])
    }
  }
  object
}

#' Standardize and simplify data for nlmixr2 estimation
#'
#' This function is typically not needed by end users.
#'
#' The standardization keeps columns that rxode2 and nlmixr2 use along with the
#' covariates.  Column order is standardized (rxode2 then nlmixr2 then
#' alphabetically sorted covariates), and rxode2 and nlmixr2 column names are
#' converted to lower case.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @param object an nlmixr_ui object (e.g. the output of running
#'   \code{nlmixr(object = model)}
#' @returns The data with the nlmixr2 column lower case and on the left and the
#'   covariate columns on the right and alphabetically sorted.
#' @family Simplifiers
#' @export
nlmixr_data_simplify <- function(data, object, table = list()) {
  checkmate::assert_data_frame(data)
  checkmate::assert_list(table)
  if (is.character(object)) {
    # load from the hash
    object <- read_nlmixr2obj_indirect(hash = object)
  }
  nlmixr_cols <-
    c(
      # rxode2 columns
      c("id", "time", "amt", "rate", "dur", "evid", "cmt", "dvid", "ss", "ii", "addl"),
      # nlmixr2 columns
      c("dv", "mdv", "cens", "limit")
    )
  # nlmixr pays attention to the columns in a case-insensitive way for the
  # standard columns.  Verify that the data has case-insensitive column names
  # for these columns (for example not "ADDL" and "addl").
  mask_nlmixr_cols <- tolower(names(data)) %in% nlmixr_cols
  nlmixr_names <- names(data)[mask_nlmixr_cols]
  mask_duplicated <- duplicated(tolower(nlmixr_names))
  if (any(mask_duplicated)) {
    stop(
      "The following column(s) are duplicated when lower case: ",
      paste0("'", nlmixr_names[mask_duplicated], "'", collapse = ", ")
    )
  }
  if (!is.null(object$ui)) {
    covVec <- object$ui$all.covs
  } else {
    covVec <- object$all.covs
  }
  cov_names <- nlmixr_data_simplify_cols(data, cols = covVec, type = "covariate")
  keep_names <- nlmixr_data_simplify_cols(data, cols = table$keep, type = "keep")
  # Simplifying the nlmixr_names column names to always be lower case ensures
  # that upper/lower case column name changes will not affect the need to rerun.
  # Also, standardizing the column name order to always be the same will prevent
  # the need to rerun, so cov_names is sorted.

  # Sorting so that they are in order, unique so that duplication between
  # covariates and keep do not try to duplicate columns in the output data.
  add_col_names <- sort(unique(c(cov_names, keep_names)))

  # Drop names from nlmixr_names from the added names
  add_col_names <- setdiff(add_col_names, nlmixr_names)

  stats::setNames(
    object = data[, c(nlmixr_names, add_col_names), drop = FALSE],
    nm = c(tolower(nlmixr_names), add_col_names)
  )
}

nlmixr_data_simplify_cols <- function(data, cols, type) {
  missing_col <- setdiff(cols, names(data))
  if (length(missing_col) > 0) {
    stop(
      "The following ", type, " column(s) are missing from the data: ",
      paste0("'", missing_col, "'", collapse = ", ")
    )
  }
  cols
}
