#' Simplify an nlmixr object
#'
#' This function is typically not needed by end users.
#'
#' The object simplification removes comments (so please use \code{label()}
#' instead of comments to label parameters) and then converts the \code{object}
#' to a "nlmixrui" object.
#'
#' Since setting initial conditions with `cmt(0)` does not work with `targets`,
#' the function definition of the object must set it with `cmt(initial)`.
#' `cmt(initial)` will be converted to `cmt(0)` before passing to nlmixr2.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @returns The hash to be able to load `object` from the converted to a
#'   nlmixrui object. The model name is always "object".
#' @family Simplifiers
#' @export
nlmixr_object_simplify <- function(object) {
  # drop comments since they won't affect most outputs and typically change
  # between interactive (when srcref is available) to batch (when srcref is not
  # available) mode.
  attr(object, "srcref") <- NULL
  object <- nlmixr_object_simplify_zero_initial(object)
  ret <- nlmixr2est::nlmixr(object)
  # TODO: ret$model.name is always "object"; it may be better to set it to
  # as.character(substitute(object)), but that didn't work with initial testing.
  # (Or, maybe it's better to have it just be "object" so that it is simpler.)
  save_nlmixr2obj_indirect(ret)
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
