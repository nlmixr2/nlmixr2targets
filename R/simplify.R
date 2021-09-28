#' Simplify an nlmixr object
#'
#' This function is typically not needed by end users.
#'
#' The object simplification removes comments (so please use \code{label()}
#' instead of comments to label parameters) and then converts the \code{object}
#' to a "nlmixrui" object.
#'
#' @inheritParams nlmixr::nlmixr
#' @return \code{object} converted to a nlmixrui object.  The model name is
#'   always "object".
#' @family Simplifiers
#' @export
nlmixr_object_simplify <- function(object) {
  # drop comments since they won't affect most outputs and typically change
  # between interactive (when srcref is available) to batch (when srcref is not
  # available) mode.
  attr(object, "srcref") <- NULL
  ret <- nlmixr::nlmixr(object)
  # TODO: ret$model.name is always "object"; it may be better to set it to
  # as.character(substitute(object)), but that didn't work with initial testing.
  # (Or, maybe it's better to have it just be "object" so that it is simpler.)
  ret
}

#' Standardize and simplify data for nlmixr estimation
#'
#' This function is typically not needed by end users.
#'
#' The standardization keeps columns that RxODE and nlmixr use along with the
#' covariates.  Column order is standardized (RxODE then nlmixr then
#' alphabetically sorted covariates), and RxODE and nlmixr column names are
#' converted to lower case.
#'
#' @inheritParams nlmixr::nlmixr
#' @param object an nlmixr_ui object (e.g. the output of running
#'   \code{nlmixr(object=model)}
#' @return The data with the nlmixr column lower case and on the left and the
#'   covariate columns on the right and alphabetically sorted.
#' @family Simplifiers
#' @export
nlmixr_data_simplify <- function(data, object) {
  nlmixr_cols <-
    c(
      # RxODE columns
      c("id", "time", "amt", "rate", "dur", "evid", "cmt", "ss", "ii", "addl"),
      # nlmixr columns
      c("dv", "dvid", "mdv")
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
      paste0("'", nlmixr_names[mask_duplicated], "'", collapse=", ")
    )
  }
  cov_names <- object$all.covs
  missing_cov <- setdiff(cov_names, names(data))
  if (length(missing_cov) > 0) {
    stop(
      "The following covariate column(s) are missing from the data: ",
      paste0("'", missing_cov, "'", collapse=", ")
    )
  }
  # Simplifying the nlmixr_names column names to always be lower case ensures
  # that upper/lower case column name changes will not affect the need to rerun.
  # Also, standardizing the column name order to always be the same will prevent
  # the need to rerun, so cov_names is sorted.
  setNames(
    object=data[, c(nlmixr_names, sort(cov_names)), drop=FALSE],
    nm=c(tolower(nlmixr_names), sort(cov_names))
  )
}
