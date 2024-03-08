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
#' @return \code{object} converted to a nlmixrui object.  The model name is
#'   always "object".
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
  ret
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
