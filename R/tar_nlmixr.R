#' Generate a set of targets for nlmixr estimation
#'
#' The targets generated will include the `name` as the final estimation step,
#' `paste(name, "object_simple", sep = "_tar_")` (e.g.
#' "pheno_tar_object_simple") as the simplified model object, and
#' `paste(name, "data_simple", sep = "_tar_")` (e.g. "pheno_tar_data_simple") as
#' the simplified data object.
#'
#' For the way that the objects are simplified, see `nlmixr_object_simplify()`
#' and `nlmixr_data_simplify()`.  To see how to write initial conditions to work
#' with targets, see `nlmixr_object_simplify()`.
#'
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @return A list of targets for the model simplification, data simplification,
#'   and model estimation.
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
#' list(
#'   tar_nlmixr(
#'     name = pheno_model,
#'     object = pheno,
#'     data = nlmixr2data::pheno_sd,
#'     est = "saem"
#'   )
#' )
#' })
#' targets::tar_make()
#' }
#' @export
tar_nlmixr <- function(name, object, data, est = NULL, control = list(), table = nlmixr2est::tableControl()) {
  if (is.null(est)) {
    stop("'est' must not be null")
  }
  name <- targets::tar_deparse_language(substitute(name))
  tar_nlmixr_raw(
    name = name,
    object = object,
    data = data,
    est = est,
    control = control,
    table = table,
    object_simple_name = paste(name, "object_simple", sep = "_tar_"),
    data_simple_name = paste(name, "data_simple", sep = "_tar_")
  )
}

#' @describeIn tar_nlmixr An internal function to generate the targets
#' @param object_simple_name,data_simple_name target names to use for the object
#'   and data
#' @export
tar_nlmixr_raw <- function(name, object, data, est, control, table, object_simple_name, data_simple_name) {
  list(
    targets::tar_target_raw(
      name = object_simple_name,
      command =
        substitute(
          nlmixr_object_simplify(object = object),
          list(object = substitute(object))
        ),
      packages = "nlmixr2est"
    ),
    targets::tar_target_raw(
      name = data_simple_name,
      command =
        substitute(
          nlmixr_data_simplify(object = object_simple, data = data, table = table),
          list(
            object_simple = as.name(object_simple_name),
            data = substitute(data),
            table = substitute(table)
          )
        )
    ),
    targets::tar_target_raw(
      name = name,
      command =
        substitute(
          nlmixr2est::nlmixr(
            object = object_simple_name,
            data = data_simple_name,
            est = est,
            control = control
          ),
          list(
            object_simple = as.name(object_simple_name),
            data_simple = as.name(name_data_simple),
            est = substitute(est),
            control = substitute(control),
            table = substitute(table)
          )
        ),
      packages = "nlmixr2est"
    )
  )
}
