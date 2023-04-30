#' Generate a set of targets for nlmixr estimation
#'
#' The targets generated will include the \code{name} as the final estimation
#' step, \code{paste(name, "object_simple", sep = "_tar_")} (e.g.
#' "pheno_tar_object_simple") as the simplified model object, and
#' \code{paste(name, "data_simple", sep = "_tar_")} (e.g. "pheno_tar_data_simple")
#' as the simplified data object.
#'
#' For the way that the objects are simplified, see
#' \code{\link{nlmixr_object_simplify}()} and
#' \code{\link{nlmixr_data_simplify}()}.  To see how to write initial conditions
#' to work with targets, see \code{\link{nlmixr_object_simplify}()}.
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
  name_obj_simple <- paste(name, "object_simple", sep = "_tar_")
  name_data_simple <- paste(name, "data_simple", sep = "_tar_")
  ret <-
    list(
      targets::tar_target_raw(
        name = name_obj_simple,
        command =
          substitute(
            nlmixr_object_simplify(object = object),
            list(object = substitute(object))
          ),
        packages = "nlmixr2est"
      ),
      targets::tar_target_raw(
        name = name_data_simple,
        command =
          substitute(
            nlmixr_data_simplify(object = object_simple, data = data, table = table),
            list(
              object_simple = as.name(name_obj_simple),
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
              object = object_simple,
              data = data_simple,
              est = est,
              control = control
            ),
            list(
              object_simple = as.name(name_obj_simple),
              data_simple = as.name(name_data_simple),
              est = substitute(est),
              control = substitute(control),
              table = substitute(table)
            )
          ),
        packages = "nlmixr2est"
      )
    )
  ret
}
