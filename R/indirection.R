#' Estimate an nlmixr2 model loading the model from a targets indirect hash storage
#'
#' This is not intended for direct use by users
#' @inheritParams nlmixr2est::nlmixr
#' @export
nlmixr2_indirect <- function(object, data, est, control) {
  nlmixr2est::nlmixr(object = read_nlmixr2obj_indirect(hash = object), data = data, est = est, control = control)
}

read_nlmixr2obj_indirect <- function(hash, directory = "_targets/nlmixr2") {
  readRDS(file.path(directory, hash))
}

save_nlmixr2obj_indirect <- function(object, directory = "_targets/nlmixr2") {
  if (!dir.exists(directory)) {
    dir.create(path = directory, showWarnings = FALSE, recursive = TRUE)
  }
  saveRDS(object = object, file = file.path(directory, object$md5))
  object$md5
}
