#' Estimate an nlmixr2 model loading the model from a targets indirect hash storage
#'
#' This is not intended for direct use by users
#' @inheritParams nlmixr2est::nlmixr
#' @export
nlmixr2_indirect <- function(object, data, est, control) {
  nlmixr2est::nlmixr(object = read_nlmixr2obj_indirect(hash = object), data = data, est = est, control = control)
}

# Indirect cache layout
#
# Simplified `nlmixrui` objects are serialized as RDS files keyed by their
# md5 hash. The cache lives next to the targets store so it follows the
# same project-local conventions:
#
#   <tar_config_get("store")>/user/nlmixr2/<md5>
#
# `save_nlmixr2obj_indirect()` writes the object and returns the md5 (which
# the caller passes through targets as a small character target).
# `read_nlmixr2obj_indirect()` loads it back. Both are unexported because
# only `nlmixr_object_simplify()` and `nlmixr2_indirect()` should be touching
# this storage.

read_nlmixr2obj_indirect <- function(hash, directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")) {
  checkmate::assert_string(hash, min.chars = 1)
  path <- file.path(directory, hash)
  if (!file.exists(path)) {
    stop(
      "nlmixr2targets indirect cache miss: no object with hash '", hash,
      "' under '", directory, "'"
    )
  }
  readRDS(path)
}

save_nlmixr2obj_indirect <- function(object, directory = file.path(targets::tar_config_get("store"), "user/nlmixr2")) {
  if (!dir.exists(directory)) {
    dir.create(path = directory, recursive = TRUE)
  }
  saveRDS(object = object, file = file.path(directory, object$md5))
  object$md5
}
