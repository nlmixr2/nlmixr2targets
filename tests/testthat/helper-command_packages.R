# A worker (a crew worker, a fresh container) runs a target after attaching
# only R's default packages and the target's own `packages`.  A local
# `tar_make()` also attaches the packages of every upstream target whose value
# it reads, so a target that omits a package it calls still passes there.
# These helpers check the declaration statically instead.

# R's default attached packages, as a freshly started `Rscript` has them.
worker_default_packages <-
  c("base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

# Names of the functions an expression calls without a namespace.  A
# `pkg::fun()` or `pkg:::fun()` head is resolved by its namespace, so it is
# skipped while its arguments are still walked; the argument of `quote()` is
# data, not a call to evaluate.
command_bare_calls <- function(expr) {
  if (!is.call(expr)) {
    return(character())
  }
  head <- expr[[1L]]
  if (identical(head, quote(quote))) {
    return(character())
  }
  if (is.symbol(head) && as.character(head) %in% c("::", ":::")) {
    return(character())
  }
  own <- if (is.symbol(head)) as.character(head) else command_bare_calls(head)
  args <- lapply(as.list(expr)[-1L], command_bare_calls)
  unique(c(own, unlist(args)))
}

# The un-namespaced functions in a target's command that a worker could not
# find after attaching `worker_default_packages` and the target's `packages`.
command_unresolved_calls <- function(target) {
  available <-
    unlist(lapply(
      c(worker_default_packages, target$command$packages),
      getNamespaceExports
    ))
  setdiff(command_bare_calls(target$command$expr[[1L]]), available)
}
