# Walk a captured expression and collect symbol names that could refer
# to user-defined functions. Skips the head of every call (`f(x)` -> we
# do not add `f`), since the head is the function being called, not a
# candidate model function. The model function appears either as the
# whole expression (`object = pheno`), as a `|>` desugared first arg
# (`object = pheno |> model({...})` -> `model(pheno, ...)`, first arg
# is `pheno`), or as a nested non-head symbol.
#
# Shared by `tar_nlmixr_protect_zero_initial()` (construction time) and
# `nlmixr_object_zero_initial_eval()` (runtime), which both need to
# locate user functions referenced by a captured expression.
.collect_top_symbols <- function(expr) {
  out <- character()
  visit <- function(e, is_head = FALSE) {
    if (is.symbol(e) && !is_head) {
      out[[length(out) + 1L]] <<- as.character(e)
    } else if (is.call(e)) {
      visit(e[[1L]], is_head = TRUE)
      for (idx in seq_along(e)[-1L]) visit(e[[idx]])
    }
  }
  visit(expr)
  unique(out)
}
