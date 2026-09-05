#' Generate a set of targets for nlmixr estimation
#'
#' The targets generated will include the `name` as the final estimation step,
#' `paste(name, "object_simple", sep = "_")` (e.g.
#' `"pheno_object_simple"`) as the simplified model object, and
#' `paste(name, "data_simple", sep = "_")` (e.g. `"pheno_data_simple"`) as
#' the simplified data object.
#'
#' For the way that the objects are simplified, see `nlmixr_object_simplify()`
#' and `nlmixr_data_simplify()`.  To see how to write initial conditions to work
#' with targets, see `nlmixr_object_simplify()`.
#'
#' The simplified data keep the columns that the estimation method reads.  For
#' most methods that is the standard event columns plus the covariates named
#' in the model; `est = "vae"` additionally searches subject-constant data
#' columns during its automated covariate selection, so those candidate
#' columns are kept, too (see `nlmixr_data_simplify()`).  Changes to any kept
#' column invalidate the cached fit; changes to dropped columns do not.
#'
#' `table` does two jobs: `table$keep` names data columns to carry through to
#' the simplified data, and a non-default `tableControl()` is handed to
#' [nlmixr2est::nlmixr()] so the residual/table step honors it.  Left at its
#' default, `table` is omitted from the generated command entirely.  That
#' keeps the command byte-identical to what earlier versions produced, so
#' upgrading does not re-run cached fits, and it preserves nlmixr2est's merge
#' of the table settings carried on `ui$meta` (`addDosing`, `subsetNonmem`,
#' `cores`, `keep`, `drop`), which happens only when its own `table` argument
#' is missing.  Supplying any other `tableControl()` changes the command and
#' re-runs the fit, as a changed `control` would.
#'
#' @section Side effects:
#' When the user's model function body contains `cmt(0) <- value` inside a
#' `model({...})` block, `tar_nlmixr()` rewrites those lines to
#' `cmt(initial) <- value` directly in the function's binding in `env` so that
#' `targets`' static analysis (which walks every function in env via
#' `codetools::findGlobals()`) accepts the model. The rewrite is reversed at
#' evaluation time, so fitting and downstream behaviour are unchanged. The
#' user-visible consequence is that printing `body(my_model)` at the REPL
#' after a call to `tar_nlmixr()` will show `cmt(initial)` rather than the
#' originally-written `cmt(0)`.
#'
#' Manual `cmt(initial) <- value` written by the user is also accepted, but
#' it is a `nlmixr2targets`-only workaround: bare nlmixr2 does not
#' understand the `cmt(initial)` form, so a model function written that way
#' only fits when routed through `tar_nlmixr()` (or
#' `tar_nlmixr_multimodel()`).
#'
#' @inheritParams nlmixr2est::nlmixr
#' @inheritParams targets::tar_target
#' @param env The environment where the model is setup (not needed for typical
#'   use)
#' @param error What should happen if the estimation step throws an error?
#'   `"stop"` (the default) lets the error propagate, halting
#'   `targets::tar_make()` as usual. `"continue"` catches the error and stores
#'   a failure sentinel (an object of class `nlmixr2targetsError`, which also
#'   inherits from `"try-error"`) carrying the error message, so a single
#'   failed model does not stop the rest of the pipeline. Detect a failed fit
#'   with `inherits(fit, "nlmixr2targetsError")` or the broader
#'   `inherits(fit, "try-error")`.
#' @returns A list of targets for the model simplification, data simplification,
#'   and model estimation.
#' @seealso [tar_nlmixr_multimodel()] for fitting many models against one
#'   dataset.
#' @examples
#' pheno <- function() {
#'   ini({
#'     lcl <- log(0.008); label("Typical value of clearance")
#'     lvc <- log(0.6); label("Typical value of volume of distribution")
#'     etalcl + etalvc ~ c(1,
#'                         0.01, 1)
#'     cpaddSd <- 0.1; label("residual variability")
#'   })
#'   model({
#'     cl <- exp(lcl + etalcl)
#'     vc <- exp(lvc + etalvc)
#'     kel <- cl / vc
#'     d / dt(central) <- -kel * central
#'     cp <- central / vc
#'     cp ~ add(cpaddSd)
#'   })
#' }
#'
#' # Build the four targets that estimate `pheno`. `data` and `est` are
#' # captured as expressions, so this just returns the target list; the
#' # estimation step runs only when you call `targets::tar_make()` from a
#' # project whose targets store you have configured (for example, with
#' # `targets::tar_config_set(store = file.path(tempdir(), "_targets"))`
#' # or by running inside a project directory you own).
#' tar_nlmixr(
#'   name = pheno_model,
#'   object = pheno,
#'   data = nlmixr2data::pheno_sd,
#'   est = "saem"
#' )
#' @export
tar_nlmixr <- function(name, object, data, est = NULL, control = list(),
                       table = nlmixr2est::tableControl(), env = parent.frame(),
                       error = c("stop", "continue")) {
  if (is.null(est)) {
    stop("'est' must not be null")
  }
  error <- match.arg(error)
  checkmate::assert_environment(env)
  name_parsed <- targets::tar_deparse_language(substitute(name))
  tar_nlmixr_raw(
    name = name_parsed,
    object = substitute(object),
    data = substitute(data),
    est = substitute(est),
    control = substitute(control),
    table = substitute(table),
    object_simple_name = paste(name_parsed, "object_simple", sep = "_"),
    data_simple_name = paste(name_parsed, "data_simple", sep = "_"),
    fit_simple_name = paste(name_parsed, "fit_simple", sep = "_"),
    env = env,
    error = error
  )
}

#' @describeIn tar_nlmixr An internal function to generate the targets
#' @param object_simple_name,data_simple_name,fit_simple_name target names to
#'   use for the simplified object, simplified data, fit of the simplified
#'   object with the simplified data, and fit with the original data
#'   re-inserted.
#' @param description Human-readable name for the model, announced by the
#'   estimation target when it starts to run.  `NULL` (the default) announces
#'   nothing.
#' @export
tar_nlmixr_raw <- function(name, object, data, est, control, table,
                           object_simple_name, data_simple_name, fit_simple_name, env,
                           error = "stop", description = NULL) {
  checkmate::assert_character(name, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assert_character(object_simple_name, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assert_character(data_simple_name, len = 1, min.chars = 1, any.missing = FALSE)
  checkmate::assert_choice(error, choices = c("stop", "continue"))
  checkmate::assert_string(description, min.chars = 1, null.ok = TRUE)

  object <- tar_nlmixr_protect_zero_initial(object, env = env)

  # Build the cache-directory expression once and splice it into each
  # generated target command, mirroring how `targets::tar_make()` and
  # friends default `store = targets::tar_config_get("store")`. The path
  # resolves at target-execution time, so the cache always sits inside
  # whatever store the user has configured for their `tar_make()` call.
  directory_expr <- quote(file.path(targets::tar_config_get("store"), "user/nlmixr2"))

  fit_simple_command <-
    substitute(
      nlmixr2_indirect(
        object = object_simple_name,
        data = data_simple_name,
        est = est,
        control = control,
        directory = directory,
        error = error
      ),
      list(
        object_simple_name = as.name(object_simple_name),
        data_simple_name = as.name(data_simple_name),
        est = est,
        control = control,
        directory = directory_expr,
        error = error
      )
    )
  # `table` reaches the estimation step only when the user asked for something
  # other than the default.  Splicing `table = nlmixr2est::tableControl()` in
  # unconditionally would be behaviour-neutral (it is what
  # `nlmixr2est::nlmixr()` uses anyway) but would change the deparsed command
  # of every existing pipeline and re-run every cached fit, which is the one
  # cost this package exists to avoid.
  if (!tar_nlmixr_table_is_default(table)) {
    fit_simple_command$table <- table
  }
  # `targets` decides whether a target is out of date by hashing the deparsed
  # command.  Hashing the description-free command (what `tar_target_raw()`
  # would have produced on its own) keeps a renamed model from re-running its
  # fit; `string` exists for exactly this kind of external control.
  fit_simple_string <- targets::tar_deparse_language(as.expression(fit_simple_command))
  if (!is.null(description)) {
    fit_simple_command$description <- description
  }

  list(
    object_simple =
      targets::tar_target_raw(
        name = object_simple_name,
        command = substitute(
          nlmixr_object_simplify(object = object, directory = directory),
          list(object = object, directory = directory_expr)
        ),
        packages = c("nlmixr2targets", "nlmixr2est")
      ),
    data_simple =
      targets::tar_target_raw(
        name = data_simple_name,
        command = substitute(
          nlmixr_data_simplify(
            object = object_simple, data = data, table = table,
            directory = directory, est = est, control = control
          ),
          list(
            object_simple = as.name(object_simple_name),
            data = data,
            table = table,
            directory = directory_expr,
            est = est,
            control = control
          )
        ),
        # nlmixr2est so that un-namespaced `control`/`table` expressions
        # (e.g. `vaeControl(...)`, `tableControl(...)`) evaluate in the
        # target's session, as they already do in the fit_simple target.
        packages = c("nlmixr2targets", "nlmixr2est")
      ),
    fit_simple =
      targets::tar_target_raw(
        name = fit_simple_name,
        command = fit_simple_command,
        string = fit_simple_string,
        packages = "nlmixr2est"
      ),
    fit =
      targets::tar_target_raw(
        name = name,
        command = substitute(
          nlmixr_object_complicate(fit = fit, object = object, data = data),
          list(
            fit = as.name(fit_simple_name),
            object = object,
            data = data
          )
        ),
        packages = "nlmixr2targets"
      )
  )
}

#' Is the captured `table` expression the one `tar_nlmixr()` defaults to?
#'
#' `table` is captured unevaluated, so the comparison is between language
#' objects rather than values.  An unsupplied `table` yields the formal
#' default expression, `nlmixr2est::tableControl()`; matching only that exact
#' expression is what makes omitting it from the generated command provably
#' identical to letting `nlmixr2est::nlmixr()` apply its own default.  Any
#' other spelling -- including a bare `tableControl()`, which could in
#' principle resolve to something else -- is treated as user-supplied and is
#' forwarded (and therefore hashed).
#'
#' @param table The captured `table` expression.
#' @returns `TRUE` when `table` is the default expression.
#' @noRd
tar_nlmixr_table_is_default <- function(table) {
  identical(table, quote(nlmixr2est::tableControl()))
}

#' Replace the fit data with the original data, then return the modified fit
#'
#' This function is intended for use within `nlmixr2targets` target creation,
#' and it's not typically invoked by users.
#'
#' @param fit an estimated `nlmixr2` object
#' @param data the data from the original fit
#' @returns The fit with the data added back in as `fit$env$origData`
#' @keywords Internal
#' @export
assign_origData <- function(fit, data) {
  checkmate::assert_environment(fit$env)
  checkmate::assert_data_frame(fit$env$origData)
  # The data being replaced must have the same number of rows as the original
  # data
  checkmate::assert_data_frame(data, nrows = nrow(fit$env$origData))
  assign(x = "origData", value = data, envir = fit$env)
  fit
}

#' Construction-time protection that makes user-written `cmt(0)` syntax
#' safe for `targets`' static analysis.
#'
#' Three things happen:
#' \enumerate{
#'   \item For each symbol in the captured `object` expression that
#'   resolves to a function in `env`, walk its body and rewrite
#'   `name(0) <- val` to `name(initial) <- val` inside any
#'   `model({...})` block. The rewritten function is assigned back into
#'   the same env where it came from -- yes, this mutates the user's
#'   binding. It is the only path that survives `targets`' env-wide
#'   function walk (`codetools::findGlobals()` is called on every
#'   function in env, even ones unreferenced by any target).
#'   \item Rewrite the captured expression itself the same way, so any
#'   inline `model({eff(0) <- ...})` block (e.g. on the RHS of a `|>`
#'   pipe) is also safe.
#'   \item If anything was rewritten, wrap the captured expression in
#'   a `nlmixr_object_zero_initial_eval(quote(...))` call so the
#'   runtime restores the `cmt(0)` form before the expression is
#'   evaluated. Needed for pipe forms; harmless for symbol-only forms.
#' }
#'
#' @param object The captured `object` expression from
#'   `tar_nlmixr_raw()`.
#' @param env The user's environment (`parent.frame()` of
#'   `tar_nlmixr()`).
#' @returns Either the original `object` (no rewrites needed) or a
#'   `bquote()`d call to `nlmixr_object_zero_initial_eval()` that
#'   restores the user's natural syntax at target execution time.
#' @noRd
tar_nlmixr_protect_zero_initial <- function(object, env) {
  # Pass 1: rewrite every user-defined function referenced by `object` in
  # env from cmt(0) to cmt(initial), reporting how many rewrites happened
  # and whether any referenced body already carries cmt(initial).
  pass1 <- tar_nlmixr_protect_zero_initial_env(object, env)

  # Pass 2: rewrite the captured expression itself.
  res2 <- nlmixr_object_protect_zero_initial(object)
  object <- res2$expr
  total_n <- pass1$total_n + res2$n_rewrites

  # Wrap when this call rewrote something, or when a non-symbol (pipe)
  # expression references a function already carrying cmt(initial). A bare
  # symbol never needs the referenced-only wrap: nlmixr_object_simplify()
  # converts cmt(initial) back to cmt(0) on the function body itself, so
  # only expressions evaluated *before* simplification (pipes) are at risk.
  if (total_n == 0L && !(pass1$referenced_initial && is.call(object))) {
    return(object)
  }

  # Pass 3: wrap so runtime restores cmt(0) before evaluation.
  bquote(
    nlmixr2targets::nlmixr_object_zero_initial_eval(quote(.(x))),
    list(x = object)
  )
}

#' Pass 1 of `tar_nlmixr_protect_zero_initial()`: rewrite referenced
#' functions' bodies in env and report the wrapping signals.
#'
#' Every symbol in the captured expression that points to a user-defined
#' function in env gets its body rewritten in place from `cmt(0)` to
#' `cmt(initial)`. `assign(..., inherits = TRUE)` walks env upward and
#' modifies the binding in the frame where it was originally found, so the
#' rewrite reaches `globalenv()` -- which is what `targets` walks. Package
#' functions are skipped: we never want to mutate, say, `rxode2::ini` or
#' `base::model`, and codetools accepts their (DSL-free) bodies as-is.
#'
#' @param object The captured `object` expression.
#' @param env The user's environment.
#' @returns A list with `total_n` (count of `cmt(0)` rewrites performed)
#'   and `referenced_initial` (`TRUE` if any referenced function body
#'   already carries `cmt(initial)` inside a `model({...})` block even
#'   though this call rewrote nothing -- e.g. a sibling multimodel entry
#'   already mutated the shared function, or the user hand-wrote the
#'   `cmt(initial)` workaround; issue #37).
#' @noRd
tar_nlmixr_protect_zero_initial_env <- function(object, env) {
  total_n <- 0L
  referenced_initial <- FALSE
  for (sym in .collect_top_symbols(object)) {
    if (!exists(sym, envir = env, inherits = TRUE)) next
    fn <- get(sym, envir = env, inherits = TRUE)
    fn_env <- if (is.function(fn)) environment(fn) else NULL
    if (!is.function(fn) || is.null(base::body(fn)) || is.null(fn_env) || isNamespace(fn_env)) next
    res <- nlmixr_object_protect_zero_initial(base::body(fn))
    if (res$n_rewrites > 0L) {
      base::body(fn) <- res$expr
      assign(sym, fn, envir = env, inherits = TRUE)
      total_n <- total_n + res$n_rewrites
    }
    # Detect an already-rewritten (or hand-written) cmt(initial) body,
    # which res$n_rewrites cannot report because there was no cmt(0)
    # left to rewrite.
    if (nlmixr_object_body_has_model_initial(base::body(fn))) {
      referenced_initial <- TRUE
    }
  }
  list(total_n = total_n, referenced_initial = referenced_initial)
}
