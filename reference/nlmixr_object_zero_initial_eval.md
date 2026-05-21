# Runtime helper: undo the `cmt(0) -> cmt(initial)` rewrite before evaluating the captured `object` expression.

Wrapped around the captured expression at construction time when
`tar_nlmixr_protect_zero_initial()` performed any rewrites. Needed for
pipe forms like `pheno |> model({...})` and `pheno |> ini(...)`, because
nlmixr2's pipe handlers parse pheno's body before the existing
`nlmixr_object_simplify_zero_initial_helper()` could intervene. Harmless
for symbol-only forms (the existing helper reaches them at simplify time
anyway).

## Usage

``` r
nlmixr_object_zero_initial_eval(expr, envir = parent.frame())
```

## Arguments

- expr:

  A quoted (unevaluated) language object.

- envir:

  The parent environment for evaluation. Defaults to the caller's frame
  (the targets execution env).

## Value

The result of evaluating the corrected expression.

## Details

Mechanism:

- Walk the quoted expression, rewriting `name(initial) <- val` back to
  `name(0) <- val` via the existing inverse helper.

- Walk the (rewritten) expression for symbols. For each that resolves to
  a function whose body still contains `name(initial) <- val` inside
  `model({...})`, build a corrected closure with `name(0) <- val` and
  bind it in an override frame.

- Evaluate the rewritten expression in the override frame (whose parent
  is `envir`), so any lookup of a rewritten symbol hits the corrected
  closure first.

The corrected copies retain the original closure environment of the
user's function; only the body is swapped.
