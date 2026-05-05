# Generate a set of targets for nlmixr estimation

The targets generated will include the `name` as the final estimation
step, `paste(name, "object_simple", sep = "_tar_")` (e.g.
"pheno_tar_object_simple") as the simplified model object, and
`paste(name, "data_simple", sep = "_tar_")` (e.g.
"pheno_tar_data_simple") as the simplified data object.

## Usage

``` r
tar_nlmixr(
  name,
  object,
  data,
  est = NULL,
  control = list(),
  table = nlmixr2est::tableControl(),
  env = parent.frame()
)

tar_nlmixr_raw(
  name,
  object,
  data,
  est,
  control,
  table,
  object_simple_name,
  data_simple_name,
  fit_simple_name,
  env
)
```

## Arguments

- name:

  Symbol, name of the target. In
  [`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html),
  `name` is an unevaluated symbol, e.g. `tar_target(name = data)`. In
  [`tar_target_raw()`](https://docs.ropensci.org/targets/reference/tar_target.html),
  `name` is a character string, e.g. `tar_target_raw(name = "data")`.

  A target name must be a valid name for a symbol in R, and it must not
  start with a dot. Subsequent targets can refer to this name
  symbolically to induce a dependency relationship: e.g.
  `tar_target(downstream_target, f(upstream_target))` is a target named
  `downstream_target` which depends on a target `upstream_target` and a
  function `f()`.

  In most cases, The target name is the name of its local data file in
  storage. Some file systems are not case sensitive, which means
  converting a name to a different case may overwrite a different
  target. Please ensure all target names have unique names when
  converted to lower case.

  In addition, a target's name determines its random number generator
  seed. In this way, each target runs with a reproducible seed so
  someone else running the same pipeline should get the same results,
  and no two targets in the same pipeline share the same seed. (Even
  dynamic branches have different names and thus different seeds.) You
  can recover the seed of a completed target with
  `tar_meta(your_target, seed)` and run
  [`tar_seed_set()`](https://docs.ropensci.org/targets/reference/tar_seed_set.html)
  on the result to locally recreate the target's initial RNG state.

- object:

  Fitted object or function specifying the model.

- data:

  nlmixr data

- est:

  estimation method (all methods are shown by \`nlmixr2AllEst()\`).
  Methods can be added for other tools

- control:

  The estimation control object. These are expected to be different for
  each type of estimation method

- table:

  The output table control object (like \`tableControl()\`)

- env:

  The environment where the model is setup (not needed for typical use)

- object_simple_name, data_simple_name, fit_simple_name:

  target names to use for the simplified object, simplified data, fit of
  the simplified object with the simplified data, and fit with the
  original data re-inserted.

## Value

A list of targets for the model simplification, data simplification, and
model estimation.

## Details

For the way that the objects are simplified, see
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md)
and
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md).
To see how to write initial conditions to work with targets, see
[`nlmixr_object_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_object_simplify.md).

## Functions

- `tar_nlmixr_raw()`: An internal function to generate the targets

## Examples

``` r
if (FALSE) { # \dontrun{
library(targets)
targets::tar_script({
pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <-  log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl/vc
    d/dt(central) <- -kel*central
    cp <- central/vc
    cp ~ add(cpaddSd)
  })
}
list(
  tar_nlmixr(
    name = pheno_model,
    object = pheno,
    data = nlmixr2data::pheno_sd,
    est = "saem"
  )
)
})
targets::tar_make()
} # }
```
