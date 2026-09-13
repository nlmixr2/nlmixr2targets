# Generate a set of targets for nlmixr estimation

The targets generated will include the `name` as the final estimation
step, `paste(name, "object_simple", sep = "_")` (e.g.
`"pheno_object_simple"`) as the simplified model object, and
`paste(name, "data_simple", sep = "_")` (e.g. `"pheno_data_simple"`) as
the simplified data object.

## Usage

``` r
tar_nlmixr(
  name,
  object,
  data,
  est = NULL,
  control = list(),
  table = nlmixr2est::tableControl(),
  env = parent.frame(),
  error = c("stop", "continue"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  library = targets::tar_option_get("library"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
  deployment = targets::tar_option_get("deployment"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
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
  env,
  error = "stop",
  description = NULL,
  target_settings = tar_nlmixr_collect_target_settings_default()
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

- error:

  What should happen if the estimation step throws an error? `"stop"`
  (the default) lets the error propagate, halting
  [`targets::tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
  as usual. `"continue"` catches the error and stores a failure sentinel
  (an object of class `nlmixr2targetsError`, which also inherits from
  `"try-error"`) carrying the error message, so a single failed model
  does not stop the rest of the pipeline. Detect a failed fit with
  `inherits(fit, "nlmixr2targetsError")` or the broader
  `inherits(fit, "try-error")`.

- format:

  Optional storage format for the target's return value. With the
  exception of `format = "file"`, each target gets a file in
  `_targets/objects`, and each format is a different way to save and
  load this file. See the "Storage formats" section for a detailed list
  of possible data storage formats.

- repository:

  Character of length 1, remote repository for target storage. Choices:

  - `"local"`: file system of the local machine.

  - `"aws"`: Amazon Web Services (AWS) S3 bucket. Can be configured with
    a non-AWS S3 bucket using the `endpoint` argument of
    [`tar_resources_aws()`](https://docs.ropensci.org/targets/reference/tar_resources_aws.html),
    but versioning capabilities may be lost in doing so. See the cloud
    storage section of <https://books.ropensci.org/targets/data.html>
    for details for instructions.

  - `"gcp"`: Google Cloud Platform storage bucket. See the cloud storage
    section of <https://books.ropensci.org/targets/data.html> for
    details for instructions.

  - A character string from
    [`tar_repository_cas()`](https://docs.ropensci.org/targets/reference/tar_repository_cas.html)
    for content-addressable storage.

  Note: if `repository` is not `"local"` and `format` is `"file"` then
  the target should create a single output file. That output file is
  uploaded to the cloud and tracked for changes where it exists in the
  cloud. As of `targets` version 1.11.0 and higher, the local file is no
  longer deleted after the target runs.

- library:

  Character vector of library paths to try when loading `packages`.

- memory:

  Character of length 1, memory strategy. Possible values:

  - `"auto"` (default): equivalent to `memory = "transient"` in almost
    all cases. But to avoid superfluous reads from disk,
    `memory = "auto"` is equivalent to `memory = "persistent"` for for
    non-dynamically-branched targets that other targets dynamically
    branch over. For example: if your pipeline has
    `tar_target(name = y, command = x, pattern = map(x))`, then
    `tar_target(name = x, command = f(), memory = "auto")` will use
    persistent memory for `x` in order to avoid rereading all of `x` for
    every branch of `y`.

  - `"transient"`: the target gets unloaded after every new target
    completes. Either way, the target gets automatically loaded into
    memory whenever another target needs the value.

  - `"persistent"`: the target stays in memory until the end of the
    pipeline (unless `storage` is `"worker"`, in which case `targets`
    unloads the value from memory right after storing it in order to
    avoid sending copious data over a network).

  For cloud-based file targets (e.g. `format = "file"` with
  `repository = "aws"`), the `memory` option applies to the temporary
  local copy of the file: `"persistent"` means it remains until the end
  of the pipeline and is then deleted, and `"transient"` means it gets
  deleted as soon as possible. The former conserves bandwidth, and the
  latter conserves local storage.

- garbage_collection:

  Logical: `TRUE` to run [`base::gc()`](https://rdrr.io/r/base/gc.html)
  just before the target runs, in whatever R process it is about to run
  (which could be a parallel worker). `FALSE` to omit garbage
  collection. Numeric values get converted to `FALSE`. The
  `garbage_collection` option in
  [`tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html)
  is independent of the argument of the same name in
  [`tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html).

- deployment:

  Character of length 1. If `deployment` is `"main"`, then the target
  will run on the central controlling R process. Otherwise, if
  `deployment` is `"worker"` and you set up the pipeline with
  distributed/parallel computing, then the target runs on a parallel
  worker. For more on distributed/parallel computing in `targets`,
  please visit <https://books.ropensci.org/targets/crew.html>.

- resources:

  Object returned by
  [`tar_resources()`](https://docs.ropensci.org/targets/reference/tar_resources.html)
  with optional settings for high-performance computing functionality,
  alternative data storage formats, and other optional capabilities of
  `targets`. See
  [`tar_resources()`](https://docs.ropensci.org/targets/reference/tar_resources.html)
  for details.

- storage:

  Character string to control when the output of the target is saved to
  storage. Only relevant when using `targets` with parallel workers
  (<https://books.ropensci.org/targets/crew.html>). Must be one of the
  following values:

  - `"worker"` (default): the worker saves/uploads the value.

  - `"main"`: the target's return value is sent back to the host machine
    and saved/uploaded locally.

  - `"none"`: `targets` makes no attempt to save the result of the
    target to storage in the location where `targets` expects it to be.
    Saving to storage is the responsibility of the user. Use with
    caution.

- retrieval:

  Character string to control when the current target loads its
  dependencies into memory before running. (Here, a "dependency" is
  another target upstream that the current one depends on.) Only
  relevant when using `targets` with parallel workers
  (<https://books.ropensci.org/targets/crew.html>). Must be one of the
  following values:

  - `"auto"` (default): equivalent to `retrieval = "worker"` in almost
    all cases. But to avoid unnecessary reads from disk,
    `retrieval = "auto"` is equivalent to `retrieval = "main"` for
    dynamic branches that branch over non-dynamic targets. For example:
    if your pipeline has `tar_target(x, command = f())`, then
    `tar_target(y, command = x, pattern = map(x), retrieval = "auto")`
    will use `"main"` retrieval in order to avoid rereading all of `x`
    for every branch of `y`.

  - `"worker"`: the worker loads the target's dependencies.

  - `"main"`: the target's dependencies are loaded on the host machine
    and sent to the worker before the target runs.

  - `"none"`: `targets` makes no attempt to load its dependencies. With
    `retrieval = "none"`, loading dependencies is the responsibility of
    the user. Use with caution.

- cue:

  An optional object from
  [`tar_cue()`](https://docs.ropensci.org/targets/reference/tar_cue.html)
  to customize the rules that decide whether the target is up to date.

- object_simple_name, data_simple_name, fit_simple_name:

  target names to use for the simplified object, simplified data, fit of
  the simplified object with the simplified data, and fit with the
  original data re-inserted.

- description:

  Human-readable name for the model, announced by the estimation target
  when it starts to run. `NULL` (the default) announces nothing.

- target_settings:

  Named list of
  [`targets::tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html)
  settings to apply to every generated target, as built by
  `tar_nlmixr()` from its own arguments. Defaults to those arguments'
  own defaults, so calling `tar_nlmixr_raw()` directly behaves the same
  as before this argument existed.

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

The simplified data keep the columns that the estimation method reads.
For most methods that is the standard event columns plus the covariates
named in the model; `est = "vae"` additionally searches subject-constant
data columns during its automated covariate selection, so those
candidate columns are kept, too (see
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md)).
Changes to any kept column invalidate the cached fit; changes to dropped
columns do not.

`table` does two jobs: `table$keep` names data columns to carry through
to the simplified data, and a non-default `tableControl()` is handed to
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
so the residual/table step honors it. Left at its default, `table` is
omitted from the generated command entirely. That keeps the command
byte-identical to what earlier versions produced, so upgrading does not
re-run cached fits, and it preserves the merge
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
performs of the table settings carried on `ui$meta` (`addDosing`,
`subsetNonmem`, `cores`, `keep`, `drop`), which happens only when its
own `table` argument is missing. Supplying any other `tableControl()`
changes the command and re-runs the fit, as a changed `control` would.

## Functions

- `tar_nlmixr_raw()`: An internal function to generate the targets

## Arguments not forwarded to nlmixr2est

:nlmixr(): The generated estimation target calls
[`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
with `object`, `data`, `est`, `control`, and – when you supply one –
`table`. Its three remaining arguments are deliberately left out:

- `...` is inert. Each
  [`nlmixr2est::nlmixr()`](https://nlmixr2.github.io/nlmixr2est/reference/nlmixr2.html)
  method captures it with `match.call(expand.dots = TRUE)` and then
  dispatches to `nlmixr2Est0()` without it, so nothing passed through
  `...` reaches an estimator. There is nothing to forward.

- `save` is accepted by those methods but not acted on by any of them.
  Even if it were, it writes an RDS copy of the fit to the working
  directory; the `targets` store already persists every fit, so the copy
  would be a duplicate produced as an untracked side effect of a target.

- `envir` is the environment `nlmixr2est` passes to
  [`rxode2::.udfEnvSet()`](https://nlmixr2.github.io/rxode2/reference/dot-udfEnvSet.html)
  to resolve R user-defined functions, and uses to evaluate
  back-transformation expressions. `nlmixr2targets` leaves it at its
  default, which resolves to a `nlmixr2targets` internal frame – objects
  defined in your `_targets.R` are not visible from there. A model that
  calls an R user-defined function may therefore fail to resolve it;
  please report it if you hit this.

## Arguments not forwarded to targets

:tar_target(): `format`, `repository`, `library`, `memory`,
`garbage_collection`, `deployment`, `resources`, `storage`, `retrieval`
and `cue` are passed to every generated target, and default exactly as
[`targets::tar_target()`](https://docs.ropensci.org/targets/reference/tar_target.html)
defaults them, so a
[`targets::tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html)
earlier in `_targets.R` reaches these targets like any other. The rest
are left out on purpose:

- `command`, `deps` and `string` describe the work a target does, and
  that is what this function exists to write. `string` in particular is
  already used to keep a renamed model from re-running its fit.

- `pattern` and `iteration` configure dynamic branching. The generated
  targets are a fixed set of four per model, and
  [`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
  already provides the many-models case, so there is nothing to branch
  over.

- `tidy_eval` controls `!!` interpolation of a `command` written by the
  caller. These commands are assembled with
  [`substitute()`](https://rdrr.io/r/base/substitute.html) from
  arguments that are already captured unevaluated, so it has nothing to
  act on.

- `packages` is chosen per generated target: the simplification targets
  load `nlmixr2est` so that an un-namespaced `control` or `table`
  expression evaluates, and the estimation target loads only what it
  needs. Overriding it would break those choices silently. Use `library`
  to point at a different package library instead.

- `priority` was deprecated in `targets` 1.10.1.9013 (2025-04-08); its
  scheduler no longer honours user priorities, so forwarding it would
  only produce a deprecation warning per generated target.

- `error` and `description` are already taken by arguments of this
  function that mean something else. `error` here decides whether a
  failed fit becomes a sentinel rather than how `targets` treats a
  failed target, and `description` names the model in the message the
  estimation target prints. Set the `targets` versions with
  [`targets::tar_option_set()`](https://docs.ropensci.org/targets/reference/tar_option_set.html).

## Running the generated targets on a remote worker

The simplified model is written to
`file.path(tar_config_get("store"), "user/nlmixr2")` by the
`object_simple` target and read back from there by the estimation
target. That is a path, not a value passed between targets, so the two
must agree on it: sending only the estimation target to a worker whose
filesystem does not carry the same store leaves it unable to find the
model. Either give the whole set the same `resources` and `deployment`
so they run together, or put the store somewhere both reach.

## Side effects

When the user's model function body contains `cmt(0) <- value` inside a
`model({...})` block, `tar_nlmixr()` rewrites those lines to
`cmt(initial) <- value` directly in the function's binding in `env` so
that `targets`' static analysis (which walks every function in env via
[`codetools::findGlobals()`](https://rdrr.io/pkg/codetools/man/findGlobals.html))
accepts the model. The rewrite is reversed at evaluation time, so
fitting and downstream behaviour are unchanged. The user-visible
consequence is that printing `body(my_model)` at the REPL after a call
to `tar_nlmixr()` will show `cmt(initial)` rather than the
originally-written `cmt(0)`.

Manual `cmt(initial) <- value` written by the user is also accepted, but
it is a `nlmixr2targets`-only workaround: bare nlmixr2 does not
understand the `cmt(initial)` form, so a model function written that way
only fits when routed through `tar_nlmixr()` (or
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)).

## See also

[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
for fitting many models against one dataset.

## Examples

``` r
pheno <- function() {
  ini({
    lcl <- log(0.008); label("Typical value of clearance")
    lvc <- log(0.6); label("Typical value of volume of distribution")
    etalcl + etalvc ~ c(1,
                        0.01, 1)
    cpaddSd <- 0.1; label("residual variability")
  })
  model({
    cl <- exp(lcl + etalcl)
    vc <- exp(lvc + etalvc)
    kel <- cl / vc
    d / dt(central) <- -kel * central
    cp <- central / vc
    cp ~ add(cpaddSd)
  })
}

# Build the four targets that estimate `pheno`. `data` and `est` are
# captured as expressions, so this just returns the target list; the
# estimation step runs only when you call `targets::tar_make()` from a
# project whose targets store you have configured (for example, with
# `targets::tar_config_set(store = file.path(tempdir(), "_targets"))`
# or by running inside a project directory you own).
tar_nlmixr(
  name = pheno_model,
  object = pheno,
  data = nlmixr2data::pheno_sd,
  est = "saem"
)
#> $object_simple
#> <tar_stem> 
#>   name: pheno_model_object_simple 
#>   description:  
#>   command:
#>     nlmixr_object_simplify(object = pheno, directory = file.path(targets::tar_config_get("store"), 
#>         "user/nlmixr2")) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets
#>     nlmixr2est 
#>   library:
#>     NULL
#> $data_simple
#> <tar_stem> 
#>   name: pheno_model_data_simple 
#>   description:  
#>   command:
#>     nlmixr_data_simplify(object = pheno_model_object_simple, 
#>         data = nlmixr2data::pheno_sd, table = nlmixr2est::tableControl(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2"), 
#>         est = "saem", control = list()) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets
#>     nlmixr2est 
#>   library:
#>     NULL
#> $fit_simple
#> <tar_stem> 
#>   name: pheno_model_fit_simple 
#>   description:  
#>   command:
#>     nlmixr2_indirect(object = pheno_model_object_simple, 
#>         data = pheno_model_data_simple, est = "saem", control = list(), 
#>         directory = file.path(targets::tar_config_get("store"), "user/nlmixr2"), 
#>         error = "stop") 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2est 
#>   library:
#>     NULL
#> $fit
#> <tar_stem> 
#>   name: pheno_model 
#>   description:  
#>   command:
#>     nlmixr_object_complicate(fit = pheno_model_fit_simple, 
#>         object = pheno, data = nlmixr2data::pheno_sd) 
#>   format: rds 
#>   repository: local 
#>   iteration method: vector 
#>   error mode: stop 
#>   memory mode: auto 
#>   storage mode: worker 
#>   retrieval mode: auto 
#>   deployment mode: worker 
#>   priority: 0 
#>   resources:
#>     list() 
#>   cue:
#>     seed: TRUE
#>     file: TRUE
#>     iteration: TRUE
#>     repository: TRUE
#>     format: TRUE
#>     depend: TRUE
#>     command: TRUE
#>     mode: thorough 
#>   packages:
#>     nlmixr2targets 
#>   library:
#>     NULL
```
