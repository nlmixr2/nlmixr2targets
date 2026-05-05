# Simplify an nlmixr object

This function is typically not needed by end users.

## Usage

``` r
nlmixr_object_simplify(object)
```

## Arguments

- object:

  Fitted object or function specifying the model.

## Value

The md5 hash used to load the simplified `nlmixrui` object back from the
`nlmixr2targets` indirect cache.

## Details

The object simplification removes comments (so please use `label()`
instead of comments to label parameters) and then converts the `object`
to a "nlmixrui" object.

Since setting initial conditions with `cmt(0)` does not work with
`targets`, the function definition of the object must set it with
`cmt(initial)`. `cmt(initial)` will be converted to `cmt(0)` before
passing to nlmixr2.

The simplified model's `model.name` is always set to `"object"`. This
keeps the simplified output stable so that the md5 hash used by the
`targets` indirect cache is independent of the symbol the caller bound
the model function to.

## See also

Other Simplifiers:
[`nlmixr_data_simplify()`](https://nlmixr2.github.io/nlmixr2targets/reference/nlmixr_data_simplify.md)
