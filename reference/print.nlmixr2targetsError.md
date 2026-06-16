# Print a failed `nlmixr2targets` estimation result

When
[`tar_nlmixr()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr.md)
or
[`tar_nlmixr_multimodel()`](https://nlmixr2.github.io/nlmixr2targets/reference/tar_nlmixr_multimodel.md)
is called with `error = "continue"` and the estimation step throws, the
target stores a lightweight failure sentinel of class
`nlmixr2targetsError` instead of an `nlmixr2` fit. This method prints
the captured error message. Detect such a sentinel with
`inherits(x, "nlmixr2targetsError")` (or the broader
`inherits(x, "try-error")`).

## Usage

``` r
# S3 method for class 'nlmixr2targetsError'
print(x, ...)
```

## Arguments

- x:

  A `nlmixr2targetsError` object.

- ...:

  Ignored; present for compatibility with
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

`x`, invisibly.
