# Replace the fit data with the original data, then return the modified fit

This function is intended for use within `nlmixr2targets` target
creation, and it's not typically invoked by users.

## Usage

``` r
assign_origData(fit, data)
```

## Arguments

- fit:

  an estimated `nlmixr2` object

- data:

  the data from the original fit

## Value

The fit with the data added back in as `fit$env$origData`
