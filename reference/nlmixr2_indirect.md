# Estimate an nlmixr2 model loading the model from a targets indirect hash storage

This is not intended for direct use by users

## Usage

``` r
nlmixr2_indirect(object, data, est, control)
```

## Arguments

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
