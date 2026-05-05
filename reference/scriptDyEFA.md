# A Function That Writes, Saves, and Exports Syntax for Fitting Dyadic Exploratory Factor Analysis (DEFA) Models

This function takes the outputted object from scrapeVarCross() and
automatically writes, returns, and exports (.txt) lavaan() syntax for
specifying a dyadic EFA model of a given number of exploratory factors.

## Usage

``` r
scriptDyEFA(
  dvn,
  nFactor = 1,
  constr_dy_meas = "none",
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- nFactor:

  numeric argument for number of exploratory factors to extract.
  Defaults to 1. Note that higher values may cause estimation problems
  as solution becomes over-factored and/or in the presence of
  insufficient data.

- constr_dy_meas:

  input character vector detailing which measurement model parameters to
  constrain across dyad members. Default is "none" but user can specify
  "loadings" and/or "residuals", to fit an exploratory model with
  loadings and/or residuals constrained across partners

- writeTo:

  A character string specifying a directory path to where a .txt file of
  the resulting lavaan script should be written. If set to “.”, the .txt
  file will be written to the current working directory. The default is
  NULL, and examples use a temporary directory created by tempdir().

- fileName:

  A character string specifying a desired base name for the .txt output
  file. The default is NULL. The specified name will be automatically
  appended with the .txt file extension. If a file with the same name
  already exists in the user's chosen directory, it will be overwritten.

## Value

character object of lavaan script that can be passed immediately to
lavaan functions

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on

Other multi-construct script-writing functions:
[`scriptCFA()`](https://jsakaluk.github.io/dySEM/reference/scriptCFA.md)

## Examples

``` r
dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.defa1.script <- scriptDyEFA(dvn, nFactor = 1,
writeTo = tempdir(), fileName = "DEFA_1fac")
```
