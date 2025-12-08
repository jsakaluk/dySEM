# A Function That Writes, Saves, and Exports Syntax for Fitting Bifactor Dyadic (BiDy) models

This function takes the outputted object from scrapeVarCross() and
automatically writes, returns, and exports (.txt) lavaan() syntax for
specifying dyadic configural, loading, and intercept invariant BiDy CFA
(BiDy-C) or SEM (BiDy-S) Model. Currently only uses fixed-factor
scale-setting

## Usage

``` r
scriptBiDy(
  dvn,
  type = "CFA",
  lvxname,
  lvyname,
  constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_x_struct = c("variances", "means"),
  constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_y_struct = c("variances", "means"),
  constr_dy_xy_struct = c("actors"),
  model = lifecycle::deprecated(),
  equate = lifecycle::deprecated(),
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- type:

  input character to specify whether to script a BiDy-CFA ("CFA",
  default) or BiDy-SEM ("SEM") model

- lvxname:

  input character to (arbitrarily) name LV X in lavaan syntax

- lvyname:

  input character to (arbitrarily) name LV Y in lavaan syntax

- constr_dy_x_meas:

  input character vector detailing which measurement model parameters to
  constrain across dyad members for latent X. Default is c("loadings",
  "intercepts", "residuals"), but user can specify any combination of
  "loadings", "intercepts", and "residuals", or "none" to specify an
  otherwise unconstrained dyadic configural invariance model. Users may
  also specify more boutique patterns of bifactor loading constraints
  with "loadings_source" or "loadings_mutual".

- constr_dy_x_struct:

  input character vector detailing which structural model parameters to
  constrain across dyad members for latent X. Default is c("variances",
  "means"), but user can specify any combination of "variances" and
  "means", or "none".

- constr_dy_y_meas:

  input character vector detailing which measurement model parameters to
  constrain across dyad members for latent X. Default is c("loadings",
  "intercepts", "residuals"), but user can specify any combination of
  "loadings", "intercepts", and "residuals", or "none" to specify an
  otherwise unconstrained dyadic configural invariance model. Users may
  also specify more boutique patterns of bifactor loading constraints
  with "loadings_source" or "loadings_mutual".

- constr_dy_y_struct:

  input character vector detailing which structural model parameters to
  constrain across dyad members for latent X. Default is c("variances",
  "means"), but user can specify any combination of "variances" and
  "means", or "none".

- constr_dy_xy_struct:

  input character vector detailing which structural model parameters to
  constrain for modeling the predictive association(s) between partners'
  latent x and y. Default is c("actors"), but users can also specify
  "dyadic_zero" or "none".

- model:

  Deprecated input character used to specify which level of invariance
  is modeled. Users should rely upon constr_dy_x_meas/constr_dy_y_meas
  and constr_dy_x_struct/constr_dy_y_struct instead, for making
  constraints to the measurement and/or structural portions of the model
  for latent x and y.

- equate:

  Deprecated input character to specify which type of structural
  parameters are constrained to equivalency between partners. Users
  should rely upon constr_dy_xy_struct for making constraints to the
  structural portion of the model for associative relationship between
  latent x and y.

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

Other bi-construct script-writing functions:
[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md),
[`scriptCFM()`](https://jsakaluk.github.io/dySEM/reference/scriptCFM.md),
[`scriptMIM()`](https://jsakaluk.github.io/dySEM/reference/scriptMIM.md)

## Examples

``` r
dvn <- scrapeVarCross(DRES, x_order = "sip", x_stem = "sexsat",
x_delim2=".", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sexsat
#> ℹ sexsat: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sexsat.bidyc.script <- scriptBiDy(dvn, lvxname = "SexSat", type = "CFA",
writeTo = tempdir(),
fileName = "BiDy_C")

dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2",
y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 2 latent variables: sat.g and com
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ com: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 20

comsat.bidys.config.script <- scriptBiDy(dvn, lvxname = "Sat",
lvyname = "Com", type = "SEM",
writeTo = tempdir(),
fileName = "BiDy_S")
```
