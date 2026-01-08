# A Function That Writes, Saves, and Exports Syntax for Fitting Latent Common Fate Models (CFMs)

This function takes the outputted object from scrapeVarCross() and
automatically writes, returns, and exports (.txt) lavaan() syntax for
specifying Common Fate Models (CFMs). Users can also invoke configural,
loading, and/or intercept invariant measurement models, and particular
types of structural comparisons.

## Usage

``` r
scriptCFM(
  dvn,
  scaleset = "FF",
  lvxname,
  lvyname,
  constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_x_struct = c("variances", "means"),
  constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_y_struct = c("variances", "means"),
  constr_dy_xy_struct = "none",
  model = lifecycle::deprecated(),
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- scaleset:

  Input character to specify how to set the scale of the latent
  variable(s). Default is `"FF"` (fixed-factor; see Details for
  rationale), but user can specify `"MV"` (Marker Variable).

- lvxname:

  input character to (arbitrarily) name LV X in lavaan syntax

- lvyname:

  input character to (arbitrarily) name LV Y in lavaan syntax

- constr_dy_x_meas:

  input character vector detailing which measurement model parameters to
  constrain across dyad members for latent X. Default is c("loadings",
  "intercepts", "residuals"), but user can specify any combination of
  "loadings", "intercepts", and "residuals", or "none" to specify an
  otherwise unconstrained dyadic configural invariance model

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
  otherwise unconstrained dyadic configural invariance model

- constr_dy_y_struct:

  input character vector detailing which structural model parameters to
  constrain across dyad members for latent X. Default is c("variances",
  "means"), but user can specify any combination of "variances" and
  "means", or "none".

- constr_dy_xy_struct:

  input character vector detailing which structural model parameters to
  constrain for modeling the predictive association(s) between partners'
  latent x and y. Defaults to "none". Options include "p1_zero" or
  "p2_zero" (to constrain within-person latent residual covariances
  between X and Y to zero), or "covar_zero" (to constrain both
  within-person latent residual correlations to zero), and/or
  "dyadic_zero" (to constrain the dyadic effect to zero).

- model:

  Deprecated input character used to specify which level of invariance
  is modeled. Users should rely upon constr_dy_x_meas/constr_dy_y_meas
  and constr_dy_x_struct/constr_dy_y_struct instead, for making
  constraints to the measurement and/or structural portions of the model
  for latent x and y.

- writeTo:

  A character string specifying a directory path to where the output
  file(s) should be saved. If set to `"."`, the file(s) will be written
  to the current working directory. The default is `NULL`, and examples
  use a temporary directory created by
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). When dealing with
  tabular output, `writeTo` is only relevant if `gtTab = TRUE`.

- fileName:

  A character string specifying a desired base name for the output file.
  The default is `NULL`. The specified name will be automatically
  appended with the appropriate file extension (e.g., `.txt` for
  `lavaan` scripts, `.rtf` for tabular output when `gtTab = TRUE`, or
  other extensions as appropriate for the output type). If a file with
  the same name already exists in the user's chosen directory, it will
  be overwritten.

## Value

character object of lavaan script that can be passed immediately to
lavaan functions. Users will receive message if structural comparisons
are specified when the recommended level of invariance is not also
specified. If user supplies dvn with containing X or Y variables, they
are alerted to respecify the dvn object.

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on

Other bi-construct script-writing functions:
[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md),
[`scriptBiDy()`](https://jsakaluk.github.io/dySEM/reference/scriptBiDy.md),
[`scriptMIM()`](https://jsakaluk.github.io/dySEM/reference/scriptMIM.md)

## Examples

``` r
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
cfm.script.indist <-  scriptCFM(dvn, lvxname = "Sat", lvyname = "Com",
writeTo = tempdir(),
fileName = "CFM_indist")
#> Warning: scriptCFM has some specification problems under certain patterns of constraints and is likely to see a major update in the near future; please consider delaying its use (this message will be removed when the update is complete)
```
