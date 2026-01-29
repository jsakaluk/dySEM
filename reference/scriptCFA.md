# A Function That Writes, Saves, and Exports Syntax for Fitting Latent Dyadic Confirmatory Factor Analysis (CFA) Models with Multiple Factors

This function takes the outputted object from scrapeVarCross() when the
var_list argument has been used, and automatically writes, returns, and
exports (.txt) lavaan() syntax for specifying dyadic configural,
loading, and intercept invariant measurement models for either a group
of latent variables (e.g., different sub-scales from a self-report
measures).

`scriptMultiCor()` is a thin wrapper around `scriptCFA()` provided as a
synonym for users who think of these models as dyadic multi-factor
correlated-factor models. It takes the same arguments as `scriptCFA()`
and returns the identical lavaan syntax.

## Usage

``` r
scriptCFA(
  dvn,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"),
  writeTo = NULL,
  fileName = NULL
)

scriptMultiCor(
  dvn,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"),
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross(); scrapeVarCross *must* have been
  run with the var_list argument

- scaleset:

  Input character to specify how to set the scale of the latent
  variable(s). Default is `"FF"` (fixed-factor; see Details for
  rationale), but user can specify `"MV"` (Marker Variable).

- constr_dy_meas:

  input character vector detailing which measurement model parameters to
  constrain across dyad members. Default is c("loadings", "intercepts",
  "residuals")(in combination with defaults for constr_dy_struct, an
  indistinguishable dyadic CFA), but user can specify any combination of
  "loadings", "intercepts", and "residuals", or "none" to specify an
  otherwise unconstrained dyadic configural invariance model

- constr_dy_struct:

  input character vector detailing which structural model parameters to
  constrain across dyad members. Default is c("variances", "means")(in
  combination with defaults for constr_dy_meas, an indistinguishable
  dyadic CFA), but user can specify any combination of "variances" and
  "means", or "none".

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
lavaan functions

character object of lavaan script that can be passed immediately to
lavaan functions

## Details

By default, many dySEM:: functions (including scriptCFA() default to a
fixed-factor method of scale-setting, whereby the latent variance of a
given factor is constrained to 1 for both partners in the configurally
invariant \#model, and then one of these variances is freely estimated
in subsequent \#models of the invariance testing sequence. We have
selected this default for two reasons: (1) the selection of a
marker-variable is usually arbitrary, yet can have a large influence on
the estimation and testing of of structural parameters (see
https://stats.stackexchange.com/questions/402133/in-cfa-does-it-matter-which-factor-loading-is-set-to-1/402732#402732);
and (2) the selection of a non-invariant marker-variable can have
disastrous down-stream consequences for the identification of
non-invariant measurement parameters, following a the rejection of an
omnibus \#invariance constraint set (see Lee, Preacher, & Little, 2011).

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on

Other multi-construct script-writing functions:
[`scriptDyEFA()`](https://jsakaluk.github.io/dySEM/reference/scriptDyEFA.md)

## Examples

``` r
# When different factor use distinct stems:
imsList <- list(
  lvnames = c("Sat", "Q_Alt", "Invest", "Comm"),
  stem = c("sat.g", "qalt.g", "invest.g", "com"),
  delim1 = c("", "", "", ""),
  delim2 = c("_", "_", "_", "_")
)

dvnIMS <- scrapeVarCross(imsM,
  var_list = imsList,
  var_list_order = "sip",
  distinguish_1 = "f",
  distinguish_2 = "m"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 4 latent variables
#> ℹ Sat: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Q_Alt: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Invest: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Comm: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Total indicators: 40

script.ims.config <- scriptCFA(dvnIMS,
  scaleset = "FF",
  constr_dy_meas = "none",
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_config"
)

script.ims.load <- scriptCFA(dvnIMS,
  scaleset = "FF",
  constr_dy_meas = c("loadings"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_load"
)

script.ims.int <- scriptCFA(dvnIMS,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_int"
)

script.ims.res <- scriptCFA(dvnIMS,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "ims_res"
)

script.ims.indist <- scriptCFA(dvnIMS,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"), writeTo = tempdir(), fileName = "ims_indist"
)

# When different factor use the same stem and muse be detected through numeric position

prqcList <- list(
  lvnames = c("Sat", "Comm", "Intim", "Trust", "Pass", "Love"),
  stem = c("prqc", "prqc", "prqc", "prqc", "prqc", "prqc"),
  delim1 = c(".", ".", ".", ".", ".", "."),
  delim2 = c("_", "_", "_", "_", "_", "_"),
  min_num = c(1, 4, 7, 10, 13, 16),
  max_num = c(3, 6, 9, 12, 15, 18)
)

dvnPRQC <- scrapeVarCross(prqcQ,
  var_list = prqcList,
  var_list_order = "spi",
  distinguish_1 = "1",
  distinguish_2 = "2"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 6 latent variables
#> ℹ Sat: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Comm: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Intim: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Trust: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Pass: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Love: 3 indicators for P1 (1), 3 indicators for P2 (2)
#> ℹ Total indicators: 36

script.prqc.config <- scriptCFA(dvnPRQC,
  scaleset = "FF",
  constr_dy_meas = "none",
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_config"
)

script.prqc.load <- scriptCFA(dvnPRQC,
  scaleset = "FF",
  constr_dy_meas = c("loadings"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_load"
)

script.prqc.int <- scriptCFA(dvnPRQC,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_int"
)

script.prqc.res <- scriptCFA(dvnPRQC,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = "none", writeTo = tempdir(), fileName = "prqc_res"
)

script.prqc.indist <- scriptCFA(dvnPRQC,
  scaleset = "FF",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"), writeTo = tempdir(), fileName = "prqc_indist"
)
```
