# A Function That Writes, Saves, and Exports Syntax for Fitting Correlated Dyadic Factor Models

This function takes the outputted object from
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
and automatically writes, returns, and exports (`.txt`) `lavaan` syntax
for specifying dyadic configural, loading, intercept, and residual
invariant two-factor models.

## Usage

``` r
scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "X",
  lvar = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"),
  writeTo = NULL,
  fileName = NULL,
  outputType = "lavaan script"
)
```

## Arguments

- dvn:

  Input dvn list from
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)

- scaleset:

  Input character to specify how to set the scale of the latent
  variable(s). Default is `"FF"` (fixed-factor; see Details for
  rationale), but user can specify `"MV"` (Marker Variable).

- lvname:

  Input character to (arbitrarily) name the latent variable in `lavaan`
  syntax

- lvar:

  Input character to specify whether the latent variable represents "X"
  or "Y" in the model. Default is `"X"`. This argument controls
  parameter labeling (e.g., `lx` vs `ly` for loadings, `tx` vs `ty` for
  intercepts).

- constr_dy_meas:

  Input character vector detailing which measurement model parameters to
  constrain across dyad members.

- constr_dy_struct:

  Input character vector detailing which structural model parameters to
  constrain across dyad members. Default is `c("variances", "means")`(in
  combination with defaults for `constr_dy_meas`, an indistinguishable
  correlated dyadic factors model), but user can specify any combination
  of `"variances"` and `"means"`, or `"none"`.

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

- outputType:

  Character string specifying the type of output to return. Options are
  `"lavaan script"` (default) to return a character object of `lavaan`
  syntax that can be passed immediately to `lavaan` functions, or
  `"syntax components"` to return a structured list of model components.

## Value

Character object of `lavaan` script that can be passed immediately to
`lavaan` functions (when `outputType = "lavaan script"`), or a
structured list of model components (when
`outputType = "syntax components"`).

## Details

- By default, many `dySEM::` functions (including `scriptCor()`) default
  to a fixed-factor method of scale-setting, whereby the latent variance
  of a given factor is constrained to 1 for both partners in the
  configurally invariant model, and then one of these variances is
  freely estimated in subsequent models of the invariance testing
  sequence. We have selected this default for two reasons: (1) the
  selection of a marker-variable is usually arbitrary, yet can have a
  large influence on the estimation and testing of of structural
  parameters (see
  https://stats.stackexchange.com/questions/402133/in-cfa-does-it-matter-which-factor-loading-is-set-to-1/402732#402732);
  and (2) the selection of a non-invariant marker-variable can have
  disastrous down-stream consequences for the identification of
  non-invariant measurement parameters, following a the rejection of an
  omnibus invariance constraint set (see Lee, Preacher, & Little, 2011).

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on.

Other uni-construct script-writing functions:
[`scriptBifac()`](https://jsakaluk.github.io/dySEM/reference/scriptBifac.md),
[`scriptHier()`](https://jsakaluk.github.io/dySEM/reference/scriptHier.md),
[`scriptUni()`](https://jsakaluk.github.io/dySEM/reference/scriptUni.md)

## Examples

``` r
dvn <- scrapeVarCross(
  dat = commitmentQ,
  x_order = "spi",
  x_stem = "sat.g",
  x_delim1 = ".",
  x_delim2 = "_",
  distinguish_1 = "1",
  distinguish_2 = "2"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.indist.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat"
)

sat.lvars.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat",
  constr_dy_meas = "loadings",
  constr_dy_struct = "variances"
)

sat.resids.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = "none",
  writeTo = tempdir(),
  fileName = "dCor_residual"
)

sat.ints.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat",
  constr_dy_meas = c("loadings", "intercepts"),
  constr_dy_struct = "none",
  writeTo = tempdir(),
  fileName = "dCor_intercept"
)

sat.loads.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat",
  constr_dy_meas = c("loadings"),
  constr_dy_struct = "none",
  writeTo = tempdir(),
  fileName = "dCor_loading"
)

sat.config.script <- scriptCor(
  dvn,
  scaleset = "FF",
  lvname = "Sat",
  constr_dy_meas = "none",
  constr_dy_struct = "none",
  writeTo = tempdir(),
  fileName = "dCor_configural"
)
```
