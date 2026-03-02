# A Function That Writes, Saves, and Exports Syntax for Fitting Boutique Two-Construct Dyadic SEM Models

This function takes the outputted object from
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
with both X and Y indicators and automatically writes, returns, and
exports (`.txt`) `lavaan` syntax for specifying a two-construct model
where the measurement structure of X and Y can differ (e.g., X as
correlated factors, Y as unidimensional).

## Usage

``` r
scriptTwoCross(
  dvn,
  x_scaleset = "FF",
  y_scaleset = "FF",
  lvxname = "X",
  lvyname = "Y",
  x_model = "cor",
  y_model = "cor",
  constr_dy_x_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_x_struct = c("variances", "means"),
  constr_dy_y_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_y_struct = c("variances", "means"),
  constr_dy_xy_struct = "free",
  includeMeanStruct = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  Input dvn list from
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  containing information for both X and Y (i.e., must include both
  x_stem and y_stem arguments when calling scrapeVarCross).

- x_scaleset:

  Character string specifying how to set the scale of latent X. Options
  are `"FF"` (fixed-factor) or `"MV"` (marker variable). Default is
  `"FF"`.

- y_scaleset:

  Character string specifying how to set the scale of latent Y. Options
  are `"FF"` (fixed-factor) or `"MV"` (marker variable). Default is
  `"FF"`.

- lvxname:

  Input character to (arbitrarily) name latent variable X in `lavaan`
  syntax.

- lvyname:

  Input character to (arbitrarily) name latent variable Y in `lavaan`
  syntax.

- x_model:

  Character string specifying the measurement model for X. Options are
  `"uni"` (unidimensional), `"cor"` (correlated factors), `"hier"`
  (hierarchical), or `"bifactor"`.

- y_model:

  Character string specifying the measurement model for Y. Same options
  as `x_model`.

- constr_dy_x_meas:

  Input character vector detailing which measurement model parameters to
  constrain across dyad members for latent X.

- constr_dy_x_struct:

  Input character vector detailing which structural model parameters to
  constrain across dyad members for latent X.

- constr_dy_y_meas:

  Input character vector detailing which measurement model parameters to
  constrain across dyad members for latent Y.

- constr_dy_y_struct:

  Input character vector detailing which structural model parameters to
  constrain across dyad members for latent Y.

- constr_dy_xy_struct:

  Character string for the X-to-Y regression paths. Options are `"free"`
  (default; all paths freely estimated) or `"zero"` (all paths fixed to
  zero).

- includeMeanStruct:

  Logical for whether to include the mean structure (intercepts and
  latent means) in the model. Defaults to `FALSE`.

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

Character object of `lavaan` script that can be passed immediately to
`lavaan` functions.

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on;
[`scriptUni`](https://jsakaluk.github.io/dySEM/reference/scriptUni.md),
[`scriptCor`](https://jsakaluk.github.io/dySEM/reference/scriptCor.md),
[`scriptHier`](https://jsakaluk.github.io/dySEM/reference/scriptHier.md),
[`scriptBifac`](https://jsakaluk.github.io/dySEM/reference/scriptBifac.md)
for the underlying scripters;
[`scriptAPIM`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)
for a related two-construct function.

Other bi-construct script-writing functions:
[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md),
[`scriptBiDy()`](https://jsakaluk.github.io/dySEM/reference/scriptBiDy.md),
[`scriptCFM()`](https://jsakaluk.github.io/dySEM/reference/scriptCFM.md),
[`scriptMIM()`](https://jsakaluk.github.io/dySEM/reference/scriptMIM.md)

## Examples

``` r
dvn <- scrapeVarCross(
  dat = commitmentQ,
  x_order = "spi", x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_",
  distinguish_1 = "1", distinguish_2 = "2",
  y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 2 latent variables: sat.g and com
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ com: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 20

# Correlated X, unidimensional Y
scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
  x_model = "cor", y_model = "uni")
#> [1] "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nComDy=~NA*com.1_1+lyg1*com.1_1+lyg2*com.1_2+lyg3*com.1_3+lyg4*com.1_4+lyg5*com.1_5+lgy1*com.2_1+lgy2*com.2_2+lgy3*com.2_3+lgy4*com.2_4+lgy5*com.2_5\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\nComDy ~~ 1*ComDy\n\n#Latent Regressions (X -> Y)\nComDy ~ b_ComDy_Sat1*Sat1 + b_ComDy_Sat2*Sat2"

# Both correlated
scriptTwoCross(dvn, lvxname = "Sat", lvyname = "Com",
  x_model = "cor", y_model = "cor",
  writeTo = tempdir(), fileName = "twoCross_cor_cor")
#> [1] "#Measurement Model\n\n#Loadings\nSat1=~NA*sat.g.1_1+lx1*sat.g.1_1+lx2*sat.g.1_2+lx3*sat.g.1_3+lx4*sat.g.1_4+lx5*sat.g.1_5\nSat2=~NA*sat.g.2_1+lx1*sat.g.2_1+lx2*sat.g.2_2+lx3*sat.g.2_3+lx4*sat.g.2_4+lx5*sat.g.2_5\n\nCom1=~NA*com.1_1+ly1*com.1_1+ly2*com.1_2+ly3*com.1_3+ly4*com.1_4+ly5*com.1_5\nCom2=~NA*com.2_1+ly1*com.2_1+ly2*com.2_2+ly3*com.2_3+ly4*com.2_4+ly5*com.2_5\n\n#Residual Variances\nsat.g.1_1 ~~ thx1*sat.g.1_1\nsat.g.1_2 ~~ thx2*sat.g.1_2\nsat.g.1_3 ~~ thx3*sat.g.1_3\nsat.g.1_4 ~~ thx4*sat.g.1_4\nsat.g.1_5 ~~ thx5*sat.g.1_5\nsat.g.2_1 ~~ thx1*sat.g.2_1\nsat.g.2_2 ~~ thx2*sat.g.2_2\nsat.g.2_3 ~~ thx3*sat.g.2_3\nsat.g.2_4 ~~ thx4*sat.g.2_4\nsat.g.2_5 ~~ thx5*sat.g.2_5\n\ncom.1_1 ~~ thy1*com.1_1\ncom.1_2 ~~ thy2*com.1_2\ncom.1_3 ~~ thy3*com.1_3\ncom.1_4 ~~ thy4*com.1_4\ncom.1_5 ~~ thy5*com.1_5\ncom.2_1 ~~ thy1*com.2_1\ncom.2_2 ~~ thy2*com.2_2\ncom.2_3 ~~ thy3*com.2_3\ncom.2_4 ~~ thy4*com.2_4\ncom.2_5 ~~ thy5*com.2_5\n\n#Residual Covariances\nsat.g.1_1 ~~ sat.g.2_1\nsat.g.1_2 ~~ sat.g.2_2\nsat.g.1_3 ~~ sat.g.2_3\nsat.g.1_4 ~~ sat.g.2_4\nsat.g.1_5 ~~ sat.g.2_5\n\ncom.1_1 ~~ com.2_1\ncom.1_2 ~~ com.2_2\ncom.1_3 ~~ com.2_3\ncom.1_4 ~~ com.2_4\ncom.1_5 ~~ com.2_5\n\n#Structural Model\n\n#Latent (Co)Variances\nSat1 ~~ 1*Sat1 + psix*Sat1\nSat2 ~~ psix*Sat2\nSat1 ~~ Sat2\n\nCom1 ~~ 1*Com1 + psiy*Com1\nCom2 ~~ psiy*Com2\nCom1 ~~ Com2\n\n#Latent Regressions (X -> Y)\nCom1 ~ b_Com1_Sat1*Sat1 + b_Com1_Sat2*Sat2\nCom2 ~ b_Com2_Sat1*Sat1 + b_Com2_Sat2*Sat2"
```
