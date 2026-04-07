# Observed Actor-Partner Interdependence Model (lavaan syntax)

Writes lavaan syntax for an APIM with one observed (or composite) X and
Y per partner. Structural constraints mirror
[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)
(dyadic variances/means, actor/partner paths, optional orthogonal dyadic
covariances).

## Usage

``` r
scriptObsAPIM(
  X1 = NULL,
  Y1 = NULL,
  X2 = NULL,
  Y2 = NULL,
  constr_dy_x_struct = c("variances", "means"),
  constr_dy_y_struct = c("variances", "means"),
  constr_dy_xy_struct = c("actors", "partners"),
  includeMeanStruct = FALSE,
  equate = lifecycle::deprecated(),
  k = lifecycle::deprecated(),
  est_k = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- X1, X2, Y1, Y2:

  Character names of variables for partners 1 and 2.

- constr_dy_x_struct:

  Character vector: any of `"variances"`, `"means"`, `"orthogonal"`,
  and/or `"none"`. Defaults match
  [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md).
  `"means"` is applied only when `includeMeanStruct` is `TRUE` (same
  behavior as
  [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)).
  `"orthogonal"` fixes the dyadic covariance between `X1` and `X2` to
  zero.

- constr_dy_y_struct:

  Same allowed values as `constr_dy_x_struct`, for the Y side.
  `"variances"` equates residual variances of `Y1` and `Y2`;
  `"orthogonal"` fixes the residual covariance between `Y1` and `Y2` to
  zero.

- constr_dy_xy_struct:

  Character vector: `"actors"`, `"partners"`, `"all"`, `"actors_zero"`,
  `"partners_zero"`, and/or `"none"` (same semantics as
  [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)).

- includeMeanStruct:

  Logical; if `TRUE`, mean structure is included and `"means"` in
  `constr_dy_x_struct` / `constr_dy_y_struct` equates exogenous means
  and regression intercepts, respectively.

- equate:

  **\[deprecated\]** Use `constr_dy_xy_struct` instead. If supplied,
  maps `none` → `"none"`, `actor` → `c("actors")`, `partner` →
  `c("partners")`, `all` → `c("actors", "partners")`.

- k:

  **\[deprecated\]** Use `est_k` instead.

- est_k:

  Logical; if `TRUE`, Kenny & Ledermann's (2010) k ratio(s) are defined
  in the syntax (see
  [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)).

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

Character string of lavaan model syntax.

## See also

[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)

Other bi-construct script-writing functions:
[`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md),
[`scriptBiDy()`](https://jsakaluk.github.io/dySEM/reference/scriptBiDy.md),
[`scriptCFM()`](https://jsakaluk.github.io/dySEM/reference/scriptCFM.md),
[`scriptMIM()`](https://jsakaluk.github.io/dySEM/reference/scriptMIM.md),
[`scriptTwoCross()`](https://jsakaluk.github.io/dySEM/reference/scriptTwoCross.md)

## Examples

``` r
scriptObsAPIM(
  X1 = "SexSatA", Y1 = "RelSatA",
  X2 = "SexSatB", Y2 = "RelSatB",
  constr_dy_xy_struct = "none",
  constr_dy_x_struct = "none",
  constr_dy_y_struct = "none",
  writeTo = tempdir(),
  fileName = "obsAPIM_script"
)
#> [1] "# Actor and partner effects\nRelSatA ~ a1*SexSatA + p1*SexSatB\nRelSatB ~ a2*SexSatB + p2*SexSatA\n\n# Dyadic X covariance\nSexSatA ~~ SexSatB\n\n# Residual covariance between Y\nRelSatA ~~ RelSatB"
```
