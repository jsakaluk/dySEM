# Population lavaan syntax for a latent indistinguishable APIM

Writes a fully specified population model with fixed numeric parameters
for use with
[`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).
Measurement blocks follow the same conventions as
[`scriptPopCor()`](https://jsakaluk.github.io/dySEM/reference/scriptPopCor.md)
for each construct; structural paths use a single actor and partner
coefficient on both partners' equations.

## Usage

``` r
scriptPopAPIM(dvn, lvxname, lvyname, pop_values)
```

## Arguments

- dvn:

  List from
  [`build_dvn()`](https://jsakaluk.github.io/dySEM/reference/build_dvn.md)
  or
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  with X and Y indicators (`p1yvarnames`, `p2yvarnames`, `yindper`
  required).

- lvxname, lvyname:

  Base names for latent X and Y (partner suffixes from `dvn$dist1` /
  `dvn$dist2` are appended, as in
  [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)).

- pop_values:

  Named list; see **Population values** below.

## Value

Single string of lavaan model syntax.

## Population values

**X measurement** (lengths `nx = dvn$xindper`): `x_loadings_p1`,
`x_loadings_p2` (optional recycle), `x_residuals_p1`, `x_residuals_p2`,
`x_coresids`, optional `x_intercepts_p1` / `x_intercepts_p2`.

**Y measurement** (`ny = dvn$yindper`): `y_loadings_p1`,
`y_loadings_p2`, `y_residuals_p1`, `y_residuals_p2`, `y_coresids`,
optional `y_intercepts_*`.

**Latent X:** `x_lv_var` (scalar or length 2, default `c(1,1)`),
`x_lv_cov` (default 0), `x_lv_means` (optional, default `c(0,0)`).

**Structural:** `actor`, `partner` (numeric), `y_lv_var` (scalar or
length 2, default `c(0.5, 0.5)`), `y_lv_cov` (default 0; covariance
between latent Y residuals).

Vectors of length 1 are recycled to `nx` or `ny` for measurement slots.

## See also

Other population-scripting functions:
[`scriptPopCor()`](https://jsakaluk.github.io/dySEM/reference/scriptPopCor.md),
[`scriptPopUni()`](https://jsakaluk.github.io/dySEM/reference/scriptPopUni.md)
