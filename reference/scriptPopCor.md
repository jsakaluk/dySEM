# Write Population lavaan Syntax for Correlated Dyadic CFA

Produces lavaan syntax with fixed numeric values for data generation in
Monte Carlo simulations. Used with
[`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).

## Usage

``` r
scriptPopCor(dvn, lvname = "X", pop_values, lvar = "X")
```

## Arguments

- dvn:

  Input dvn list from
  [`build_dvn()`](https://jsakaluk.github.io/dySEM/reference/build_dvn.md)
  or
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)

- lvname:

  Character name for the latent variable

- pop_values:

  Named list of population parameter values:

  - `loadings_p1`: numeric vector for partner 1 (length = n_items)

  - `loadings_p2`: numeric vector for partner 2 (length = n_items). If
    NULL, uses loadings_p1.

  - `residuals_p1`: numeric vector (length = n_items)

  - `residuals_p2`: numeric vector (length = n_items). If NULL, uses
    residuals_p1.

  - `coresids`: numeric vector (length = n_items)

  - `intercepts_p1`: optional (default 0)

  - `intercepts_p2`: optional (default 0)

  - `lv_var`: latent variances; scalar (both=1) or length-2 vector

  - `lv_cov`: latent covariance

  - `lv_means`: latent means; scalar (both=0) or length-2 vector

- lvar:

  Character `"X"` or `"Y"`. Default `"X"`.

## Value

Character string of lavaan syntax

## See also

Other population-scripting functions:
[`scriptPopUni()`](https://jsakaluk.github.io/dySEM/reference/scriptPopUni.md)

## Examples

``` r
dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
  distinguish_1 = "A", distinguish_2 = "B")
scriptPopCor(dvn, "X", list(
  loadings_p1 = c(.75, .80, .55),
  loadings_p2 = c(.70, .85, .50),
  residuals_p1 = c(.44, .36, .70),
  residuals_p2 = c(.51, .28, .75),
  coresids = c(.10, .12, .08),
  lv_cov = 0.3
))
#> [1] "# Correlated Dyadic CFA - Population Model\n\n# Loadings\nXA =~ 0.75*x1_A + 0.8*x2_A + 0.55*x3_A\nXB =~ 0.7*x1_B + 0.85*x2_B + 0.5*x3_B\n\n# Intercepts\nx1_A ~ 0*1\nx2_A ~ 0*1\nx3_A ~ 0*1\nx1_B ~ 0*1\nx2_B ~ 0*1\nx3_B ~ 0*1\n\n# Residual Variances\nx1_A ~~ 0.44*x1_A\nx2_A ~~ 0.36*x2_A\nx3_A ~~ 0.7*x3_A\nx1_B ~~ 0.51*x1_B\nx2_B ~~ 0.28*x2_B\nx3_B ~~ 0.75*x3_B\n\n# Residual Covariances\nx1_A ~~ 0.1*x1_B\nx2_A ~~ 0.12*x2_B\nx3_A ~~ 0.08*x3_B\n\n# Latent Variances\nXA ~~ 1*XA\nXB ~~ 1*XB\n\n# Latent Covariance\nXA ~~ 0.3*XB\n\n# Latent Means\nXA ~ 0*1\nXB ~ 0*1"
```
