# Write Population lavaan Syntax for Unidimensional Dyadic CFA

Produces lavaan syntax with fixed numeric values for data generation in
Monte Carlo simulations. Used with
[`lavaan::simulateData()`](https://rdrr.io/pkg/lavaan/man/simulateData.html).

## Usage

``` r
scriptPopUni(dvn, lvname = "X", pop_values, lvar = "X")
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

  - `loadings`: numeric vector of loadings (length = n_items)

  - `residuals`: numeric vector of residual variances (length = n_items)

  - `coresids`: numeric vector of residual covariances (length =
    n_items)

  - `intercepts`: optional; numeric vector (default 0 for all)

  - `lv_var`: latent variance (default 1)

  - `lv_mean`: latent mean (default 0)

- lvar:

  Character `"X"` or `"Y"`. Default `"X"`.

## Value

Character string of lavaan syntax

## See also

Other population-scripting functions:
[`scriptPopCor()`](https://jsakaluk.github.io/dySEM/reference/scriptPopCor.md)

## Examples

``` r
dvn <- dySEM::build_dvn(n_items_x = 3, x_stem = "x", x_delim1 = "", x_delim2 = "_",
  distinguish_1 = "A", distinguish_2 = "B")
scriptPopUni(dvn, "X", list(
  loadings = c(.75, .80, .55),
  residuals = c(.44, .36, .70),
  coresids = c(.10, .12, .08)
))
#> [1] "# Unidimensional Dyadic CFA - Population Model\n\n# Loadings\nXDy =~ 0.75*x1_A + 0.8*x2_A + 0.55*x3_A + 0.75*x1_B + 0.8*x2_B + 0.55*x3_B\n\n# Intercepts\nx1_A ~ 0*1\nx2_A ~ 0*1\nx3_A ~ 0*1\nx1_B ~ 0*1\nx2_B ~ 0*1\nx3_B ~ 0*1\n\n# Residual Variances\nx1_A ~~ 0.44*x1_A\nx2_A ~~ 0.36*x2_A\nx3_A ~~ 0.7*x3_A\nx1_B ~~ 0.44*x1_B\nx2_B ~~ 0.36*x2_B\nx3_B ~~ 0.7*x3_B\n\n# Residual Covariances\nx1_A ~~ 0.1*x1_B\nx2_A ~~ 0.12*x2_B\nx3_A ~~ 0.08*x3_B\n\n# Latent Variance\nXDy ~~ 1*XDy\n\n# Latent Mean\nXDy ~ 0*1"
```
