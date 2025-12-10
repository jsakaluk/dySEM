# Compare model fit of nested dyadic invariance models in order from most parsimonious (residual) to least parsimonious (configural)

Compare model fit of nested dyadic invariance models in order from most
parsimonious (residual) to least parsimonious (configural)

## Usage

``` r
getInvarCompTable(mods)
```

## Arguments

- mods:

  a list of neted lavaan dyadic invariance models, in the order of
  residual, intercept, loading, configural

## Value

a data frame of model fit statistics for each model, as well as the
difference in fit statistics between each model and the previous model

## Examples

``` r
dvn <- scrapeVarCross(
  dat = commitmentQ, x_order = "spi",
  x_stem = "sat.g", x_delim1 = ".", x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.residual.script <- scriptCor(dvn,
  lvname = "Sat",
  constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none"
)

sat.intercept.script <- scriptCor(dvn,
  lvname = "Sat",
  constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none"
)

sat.loading.script <- scriptCor(dvn,
  lvname = "Sat",
  constr_dy_meas = c("loadings"), constr_dy_struct = "none"
)

sat.config.script <- scriptCor(dvn,
  lvname = "Sat",
  constr_dy_meas = "none", constr_dy_struct = "none"
)

sat.residual.fit <- lavaan::cfa(sat.residual.script,
  data = commitmentQ,
  std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
)

sat.intercept.fit <- lavaan::cfa(sat.intercept.script,
  data = commitmentQ,
  std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
)

sat.loading.fit <- lavaan::cfa(sat.loading.script,
  data = commitmentQ,
  std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
)

sat.config.fit <- lavaan::cfa(sat.config.script,
  data = commitmentQ,
  std.lv = FALSE, auto.fix.first = FALSE, meanstructure = TRUE
)

mods <- list(sat.residual.fit, sat.intercept.fit, sat.loading.fit, sat.config.fit)

getInvarCompTable(mods)
#> Warning: `getInvarCompTable()` was deprecated in dySEM 1.1.0.
#> ℹ Please use `dySEM::outputInvarCompTab()` instead.
#>          mod  chisq df pvalue      aic      bic rmsea   cfi chisq_diff df_diff
#> 1   residual 78.842 42  0.000 3855.175 3918.308 0.087 0.970         NA      NA
#> 2  intercept 60.321 37  0.009 3846.654 3923.512 0.074 0.981    -18.521      -5
#> 3    loading 58.810 33  0.004 3853.143 3940.981 0.082 0.979     -1.511      -4
#> 4 configural 53.026 29  0.004 3855.359 3954.176 0.085 0.981     -5.784      -4
#>   p_diff aic_diff bic_diff rmsea_diff cfi_diff
#> 1     NA       NA       NA         NA       NA
#> 2  0.002   -8.521    5.204     -0.013    0.011
#> 3  0.825    6.489   17.469      0.008   -0.002
#> 4  0.216    2.216   13.196      0.002    0.001
```
