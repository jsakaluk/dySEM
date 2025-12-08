# A function that performs a score test for relaxing each invariance equality constraint between partners in a given dyadic SEM model.

A function that performs a score test for relaxing each invariance
equality constraint between partners in a given dyadic SEM model.

## Usage

``` r
getConstraintTests(constrainFit, filterSig = FALSE)
```

## Arguments

- constrainFit:

  fitted lavaan model with dyadic invariance equality constraints

- filterSig:

  logical indicating whether to filter for significant constraints
  (default is FALSE)

## Value

a data frame with rows of equality constraints (now with readable param
labels) and test statistic, df, and p for whether constraint worsens
model fit

## Examples

``` r
dvn <- scrapeVarCross(dat = commitmentM, x_order = "sip", x_stem = "sat.g",
x_delim2="_", distinguish_1="f", distinguish_2="m")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Total indicators: 10

sat.resids.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = c("loadings", "intercepts", "residuals"),
constr_dy_struct = "none")

sat.resids.mod <- lavaan::cfa(sat.resids.script, data = commitmentM, std.lv = FALSE,
auto.fix.first= FALSE, meanstructure = TRUE)

getConstraintTests(sat.resids.mod)
#> Warning: `getConstraintTests()` was deprecated in dySEM 1.1.0.
#> ℹ Please use `dySEM::outputConstraintTab()` instead.
#>                  param1 constraint               param2   chi2 df pvalue  sig
#> 1      Satf =~ sat.g1_f         ==     Satm =~ sat.g1_m  1.131  1  0.288 <NA>
#> 2      Satf =~ sat.g2_f         ==     Satm =~ sat.g2_m  0.633  1  0.426 <NA>
#> 3      Satf =~ sat.g3_f         ==     Satm =~ sat.g3_m  0.060  1  0.806 <NA>
#> 4      Satf =~ sat.g4_f         ==     Satm =~ sat.g4_m  1.839  1  0.175 <NA>
#> 5      Satf =~ sat.g5_f         ==     Satm =~ sat.g5_m  3.603  1  0.058 <NA>
#> 6          sat.g1_f ~1          ==         sat.g1_m ~1   0.057  1  0.812 <NA>
#> 7          sat.g2_f ~1          ==         sat.g2_m ~1   1.316  1  0.251 <NA>
#> 8          sat.g3_f ~1          ==         sat.g3_m ~1   0.048  1  0.827 <NA>
#> 9          sat.g4_f ~1          ==         sat.g4_m ~1   0.103  1  0.748 <NA>
#> 10         sat.g5_f ~1          ==         sat.g5_m ~1   2.090  1  0.148 <NA>
#> 11 sat.g1_f ~~ sat.g1_f         == sat.g1_m ~~ sat.g1_m 22.977  1  0.000  ***
#> 12 sat.g2_f ~~ sat.g2_f         == sat.g2_m ~~ sat.g2_m  0.263  1  0.608 <NA>
#> 13 sat.g3_f ~~ sat.g3_f         == sat.g3_m ~~ sat.g3_m  0.317  1  0.573 <NA>
#> 14 sat.g4_f ~~ sat.g4_f         == sat.g4_m ~~ sat.g4_m  2.422  1  0.120 <NA>
#> 15 sat.g5_f ~~ sat.g5_f         == sat.g5_m ~~ sat.g5_m 17.185  1  0.000  ***
```
