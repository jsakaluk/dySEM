# Calculates dmacs difference in expected indicator scores for between dyad members

Calculates dmacs difference in expected indicator scores for between
dyad members

## Usage

``` r
getDydmacs(dat, dvn, fit, nodewidth = 0.01, lowerLV = -5, upperLV = 5)
```

## Arguments

- dat:

  data frame of indicators

- dvn:

  input dvn list from scrapeVarCross

- fit:

  outputted dyadic cfa lavaan object; should be from a
  partial-invariance model

- nodewidth:

  space between nodes during quadrature approximation (default = .01)

- lowerLV:

  lowest latent variable value evaluated (default = -5)

- upperLV:

  greatest latent variable value evaluated (default = 5)

## Value

vector of d_macs values

## See also

Other supplemental model calculators:
[`getDyReliability()`](https://jsakaluk.github.io/dySEM/reference/getDyReliability.md),
[`getIndistFit()`](https://jsakaluk.github.io/dySEM/reference/getIndistFit.md)

## Examples

``` r
dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10
sat.config.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = "none",
constr_dy_struct = "none")

sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
auto.fix.first= FALSE, meanstructure = TRUE)
getDydmacs(commitmentQ, dvn, sat.config.mod)
#>       dMACS
#> [1,] 0.2034
#> [2,] 0.1755
#> [3,] 0.1695
#> [4,] 0.2776
#> [5,] 0.1770
```
