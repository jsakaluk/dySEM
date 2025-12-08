# A Function Calculates Omega Total Coefficients from a Dyadic CFA

This function takes the model from fitted scriptCor() scripts and
returns omega total coefficients for each dyad member, adapted following
Formula 2 in McNeish (2018).

## Usage

``` r
getDyReliability(dvn, fit)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- fit:

  outputted dyadic cfa lavaan object based on the default (i.e.,
  "configural") dyadCFA() function

## Value

a tibble/data frame with calculated omega total coefficients for dyad
Member 1 and Member 2

## See also

Other supplemental model calculators:
[`getDydmacs()`](https://jsakaluk.github.io/dySEM/reference/getDydmacs.md),
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
sat.indist.script <-  scriptCor(dvn, lvname = "Sat")
sat.indist.mod <- lavaan::cfa(sat.indist.script, data = commitmentQ, std.lv = FALSE,
auto.fix.first= FALSE, meanstructure = TRUE)
getDyReliability(dvn, sat.indist.mod)
#> # A tibble: 1 × 2
#>   omega.1 omega.2
#>     <dbl>   <dbl>
#> 1   0.948   0.948
```
