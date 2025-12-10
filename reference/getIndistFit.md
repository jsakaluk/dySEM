# A Function that Computes Corrected Fit Indexes According to the ISAT and INULL Models of Olsen & Kenny (2006)

This function takes the outputted model fit using scriptCor() with model
= "indist", as well as scriptISAT(), and scriptINULL() and computes
corrected model fit indexes according to the approach outlined by Olsen
& Kenny (2006)

## Usage

``` r
getIndistFit(indmodel, isatmod, inullmod)
```

## Arguments

- indmodel:

  input lavaan model object fitted using dyadCFA(model =
  "indistinguishable")

- isatmod:

  input lavaan model object fitted using ISAT()

- inullmod:

  input lavaan model object fitted using INULL()

## Value

A data frame of the original and corrected chi sq, df, p, rmsea, and tli

## See also

Other supplemental model calculators:
[`getDyReliability()`](https://jsakaluk.github.io/dySEM/reference/getDyReliability.md),
[`getDydmacs()`](https://jsakaluk.github.io/dySEM/reference/getDydmacs.md)

## Examples

``` r
dvn <- scrapeVarCross(
  dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
  x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.indist.script <- scriptCor(dvn, lvname = "Sat")
sat.indist.mod <- lavaan::cfa(sat.indist.script,
  data = commitmentQ, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = TRUE
)

sat.isat.script <- scriptISAT(dvn, lvxname = "Sat")
sat.isat.mod <- lavaan::cfa(sat.isat.script,
  data = commitmentQ, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = FALSE
)

sat.inull.script <- scriptINULL(dvn, lvxname = "Sat")
sat.inull.mod <- lavaan::cfa(sat.inull.script,
  data = commitmentQ, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = FALSE
)

getIndistFit(sat.indist.mod, sat.isat.mod, sat.inull.mod)
#>   chi2_orig df_orig p_orig rmsea_orig tli_orig chi2_adj df_adj  p_adj rmsea_adj
#> 1   88.7346      44  1e-04      0.094   0.9632  29.5661     14 0.0088    0.0983
#>    tli_adj
#> 1 0.977164
```
