# Compare Model Fit of Nested Dyadic Invariance Models

`outputInvarCompTab()` is used to compare the model fit of nested dyadic
invariance models in order from most parsimonious (residual) to least
parsimonious (configural)

## Usage

``` r
outputInvarCompTab(
  mods,
  parsimonyFirst = FALSE,
  gtTab = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- mods:

  A list of nested `lavaan` dyadic invariance models, in the order of
  residual, intercept, loading, configural

- parsimonyFirst:

  A logical input indicating whether to prioritize the residual dyadic
  invariance (i.e. most parsimonious measurement model) as the baseline
  model for nested comparisons, or to prioritize the configural dyadic
  invariance (i.e. least parsimonious measurement model). Defaults to
  FALSE (i.e., configural dyadic invariance is the baseline model).

- gtTab:

  A logical input indicating whether to generate the output in
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object
  format (`TRUE`). By default (`FALSE`), the output is generated in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  format. Users can also apply the `writeTo` argument if they wish to
  export the `gt:gt()` table object.

- writeTo:

  A character string specifying a directory path to where the
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object
  should be saved. If set to ".", the file will be written to the
  current working directory. The default is `NULL`, and examples use a
  temporary directory created by
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). `writeTo` is only
  relevant if `gtTab = TRUE`.

- fileName:

  A character string specifying a desired base name for the output
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) file. If a
  `fileName` is not provided (i.e., `fileName = NULL`), then a default
  will be used (i.e., "dySEM_table"). The resulting base name will
  automatically be appended with a `.rtf` file extension. `fileName` is
  only relevant if `gtTab = TRUE` and `writeTo` is specified.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
if `gtTab = FALSE` (default), or
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) object if
`gtTab = TRUE`, of model fit statistics for each model, as well as the
difference in fit statistics between each model and the previous model

## Details

- If `gtTab = TRUE` and `writeTo` is specified, then output will
  simultaneously be saved as a `.rtf` file to the user's specified
  directory.

- If output file is successfully saved, a confirmation message will be
  printed to the console.

- If a file with the same name already exists in the user's chosen
  directory, it will be overwritten.

## Examples

``` r
dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi",
x_stem = "sat.g", x_delim1 = ".", x_delim2="_", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.residual.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = "none")

sat.intercept.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = "none")

sat.loading.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = c("loadings"), constr_dy_struct = "none")

sat.config.script <- scriptCor(dvn, lvname = "Sat",
constr_dy_meas = "none", constr_dy_struct = "none")

sat.residual.fit <- lavaan::cfa(sat.residual.script, data = commitmentQ,
std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

sat.intercept.fit <- lavaan::cfa(sat.intercept.script, data = commitmentQ,
std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

sat.loading.fit <- lavaan::cfa(sat.loading.script, data = commitmentQ,
std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

sat.config.fit <- lavaan::cfa(sat.config.script, data = commitmentQ,
std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

mods <- list(sat.residual.fit, sat.intercept.fit, sat.loading.fit, sat.config.fit)

outputInvarCompTab(mods, parsimonyFirst = FALSE,
gtTab = TRUE, writeTo = tempdir(), fileName = "dCFA_Invar_Standard")
#> Output stored in: /tmp/RtmpAu6CmA/dCFA_Invar_Standard.rtf


  

mod
```

chisq

df

pvalue

aic

bic

rmsea

cfi

chisq_diff

df_diff

p_diff

aic_diff

bic_diff

rmsea_diff

cfi_diff

configural

78.842

42

0.000

3855.175

3918.308

0.087

0.970

NA

NA

NA

NA

NA

NA

NA

loading

60.321

37

0.009

3846.654

3923.512

0.074

0.981

-18.521

-5

0.002

-8.521

5.204

-0.013

0.011

intercept

58.810

33

0.004

3853.143

3940.981

0.082

0.979

-1.511

-4

0.825

6.489

17.469

0.008

-0.002

residual

53.026

29

0.004

3855.359

3954.176

0.085

0.981

-5.784

-4

0.216

2.216

13.196

0.002

0.001

mods \<- [list](https://rdrr.io/r/base/list.html)(sat.config.fit,
sat.loading.fit, sat.intercept.fit, sat.residual.fit)
outputInvarCompTab(mods, parsimonyFirst = TRUE, gtTab = TRUE, writeTo =
[tempdir](https://rdrr.io/r/base/tempfile.html)(), fileName =
"dCFA_Invar_Reverse") \#\> Output stored in:
/tmp/RtmpAu6CmA/dCFA_Invar_Reverse.rtf

| mod        |  chisq |  df | pvalue |      aic |      bic | rmsea |   cfi | chisq_diff | df_diff | p_diff | aic_diff | bic_diff | rmsea_diff | cfi_diff |
|:-----------|-------:|----:|-------:|---------:|---------:|------:|------:|-----------:|--------:|-------:|---------:|---------:|-----------:|---------:|
| residual   | 53.026 |  29 |  0.004 | 3855.359 | 3954.176 | 0.085 | 0.981 |         NA |      NA |     NA |       NA |       NA |         NA |       NA |
| intercept  | 58.810 |  33 |  0.004 | 3853.143 | 3940.981 | 0.082 | 0.979 |      5.784 |       4 |  0.216 |   -2.216 |  -13.196 |     -0.002 |   -0.001 |
| loading    | 60.321 |  37 |  0.009 | 3846.654 | 3923.512 | 0.074 | 0.981 |      1.511 |       4 |  0.825 |   -6.489 |  -17.469 |     -0.008 |    0.002 |
| configural | 78.842 |  42 |  0.000 | 3855.175 | 3918.308 | 0.087 | 0.970 |     18.521 |       5 |  0.002 |    8.521 |   -5.204 |      0.013 |   -0.011 |
