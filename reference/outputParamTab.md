# A Function That Exports Tables based on dySEM models

This function takes the model from fitted `dySEM` scripts and creates
tables of expected output and/or exports them as `.rtf`s.

## Usage

``` r
outputParamTab(
  dvn,
  model = NULL,
  fit,
  tabletype = NULL,
  gtTab = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  Input dvn list from
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)

- model:

  A character input specifying type of model to output: "cfa", "bidyc",
  "apim", "mim", "cfm", or "bidys".

- fit:

  input object from fitted `lavaan` model.

- tabletype:

  A character input of what type of table(s) is(are) desired. Options
  are "measurement" (i.e., loadings, intercepts, etc.), "structural"
  (i.e., latent slopes, such as actor/partner effects, k parameters),
  "both" (i.e., both measurement and structural tables), or
  "correlation" (for a table of factor correlations).

- gtTab:

  A logical input indicating whether to generate the table(s) in
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object
  format (`TRUE`). By default (`FALSE`), the table(s) are generated in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  format. Users can also apply the `writeTo` argument if they wish to
  export the [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)
  table object(s).

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

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
if `gtTab = FALSE` (default), or
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) object if
`gtTab = TRUE`, of specified model parameter estimates and corresponding
statistical tests.

## Details

- If `gtTab = TRUE` and `writeTo` is specified, then output will
  simultaneously be saved as a `.rtf` file to the user's specified
  directory.

- If output file(s) is(are) successfully saved, a confirmation message
  will be printed to the console.

- If a file with the same name already exists in the user's chosen
  directory, it will be overwritten.

## Examples

``` r
dvnx <- scrapeVarCross(
  dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
  x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10

sat.config.script <- scriptCor(dvnx,
  lvname = "Sat", constr_dy_meas = "none",
  constr_dy_struct = "none"
)

sat.config.mod <- lavaan::cfa(sat.config.script,
  data = commitmentQ, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = TRUE
)

outputParamTab(dvnx,
  model = "cfa", sat.config.mod, tabletype = "measurement",
  writeTo = tempdir(), fileName = "dCFA_configural"
)
#> # A tibble: 10 × 8
#>    `Latent Factor` Indicator Loading    SE     Z `p-value` `Std. Loading`
#>    <chr>           <chr>       <dbl> <dbl> <dbl> <chr>              <dbl>
#>  1 Sat1            sat.g.1_1    2.11 0.158  13.3 < .001             0.937
#>  2 Sat1            sat.g.1_2    1.91 0.168  11.4 < .001             0.851
#>  3 Sat1            sat.g.1_3    2.10 0.161  13.1 < .001             0.926
#>  4 Sat1            sat.g.1_4    1.96 0.162  12.0 < .001             0.884
#>  5 Sat1            sat.g.1_5    1.87 0.179  10.4 < .001             0.806
#>  6 Sat2            sat.g.2_1    1.83 0.144  12.7 < .001             0.91 
#>  7 Sat2            sat.g.2_2    1.84 0.146  12.7 < .001             0.908
#>  8 Sat2            sat.g.2_3    1.88 0.146  12.9 < .001             0.918
#>  9 Sat2            sat.g.2_4    1.61 0.151  10.7 < .001             0.819
#> 10 Sat2            sat.g.2_5    1.93 0.159  12.1 < .001             0.887
#> # ℹ 1 more variable: Intercept <dbl>

dvnxy <- scrapeVarCross(
  dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
  x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2",
  y_order = "spi", y_stem = "com", y_delim1 = ".", y_delim2 = "_"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 2 latent variables: sat.g and com
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ com: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 20

apim.indist.script <- scriptAPIM(dvnxy, lvxname = "Sat", lvyname = "Com", est_k = TRUE)

apim.indist.mod <- lavaan::cfa(apim.indist.script,
  data = commitmentQ, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = TRUE
)

outputParamTab(dvnxy,
  model = "cfa", sat.config.mod, tabletype = "measurement",
  writeTo = tempdir(), fileName = "APIM_indist"
)
#> NULL
```
