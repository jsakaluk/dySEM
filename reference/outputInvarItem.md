# Output Item-Level Invariance Summary Table

`outputInvarItem()` generates a summary table showing which items
exhibit noninvariance in loadings and/or intercepts based on a partial
invariance model script, along with dMACS effect sizes for each item.

## Usage

``` r
outputInvarItem(
  dvn,
  fit,
  partialScript,
  dat,
  nodewidth = 0.01,
  lowerLV = -5,
  upperLV = 5,
  gtTab = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- fit:

  outputted dyadic cfa lavaan object from the partial invariance model

- partialScript:

  character string containing the lavaan syntax for the partial
  invariance model

- dat:

  data frame of indicators used to fit the model (needed for dMACS
  calculation)

- nodewidth:

  space between nodes during quadrature approximation for dMACS (default
  = .01)

- lowerLV:

  lowest latent variable value evaluated for dMACS (default = -5)

- upperLV:

  greatest latent variable value evaluated for dMACS (default = 5)

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
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) file.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
if `gtTab = FALSE` (default), or
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) object if
`gtTab = TRUE`, with columns for Item, LoadingNoninvariance,
InterceptNoninvariance, and dMACS. For multi-LV structures, an
additional LV column is included; in gt tables, this is used as the row
group label with Item names as row labels within each group.

## Details

- The function parses the lavaan syntax to determine which loadings and
  intercepts are constrained to equality across partners (invariant)
  versus freed (noninvariant).

- LoadingNoninvariance = "Yes" indicates the loading was freed in the
  partial invariance model.

- InterceptNoninvariance = "Yes" indicates the intercept was freed in
  the partial invariance model.

- dMACS values are calculated using the
  [`getDydmacs()`](https://jsakaluk.github.io/dySEM/reference/getDydmacs.md)
  function.

- If `gtTab = TRUE` and `writeTo` is specified, then output will
  simultaneously be saved as a `.rtf` file to the user's specified
  directory.

- If output file is successfully saved, a confirmation message will be
  printed to the console.

- If a file with the same name already exists in the user's chosen
  directory, it will be overwritten.

## Examples

``` r
if (FALSE) { # \dontrun{
dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
                      x_delim2 = "_", distinguish_1 = "1", distinguish_2 = "2")

# Create and fit a partial invariance model
partial.script <- scriptCor(dvn, lvname = "Sat",
                            constr_dy_meas = c("loadings", "intercepts"),
                            constr_dy_struct = "none")

partial.fit <- lavaan::cfa(partial.script, data = commitmentQ, std.lv = FALSE,
                           auto.fix.first = FALSE, meanstructure = TRUE)

outputInvarItem(dvn, partial.fit, partial.script, dat = commitmentQ,
                gtTab = TRUE, writeTo = tempdir(), fileName = "item_invar")
} # }
```
