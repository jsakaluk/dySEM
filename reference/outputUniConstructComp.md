# A Function That Fits and Compares Competing Dyadic Uni-construct Models

This function takes the outputted object from
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
along with the corresponding dataset and automatically tests competing
uni-construct dyadic models for the latent variable under consideration.
It inspects four possible model variants:

- Bifactor (scripted via
  [`dySEM::scriptBifac`](https://jsakaluk.github.io/dySEM/reference/scriptBifac.md))

- Hierarchical (scripted via
  [`dySEM::scriptHier`](https://jsakaluk.github.io/dySEM/reference/scriptHier.md))

- Correlated Factors (scripted via
  [`dySEM::scriptCor`](https://jsakaluk.github.io/dySEM/reference/scriptCor.md))

- Unidimensional (scripted via
  [`dySEM::scriptUni`](https://jsakaluk.github.io/dySEM/reference/scriptUni.md))

## Usage

``` r
outputUniConstructComp(
  dvn,
  dat,
  indexes = c("df", "chisq", "cfi", "rmsea", "bic", "GenTEFI"),
  ...,
  gtTab = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  Input dvn list from
  [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md).

- dat:

  Input data frame containing the dataset for model estimation.

- indexes:

  Input character vector specifying which index(es) to return. Default
  is `c("df", "chisq", "cfi", "rmsea", "bic", "GenTEFI")`. **Note**:

  - Valid entries include "GenTEFI"—the Generalized Total Entropy Fit
    Index (see Golino et al., 2024)—and those from
    [`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).

  - If "chisq" is entered, chi-squared difference tests are
    automatically performed via
    [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html),
    and the resulting p-values are added to the output.

- ...:

  Additional arguments to be passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html), allowing
  users to customize model estimation settings. By default, the models
  are fit with maximum-likelihood estimation (`estimator = "ml"`) and
  missing data are handled via listwise deletion
  (`missing = "listwise"`), as per
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html)'s default
  behaviour.

- gtTab:

  A logical input indicating whether to generate the requested index(es)
  for each fitted model (requested via the `indexes` argument) in
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object
  format (`TRUE`). Users can also apply the `writeTo` argument if they
  wish to export the
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object.

- writeTo:

  A character vector string specifying a directory path to where the
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table object
  should be saved. If set to ".", the file will be written to the
  current working directory. The default is `NULL`, and examples use a
  temporary directory created by
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). `writeTo` is only
  relevant if `gtTab = TRUE`.

- fileName:

  A character string specifying a desired base name for the output
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) file. The
  resulting base name will automatically be appended with a `.rtf` file
  extension. `fileName` is only relevant if `gtTab = TRUE` and `writeTo`
  is specified.

## Value

A `list` containing up to two components:

- `Indexes`: A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  if `gtTab = FALSE` (default), or
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) object if
  `gtTab = TRUE`, with the desired index(es) for each fitted model
  (requested via the `indexes` argument).

- `GenTEFI`: A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  of the GenTEFI (if "GenTEFI" is included in the `indexes` argument).

## Details

- If "chisq" is included in `indexes`, the specific form of the applied
  chi-squared difference test (e.g., standard vs. robust) is determined
  automatically by
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html),
  based on the model estimation method used.

- If `gtTab = TRUE` and `writeTo` is specified, then output will
  simultaneously be saved as a `.rtf` file to the user's specified
  directory.

- If output file is successfully saved, a confirmation message will be
  printed to the console.

- If a file with the same name already exists in the user's chosen
  directory, it will be overwritten.

## Examples

``` r
dvn <- scrapeVarCross(
  commitmentQ,
  x_order = "spi",
  x_stem = "sat.g",
  x_delim1 = ".",
  x_delim2="_",
  distinguish_1="1",
  distinguish_2="2"
  )
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10
  
  outputUniConstructComp(
  dvn,
  commitmentQ,
  missing = "fiml"
  )
#> Error in dplyr::mutate(., genTEFI = dplyr::case_when(gen_tefi$Lower.Order.VN <     gen_tefi$Higher.Order.VN & Model %in% c("Unidimensional",     "Correlated Factors") ~ "Yes", gen_tefi$Higher.Order.VN <     gen_tefi$Lower.Order.VN & Model %in% c("Hierarchical", "Bifactor") ~     "Yes", TRUE ~ "No")): ℹ In argument: `genTEFI = dplyr::case_when(...)`.
#> Caused by error:
#> ! `genTEFI` must be size 4 or 1, not 0.
  
  outputUniConstructComp(
  dvn,
  commitmentQ,
  indexes = c("df", "bic"),
  missing = "fiml"
  )
#> # A tibble: 4 × 6
#>   Model              Type            df   bic df_diff bic_diff
#>   <chr>              <chr>        <dbl> <dbl>   <dbl>    <dbl>
#> 1 Bifactor           Higher Order    20 4077.      NA     NA  
#> 2 Hierarchical       Higher Order    29 4042.       9    -35.2
#> 3 Correlated Factors Lower Order     29 4042.      NA     NA  
#> 4 Unidimensional     Lower Order     30 4223.       1    181. 
  
  outputUniConstructComp(
  dvn,
  commitmentQ,
  indexes = c("df", "bic"),
  estimator = "ml",
  missing = "fiml"
  )
#> # A tibble: 4 × 6
#>   Model              Type            df   bic df_diff bic_diff
#>   <chr>              <chr>        <dbl> <dbl>   <dbl>    <dbl>
#> 1 Bifactor           Higher Order    20 4077.      NA     NA  
#> 2 Hierarchical       Higher Order    29 4042.       9    -35.2
#> 3 Correlated Factors Lower Order     29 4042.      NA     NA  
#> 4 Unidimensional     Lower Order     30 4223.       1    181. 
  
  outputUniConstructComp(
  dvn,
  commitmentQ,
  indexes = c("df", "bic"),
  missing = "fiml",
  gtTab = TRUE,
  writeTo = tempdir(),
  fileName = "uni-construct-dyad-models"
  )
#> Output stored in: /tmp/RtmpjwRMMo/uni-construct-dyad-models.rtf


  

Model
```

Type

df

bic

df_diff

bic_diff

Bifactor

Higher Order

20

4077.220

NA

NA

Hierarchical

Higher Order

29

4042.068

9

-35.1525

Correlated Factors

Lower Order

29

4042.068

NA

NA

Unidimensional

Lower Order

30

4222.850

1

180.7826
