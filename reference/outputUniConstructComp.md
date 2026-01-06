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
commitmentM,
x_order = "sip",
x_stem = "sat.g",
x_delim1 = "",
x_delim2 = "_",
distinguish_1 = "f",
distinguish_2 = "m"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Total indicators: 10

# Quick example for CRAN checks
outputUniConstructComp(
  dvn,
  commitmentM,
  indexes = c("df", "bic"),
  missing = "listwise"
)
#> # A tibble: 4 × 6
#>   Model              Type            df   bic df_diff bic_diff
#>   <chr>              <chr>        <dbl> <dbl>   <dbl>    <dbl>
#> 1 Bifactor           Higher Order    20 8374.      NA     NA  
#> 2 Hierarchical       Higher Order    29 8336.       9    -38.2
#> 3 Correlated Factors Lower Order     29 8336.      NA     NA  
#> 4 Unidimensional     Lower Order     30 9316.       1    980. 

# \donttest{
# More comprehensive examples (slower due to FIML estimation)
outputUniConstructComp(
  dvn,
  commitmentM,
  missing = "fiml"
)
#> According to GenTEFI (-15.564),
#>           higher-order models (e.g., bifactor or hierarchical) fit the data better than lower-order models (e.g., correlated factors),
#>           as the higher-order TEFI (-9.606) is smaller than the lower-order TEFI (-5.959).
#> $Indexes
#> # A tibble: 4 × 14
#>   Model         Type  genTEFI    df  chisq   cfi  rmsea   bic df_diff chisq_diff
#>   <chr>         <chr> <chr>   <dbl>  <dbl> <dbl>  <dbl> <dbl>   <dbl>      <dbl>
#> 1 Bifactor      High… Yes        20   44.8 0.994 0.0663 8512.      NA       NA  
#> 2 Hierarchical  High… Yes        29   56.8 0.993 0.0583 8474.       9       12.0
#> 3 Correlated F… Lowe… No         29   56.8 0.993 0.0583 8474.      NA       NA  
#> 4 Unidimension… Lowe… No         30 1048.  0.743 0.347  9459.       1      991. 
#> # ℹ 4 more variables: chisq_diff_test_p <dbl>, cfi_diff <dbl>,
#> #   rmsea_diff <dbl>, bic_diff <dbl>
#> 
#> $`GenTEFI Details`
#> # A tibble: 1 × 3
#>   VN.Entropy.Fit Level_1_VN Level_2_VN
#>            <dbl>      <dbl>      <dbl>
#> 1          -15.6      -5.96      -9.61
#> 

outputUniConstructComp(
  dvn,
  commitmentM,
  indexes = c("df", "bic"),
  missing = "fiml"
)
#> # A tibble: 4 × 6
#>   Model              Type            df   bic df_diff bic_diff
#>   <chr>              <chr>        <dbl> <dbl>   <dbl>    <dbl>
#> 1 Bifactor           Higher Order    20 8512.      NA     NA  
#> 2 Hierarchical       Higher Order    29 8474.       9    -38.8
#> 3 Correlated Factors Lower Order     29 8474.      NA     NA  
#> 4 Unidimensional     Lower Order     30 9459.       1    985. 

outputUniConstructComp(
  dvn,
  commitmentM,
  indexes = c("df", "bic"),
  estimator = "ml",
  missing = "fiml"
)
#> # A tibble: 4 × 6
#>   Model              Type            df   bic df_diff bic_diff
#>   <chr>              <chr>        <dbl> <dbl>   <dbl>    <dbl>
#> 1 Bifactor           Higher Order    20 8512.      NA     NA  
#> 2 Hierarchical       Higher Order    29 8474.       9    -38.8
#> 3 Correlated Factors Lower Order     29 8474.      NA     NA  
#> 4 Unidimensional     Lower Order     30 9459.       1    985. 

outputUniConstructComp(
  dvn,
  commitmentM,
  indexes = c("df", "bic"),
  missing = "fiml",
  gtTab = TRUE,
  writeTo = tempdir(),
  fileName = "uni-construct-dyad-models"
)
#> Output stored in: /tmp/RtmpyIh8zQ/uni-construct-dyad-models.rtf


  

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

8512.446

NA

NA

Hierarchical

Higher Order

29

8473.677

9

-38.76866

Correlated Factors

Lower Order

29

8473.677

NA

NA

Unidimensional

Lower Order

30

9459.109

1

985.43122

\# }
