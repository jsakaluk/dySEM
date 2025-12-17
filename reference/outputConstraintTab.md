# Evaluate Invariance Equality Constraints in a specified Dyadic Invariance Model

`outputConstraintTab()` is used to perform a score test for relaxing
each invariance equality constraint between partners in a given dyadic
SEM model.

## Usage

``` r
outputConstraintTab(
  constrainFit,
  filterSig = FALSE,
  gtTab = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- constrainFit:

  A fitted `lavaan` model with dyadic invariance equality constraints

- filterSig:

  A logical indicating whether to filter for significant constraints
  (default is `FALSE`)

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
`gtTab = TRUE`, with rows of equality constraints (now with readable
`param` labels) and test statistic, *df*, and *p* for whether constraint
worsens model fit.

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
dvn <- scrapeVarCross(
  dat = commitmentM, x_order = "sip", x_stem = "sat.g",
  x_delim2 = "_", distinguish_1 = "f", distinguish_2 = "m"
)
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (f), 5 indicators for P2 (m)
#> ℹ Total indicators: 10

sat.resids.script <- scriptCor(dvn,
  lvname = "Sat",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = "none"
)

sat.resids.mod <- lavaan::cfa(sat.resids.script,
  data = commitmentM, std.lv = FALSE,
  auto.fix.first = FALSE, meanstructure = TRUE
)

outputConstraintTab(sat.resids.mod,
  filterSig = FALSE,
  gtTab = TRUE, writeTo = tempdir(), fileName = "dCFA_Residual"
)
#> Output stored in: /tmp/RtmpdAO9IC/dCFA_Residual.rtf


  

param1
```

constraint

param2

chi2

df

pvalue

sig

Satf =~ sat.g1_f

==

Satm =~ sat.g1_m

1.131

1

0.288

NA

Satf =~ sat.g2_f

==

Satm =~ sat.g2_m

0.633

1

0.426

NA

Satf =~ sat.g3_f

==

Satm =~ sat.g3_m

0.060

1

0.806

NA

Satf =~ sat.g4_f

==

Satm =~ sat.g4_m

1.839

1

0.175

NA

Satf =~ sat.g5_f

==

Satm =~ sat.g5_m

3.603

1

0.058

NA

sat.g1_f ~1

==

sat.g1_m ~1

0.057

1

0.812

NA

sat.g2_f ~1

==

sat.g2_m ~1

1.316

1

0.251

NA

sat.g3_f ~1

==

sat.g3_m ~1

0.048

1

0.827

NA

sat.g4_f ~1

==

sat.g4_m ~1

0.103

1

0.748

NA

sat.g5_f ~1

==

sat.g5_m ~1

2.090

1

0.148

NA

sat.g1_f \~~ sat.g1_f

==

sat.g1_m \~~ sat.g1_m

22.977

1

0.000

\*\*\*

sat.g2_f \~~ sat.g2_f

==

sat.g2_m \~~ sat.g2_m

0.263

1

0.608

NA

sat.g3_f \~~ sat.g3_f

==

sat.g3_m \~~ sat.g3_m

0.317

1

0.573

NA

sat.g4_f \~~ sat.g4_f

==

sat.g4_m \~~ sat.g4_m

2.422

1

0.120

NA

sat.g5_f \~~ sat.g5_f

==

sat.g5_m \~~ sat.g5_m

17.185

1

0.000

\*\*\*
