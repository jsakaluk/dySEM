# A Variable Name-Scraping and Indexing Function for cross-sectional data

This function scrapes the names of indicator variables in a wide-format
data set used for dyadic analyses of two latent variables (LV; X and Y),
and indexes which indicators correspond to which partner, for which LV.
It is used primarily to guide the syntax-writing of the other dySEM
functions.

## Usage

``` r
scrapeVarCross(
  dat,
  x_order = "spi",
  x_stem,
  x_delim1 = NULL,
  x_delim2 = NULL,
  x_item_num = "\\d+",
  distinguish_1 = "1",
  distinguish_2 = "2",
  y_order = NULL,
  y_stem = NULL,
  y_delim1 = NULL,
  y_delim2 = NULL,
  y_item_num = "\\d+",
  var_list = NULL,
  var_list_order = NULL,
  var_list_item_num = "\\d+",
  covs_order = NULL,
  covs_stem = NULL,
  covs_delim1 = NULL,
  covs_delim2 = NULL
)
```

## Arguments

- dat:

  input data frame of indicators of a particular LV

- x_order:

  input character for order of (S)tem, (P)artner number, and (I)tem
  number when creating variable names. Defaults to "spi"
  (Qualtrics-friendly), but can alternatively take "sip" or "psi"

- x_stem:

  input character stem of indicator variables for LV X

- x_delim1:

  optional character to separate stem from partner number (spi) or item
  number (sip)

- x_delim2:

  optional character to separate stem/partner number (spi) or stem/item
  number (sip) from from final element of variable name

- x_item_num:

  defaults to scrape all items that match the stem with any digits that
  follow. Will be updated to allow particular range of values, to make
  more sub-scale friendly.

- distinguish_1:

  input character used as the identifier for the first partner

- distinguish_2:

  input character used as the identifier for the first partner

- y_order:

  optional character for order of (S)tem, (P)artner number, and (I)tem
  number when creating variable names. Defaults to "spi"
  (Qualtrics-friendly), but can alternatively take "sip" or "psi". This
  and other Y-arguments only necessary if there is a latent Y variable
  to model

- y_stem:

  optional input character stem of indicator variables for LV X

- y_delim1:

  optional character to separate stem from partner number (spi) or item
  number (sip)

- y_delim2:

  optional character to separate stem/partner number (spi) or stem/item
  number (sip) from from final element of variable name

- y_item_num:

  defaults to scrape all items that match the stem with any digits that
  follow. Will be updated to allow particular range of values, to make
  more sub-scale friendly.

- var_list:

  optional named list of indicator variable information, if more than
  one LV is to be scripted (e.g., a dyadic CFA with multiple sub-scales
  from the same measure). If supplied, this list *must* contain the
  following elements: "stem" (a vector of stems), "delim1" (a vector of
  delimiting characters), and"delim2" (a vector of subsequently
  delimiting characters). Optionally may include numeric vectors
  "min_num" and "max_num" if indicators for different LVs share the same
  stem and must be separated by range of item numbers within a measure.

- var_list_order:

  optional character for order of (S)tem, (P)artner number, and (I)tem
  number for any of the indicator variables of a multi-LV model (i.e.,
  this functionality assumes the same ordering of elements throughout)

- var_list_item_num:

  optional character for item number of any of the indicator variables
  of a multi-LV model

- covs_order:

  optional character for order of (S)tem, (P)artner number, and (I)tem
  number for any covariate(s). Defaults to NULL. This and other
  covariate arguments only necessary if there are covariates to be
  scripted in your model(s).

- covs_stem:

  optional input character stem(s) of indicator variables for
  covariate(s). Can accept a single stem (e.g., "anx"), or a vector of
  stems (e.g., c("anx", "dep")). Defaults to NULL.

- covs_delim1:

  optional character to separate stem from partner number (spi) or item
  number (sip) for covariate(s). Defaults to NULL.

- covs_delim2:

  optional character to separate stem/partner number (spi) or stem/item
  number (sip) from

## Value

a list, referred in short-hand as a "dvn" (dyad variable names list)
containing variable names for p1, p2, \# of items per LV, characters
distinguishing partners, and total number of indicators

## Examples

``` r
dvnx <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: sat.g
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 10
dvnxy <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2",
y_order="spi", y_stem="com", y_delim1 = ".", y_delim2="_")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 2 latent variables: sat.g and com
#> ℹ sat.g: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ com: 5 indicators for P1 (1), 5 indicators for P2 (2)
#> ℹ Total indicators: 20
```
