# Build a Dyad Variable Names (dvn) List Without a Data Frame

Creates a dvn structure for use with dySEM population scripters and
simulation workflows. Variable names are generated directly from naming
conventions without requiring an actual data frame. Compatible with
`scrapeVarCross` naming patterns.

## Usage

``` r
build_dvn(
  n_items_x,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B"
)
```

## Arguments

- n_items_x:

  Number of indicators per partner (X construct; future Y support).

- x_order:

  Order of (S)tem, (I)tem, (P)artner in variable names. One of `"sip"`,
  `"spi"`, `"psi"`. Default `"sip"` produces `x1_A`, `x2_A`, etc.

- x_stem:

  Character stem of indicator variables (e.g., `"x"`).

- x_delim1:

  Character separating stem from item or partner. Use `""` for none.

- x_delim2:

  Character separating item from partner (sip) or partner from item
  (spi).

- distinguish_1:

  Character identifying the first partner (e.g., `"A"`).

- distinguish_2:

  Character identifying the second partner (e.g., `"B"`).

## Value

A list compatible with dySEM dvn structure:

- p1xvarnames:

  Character vector of variable names for partner 1

- p2xvarnames:

  Character vector of variable names for partner 2

- xindper:

  Number of indicators per partner

- dist1:

  First partner distinguisher

- dist2:

  Second partner distinguisher

- indnum:

  Total number of indicators

## See also

Other variable-scraping functions:
[`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)

## Examples

``` r
dvn <- build_dvn(
  n_items_x = 3,
  x_order = "sip",
  x_stem = "x",
  x_delim1 = "",
  x_delim2 = "_",
  distinguish_1 = "A",
  distinguish_2 = "B"
)
# Produces x1_A, x2_A, x3_A and x1_B, x2_B, x3_B
```
