# A Function That Writes, Saves, and Exports Syntax for Fitting the I-NULL model for indistinguishable dyads

This function takes the outputted object from scrapeVarCross() and
automatically writes, returns, and exports (.txt) lavaan() syntax for
the I-NULL model described in Olsen & Kenny (2006)

## Usage

``` r
scriptINULL(
  dvn,
  lvxname = "X",
  lvyname = NULL,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- lvxname:

  input character to (arbitrarily) name X LV in lavaan syntax

- lvyname:

  (optional) input character to (arbitrarily) name Y LV in lavaan syntax

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

character object of lavaan script that can be passed immediately to
lavaan functions

## See also

[`scrapeVarCross`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
which this function relies on

Other indistinguishable script-writing functions:
[`scriptISAT()`](https://jsakaluk.github.io/dySEM/reference/scriptISAT.md)

## Examples

``` r
dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_",
x_delim2=".", x_item_num="\\d+", distinguish_1="1", distinguish_2="2")
#> 
#> ── Variable Scraping Summary ──
#> 
#> ✔ Successfully scraped 1 latent variable: PRQC
#> ℹ PRQC: 9 indicators for P1 (1), 9 indicators for P2 (2)
#> ℹ Total indicators: 18
qual.inull.script <- scriptINULL(dvn, lvxname = "Qual",
writeTo = tempdir(),
fileName = "I-NULL_script")
```
