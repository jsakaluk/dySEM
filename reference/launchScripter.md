# Launch the dySEM Interactive Scripter

Opens a Shiny application that guides researchers through selecting a
dyadic model, configuring invariance constraints and variable naming
patterns, previewing a path diagram, and generating the corresponding
lavaan syntax.

## Usage

``` r
launchScripter(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical; if `TRUE` (the default), the app opens in the system's
  default web browser. Set to `FALSE` to use the RStudio viewer pane
  instead.

## Value

Called for its side effect (launching the app). Returns the value from
[`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) invisibly.

## Details

The app supports uni-construct (unidimensional, correlated,
hierarchical, bifactor), bi-construct (L-APIM, Common Fate, Mutual
Influence, Two-Construct), and multi-construct (multiple correlated
factors) models.

No data upload is required. The app constructs variable names from
naming patterns you specify, generates lavaan syntax via dySEM
scripters, and provides a path diagram preview, copy-to-clipboard,
download, and reproducible R code.

Requires the shiny, bslib, and shinyjs packages (listed in Suggests).

## Examples

``` r
if (FALSE) { # \dontrun{
launchScripter()
} # }
```
