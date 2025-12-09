# A function That Exports SEM diagrams based on dySEM models

This function takes the model from fitted `dySEM` scripts and exports
`.png` path diagram figures of expected output.

## Usage

``` r
outputParamFig(fit, figtype = NULL, writeTo = NULL, fileName = NULL)
```

## Arguments

- fit:

  Input object from fitted `lavaan` model.

- figtype:

  A character input of what type of model is desired:

  - "unstandardized" for unstandardized path coefficients.

  - "standardized" for standardized path coefficients.

  - "labels" for labelled parameters.

- writeTo:

  A character string specifying a directory path to where the output
  file of the path diagram should be saved. If set to “.”, the file will
  be written to the current working directory. The default is `NULL`
  (which will throw an error), and examples use a temporary directory
  created by [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- fileName:

  A character string specifying a desired base name for the output file.
  If a `fileName` not provided (i.e., fileName = `NULL`), then a default
  will be used depending on the specified `figtype` (e.g., "dySEM_figure
  unstd", "dySEM_figure std", or "dySEM_figure lab"). The specified name
  will automatically be appended with the `.png` file extension.

## Value

A `qgraph` object of the desired SEM path diagram, which is
simultaneously exported as a `.png` file to the specified directory.

## Details

- The function uses
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  to create a `qgraph` object of the desired SEM path diagram.

- After execution, a
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  message will be printed to the console confirming the directory path
  of the saved output file.

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

outputParamFig(sat.config.mod,
  figtype = "standardized",
  writeTo = tempdir(), fileName = "dCFA_configural"
)
#> Output stored in /home/runner/work/dySEM/dySEM/docs/reference//tmp/RtmpONn732/dCFA_configural std.png

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

outputParamFig(apim.indist.mod,
  figtype = "standardized",
  writeTo = tempdir(), fileName = "APIM_indist"
)
#> Output stored in /home/runner/work/dySEM/dySEM/docs/reference//tmp/RtmpONn732/APIM_indist std.png
```
