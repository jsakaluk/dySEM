# A Function That Exports Tables and/or SEM Diagrams based on dySEM models

This function takes the model from fitted dySEM() scripts and exports
table(s) and/or a path diagram figure of expected output.

## Usage

``` r
outputModel(
  dvn,
  model = NULL,
  fit,
  table = TRUE,
  tabletype = NULL,
  figure = TRUE,
  figtype = NULL,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- dvn:

  input dvn list from scrapeVarCross

- model:

  character input specifying type of model to output (e.g., "cfa",
  "apim", "cfm")

- fit:

  input object from fitted lavaan model

- table:

  logical input of whether table output is desired. Default is TRUE

- tabletype:

  character input of what type of table(s) is(are) desired. options are
  "measurement" (i.e,, loadings, intercepts,), "structural" (i.e.,
  latent slopes, such as actor/partner effects, k parameters), or "both"
  (i.e., both measurement and structural tables)

- figure:

  logical input of whether figure output is desired. Default is TRUE

- figtype:

  character input of what type of figure is desired

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

Ignore console (prints unnecessary semPlot::semPaths details). More
importantly, prints word files for the table(s) and/or figure, outputted
to the users working directory

## Details

If a file with the same name already exists in the user's chosen
directory, it will be overwritten.

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

sat.config.script <- scriptCor(dvnx, lvname = "Sat", constr_dy_meas = "none",
constr_dy_struct = "none")

sat.config.mod <- lavaan::cfa(sat.config.script, data = commitmentQ, std.lv = FALSE,
auto.fix.first= FALSE, meanstructure = TRUE)

outputModel(dvnx, model = "cfa", fit = sat.config.mod, table = TRUE,
tabletype = "measurement", figure = "TRUE", figtype = "standardized",
writeTo = tempdir(), fileName = "dCFA_configural")
#> Warning: `outputModel()` was deprecated in dySEM 1.1.0.
#> ℹ Please use `dySEM::outputParamTab` and/or `dySEM::outputParamFig` instead.
#> Output stored in /home/runner/work/dySEM/dySEM/docs/reference//tmp/RtmpIVCISQ/dCFA_configural std.png
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

apim.indist.script <-  scriptAPIM(dvnxy, lvxname = "Sat", lvyname = "Com", est_k = TRUE)

apim.indist.mod <- lavaan::cfa(apim.indist.script, data = commitmentQ, std.lv = FALSE,
auto.fix.first= FALSE, meanstructure = TRUE)

outputModel(dvnxy, model = "apim", fit = apim.indist.mod, table = TRUE,
tabletype = "measurement", figure = "TRUE", figtype = "standardized",
writeTo = tempdir(), fileName = "APIM_indist")
#> Output stored in /home/runner/work/dySEM/dySEM/docs/reference//tmp/RtmpIVCISQ/APIM_indist std.png
```
