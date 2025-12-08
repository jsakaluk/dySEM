# Changelog

## dySEM 1.4.0

- outputParamTab() can now generate df of correlation matrix of latent
  variables in the model
- vignettes for Correlated Dyadic Factors Model, Mulitple Correlated
  Dyadic Factors Model, and Latent APIM written in advance of
  preprint/submission of Sakaluk & Camanto

## dySEM 1.3.2

- @omarjcamanto has built a beauty function called
  outputUniConstructComp() for comparing different varieties of
  uniconstruct models.
- added a new argument (includeMeanStruct) to scriptAPIM() (and soon to
  be for other bi-construct scripters). By defaulting to FALSE, this
  will enable these scripted models to cooperate with dynamic fit
  indexes (which currently do not support mean structures)

## dySEM 1.3.1

- deep dive ensuring correct specification + unit tests for all
  uniconstruct models across FF and MV scale-setting options

## dySEM 1.3.0

- add two new exemplar datasets, imsM and prqcQ (both of which contain
  multi-construct data) for use in the vignettes and examples
- scriptHier() and scriptBifac() added to list of uni-construct
  scripters

## dySEM 1.2.1

- scriptCor() functionality assumes scripting of correlated factor
  models for 1-construct dyadic models; multi-construct correlated
  factor models now handled by scriptCFA() (previously for uni-construct
  correlated factor models)

## dySEM 1.2.0

### Major changes

- New scriptUni() functionality for univariate dyadic SEM models, lead
  [@omarjcamanto](https://github.com/omarjcamanto)
- New functionality for dyadic CFA models with multiple factors (e.g.,
  from subscales), lead by [@jsakaluk](https://github.com/jsakaluk)

### Minor changes

- New optionality for ordering in scrapeVarCross (Partner-Stem-Item
  \[“psi”\])
- Improved test coverage (esp. for new functionality)

## dySEM 1.1.1

CRAN release: 2025-01-07

- A revamping of the outputting functionality, lead
  [@omarjcamanto](https://github.com/omarjcamanto)

### Major changes

- Deprecated outputModel(), in favour of two separate functions:
  outputParamFig() (for path diagram), and outputParamTab() (for table
  of parameter estimates)
- Deprecated getInvarCompTable(), in favour of outputInvarCompTable()
  (for table of invariance comparisons); this now returns a more
  flexible data frame (with optionality for gt())
- Created outputContraintTests() (for table of parameter-specific dyadic
  noninvariance tests with Langrange multipliers); this now returns a
  more flexible tibble (with optionality for gt())

## dySEM 1.1.0

- Added some basic dyadic EFA functionality (simulations forthcoming),
  including scripters, a model fitting and comparison sequence wrapper,
  and some unit tests

## dySEM 1.0.0

CRAN release: 2024-03-15

Marks the first release of dySEM on CRAN. From here on out, the NEWS.md
file will be updated with each new release.

### BREAKING CHANGES

- Implemented a new style of arguments for constraints on the
  measurement model (e.g., constr_dy_x_meas) and structural model (e.g.,
  constr_dy_x_struct) for the scripters, in order to make addition of
  arguments for other design-based constraints (e.g., groups, waves of
  assessment) easier in future releases. These arguments replace the
  previous model and equate arguments in the scripters (which have been
  deprecated) and default to the most constrained (i.e.,
  indistinguishable) version of the model being scripted.

### Major Changes

- [@omarjcamanto](https://github.com/omarjcamanto) joins the dySEM team
  as a co-author! Omar has been a major contributor to the dySEM project
  during the Fall 2023 semester, and we are excited to have him on board
  moving forward.

- We now have unit tests! While coverage has taken a hit in our switch
  to the new arguments for constraints, there is a critical mass of
  tests in place for scrapeVarCross(), scriptCFA() and scriptAPIM(), and
  the scriptHelpers underlying them. For scriptCFA() and scriptAPIM(),
  there are specific tests ensuring the appropriate number of estimated
  parameters and dfs, and equivalent model fit for the FF and MV
  scale-setting versions (thanks to
  [@omarjcamanto](https://github.com/omarjcamanto) for his work on
  these). We will continue to add tests for other functions in future
  releases, as well as a vignette explaining how we use number of
  estimated parameters and dfs, and model fit indexes in unit tests for
  quality assurance in dySEM’s model specification.

- We have added two new user-facing wrapper functions:
  getContraintTests() (which should help with more insights into what
  items are driving dyadic noninvariance), and getInvarCompTable()
  (which facilitates a more parsimony-favouring and inclusive “build-up”
  invariance testing sequence, beginning with most constrained model and
  relaxing constraints in a step-wise fashion).

### Minor Changes

- We have made a number of updates to our documentation and vignettes,
  in order to bring these into line with the BREAKING and Major changes
  of this release.

## dySEM 0.0.3

## dySEM 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
