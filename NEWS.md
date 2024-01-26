# dySEM 1.0.0

# dySEM 1.0

Marks the first release of dySEM on CRAN. From here on out, the NEWS.md file will be updated with each new release.

## BREAKING CHANGES

* Implemented a new style of arguments for constraints on the measurement model (e.g., constr_dy_x_meas) and structural model (e.g., constr_dy_x_struct) for the scripters, in order to make addition of arguments for other design-based constraints (e.g., groups, waves of assessment) easier in future releases. These arguments replace the previous model and equate arguments in the scripters (which have been deprecated) and default to the most constrained (i.e., indistinguishable) version of the model being scripted.

## Major Changes

* @omarjcamanto joins the dySEM team as a co-author! Omar has been a major contributor to the dySEM project during the Fall 2023 semester, and we are excited to have him on board moving forward.

* We now have unit tests! While coverage has taken a hit in our switch to the new arguments for constraints, there is a critical mass of tests in place for scrapeVarCross(), scriptCFA() and scriptAPIM(), and the scriptHelpers underlying them. For scriptCFA() and scriptAPIM(), there are specific tests ensuring the appropriate number of estimated parameters and dfs, and equivalent model fit for the FF and MV scale-setting versions (thanks to @omarjcamanto for his work on these). We will continue to add tests for other functions in future releases, as well as a vignette explaining how we use number of estimated parameters and dfs, and model fit indexes in unit tests for quality assurance in dySEM's model specification.

* We have added two new user-facing wrapper functions: getContraintTests() (which should help with more insights into what items are driving dyadic noninvariance), and getInvarCompTable() (which facilitates a more parsimony-favouring and inclusive "build-up" invariance testing sequence, beginning with most constrained model and relaxing constraints in a step-wise fashion).

## Minor Changes

* We have made a number of updates to our documentation and vignettes, in order to bring these into line with the BREAKING and Major changes of this release.

# dySEM 0.0.3

# dySEM 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
