# A Function That Writes, Saves, and Exports Syntax for Fitting Observed Actor-Partner Interdependence Models

A Function That Writes, Saves, and Exports Syntax for Fitting Observed
Actor-Partner Interdependence Models

## Usage

``` r
scriptObsAPIM(
  X1 = NULL,
  Y1 = NULL,
  X2 = NULL,
  Y2 = NULL,
  equate = "none",
  k = FALSE,
  writeTo = NULL,
  fileName = NULL
)
```

## Arguments

- X1:

  character of vector name containing X variable/composite for partner 1

- Y1:

  character of vector name containing Y variable/composite for partner 1

- X2:

  character of vector name containing X variable/composite for partner 2

- Y2:

  character of vector name containing Y variable/composite for partner 2

- equate:

  character of what parameter(s) to constrain ("actor", "partner",
  "all"); default is "none" (all freely estimated)

- k:

  input logical for whether Kenny & Ledermann's (2010) k parameter
  should be calculated to characterize the dyadic pattern in the APIM.
  Default to FALSE

- writeTo:

  A character string specifying a directory path to where a .txt file of
  the resulting lavaan script should be written. If set to “.”, the .txt
  file will be written to the current working directory. The default is
  NULL, and examples use a temporary directory created by tempdir().

- fileName:

  A character string specifying a desired base name for the .txt output
  file. The default is NULL. The specified name will be automatically
  appended with the .txt file extension. If a file with the same name
  already exists in the user's chosen directory, it will be overwritten.

## Value

character object of lavaan script that can be passed immediately to
lavaan functions.

## Examples

``` r
obsAPIMScript <- scriptObsAPIM (X1 = "SexSatA", Y1 = "RelSatA",
X2 = "SexSatB", Y2 = "RelSatB",
equate = "none",
writeTo = tempdir(),
fileName = "obsAPIM_script")
```
