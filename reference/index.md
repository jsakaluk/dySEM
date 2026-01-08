# Package index

## Data

### Data sets of dyadic data for use with *dySEM*

These data sets come from a variety of sources, and are used to
facilitate the exemplar deployment of *dySEM* functionality; they may be
useful to you for learning, teaching, and/or development.

- [`commitmentM`](https://jsakaluk.github.io/dySEM/reference/commitmentM.md)
  : Ratings of relational satisfaction and commitment from 282
  (M)ixed-sex couples
- [`commitmentQ`](https://jsakaluk.github.io/dySEM/reference/commitmentQ.md)
  : Ratings of relational satisfaction and commitment from 282 (Q)ueer
  couples
- [`DRES`](https://jsakaluk.github.io/dySEM/reference/DRES.md) :
  Relationship quality and sexual satisfaction of 121 couples
- [`imsM`](https://jsakaluk.github.io/dySEM/reference/imsM.md) : Ratings
  on the full Investment Model Scale (IMS) from 282 (M)ixed-sex couples
- [`pnrqM`](https://jsakaluk.github.io/dySEM/reference/pnrqM.md) :
  Ratings on items from the Positive-Negative Relationship Quality Scale
  (PNRQ; Rogge et al., 2017) from 219 (M)ixed-sex couples
- [`prqcQ`](https://jsakaluk.github.io/dySEM/reference/prqcQ.md) :
  Ratings on items from the Perceived Relationship Quality Components
  (PRQC) Inventory from 118 (Q)ueer couples

## Scrapers

### Scrape variable information from a dyadic data set

These functions are used to extract variable names and other information
from dyadic data sets, to facilitate the specification of *dySEM*
models.

- [`scrapeVarCross()`](https://jsakaluk.github.io/dySEM/reference/scrapeVarCross.md)
  : A Variable Name-Scraping and Indexing Function for cross-sectional
  data

## Scripters

### Uni-construct Scripters

These functions are used to generate lavaan syntax corresponding to
models representing one construct within a dyadic data set

- [`scriptUni()`](https://jsakaluk.github.io/dySEM/reference/scriptUni.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Unidimensional Dyadic Factor Models
- [`scriptCor()`](https://jsakaluk.github.io/dySEM/reference/scriptCor.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Correlated Dyadic Factor Models
- [`scriptHier()`](https://jsakaluk.github.io/dySEM/reference/scriptHier.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Hierarchical Dyadic Factor Models
- [`scriptBifac()`](https://jsakaluk.github.io/dySEM/reference/scriptBifac.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Bifactor Dyadic Models

### Bi-construct Scripters

These functions are used to generate lavaan syntax corresponding to
models representing two constructs within a dyadic data set

- [`scriptAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptAPIM.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting Latent
  Actor-Partner Interdependence Models (APIMs)
- [`scriptObsAPIM()`](https://jsakaluk.github.io/dySEM/reference/scriptObsAPIM.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Observed Actor-Partner Interdependence Models
- [`scriptCFM()`](https://jsakaluk.github.io/dySEM/reference/scriptCFM.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting Latent
  Common Fate Models (CFMs)
- [`scriptMIM()`](https://jsakaluk.github.io/dySEM/reference/scriptMIM.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting Latent
  Mutual influence Model
- [`scriptBiDy()`](https://jsakaluk.github.io/dySEM/reference/scriptBiDy.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting
  Bifactor Dyadic (BiDy) models

### Multi-construct Scripters

These functions are used to generate lavaan syntax corresponding to
models representing more than two constructs within a dyadic data set

- [`scriptCFA()`](https://jsakaluk.github.io/dySEM/reference/scriptCFA.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting Latent
  Dyadic Confirmatory Factor Analysis (CFA) Models with Multiple Factors
- [`scriptDyEFA()`](https://jsakaluk.github.io/dySEM/reference/scriptDyEFA.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting Dyadic
  Exploratory Factor Analysis (DEFA) Models

### Specialized Indistinguishability-Related Scripters

These functions are used to generate lavaan syntax corresponding to
models that are used to make corrections to model fit indexes when
fitting indistinguishable models

- [`scriptISAT()`](https://jsakaluk.github.io/dySEM/reference/scriptISAT.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting the
  I-SAT model for indistinguishable dyads
- [`scriptINULL()`](https://jsakaluk.github.io/dySEM/reference/scriptINULL.md)
  : A Function That Writes, Saves, and Exports Syntax for Fitting the
  I-NULL model for indistinguishable dyads

## Outputters and Calculators

### Outputters

These functions are used to extract and present information from fitted
lavaan models in tabular or path diagram form

- [`outputParamTab()`](https://jsakaluk.github.io/dySEM/reference/outputParamTab.md)
  : A Function That Exports Tables based on dySEM models
- [`outputParamFig()`](https://jsakaluk.github.io/dySEM/reference/outputParamFig.md)
  : A function That Exports SEM diagrams based on dySEM models
- [`outputInvarCompTab()`](https://jsakaluk.github.io/dySEM/reference/outputInvarCompTab.md)
  : Compare Model Fit of Nested Dyadic Invariance Models
- [`outputConstraintTab()`](https://jsakaluk.github.io/dySEM/reference/outputConstraintTab.md)
  : Evaluate Invariance Equality Constraints in a specified Dyadic
  Invariance Model
- [`outputUniConstructComp()`](https://jsakaluk.github.io/dySEM/reference/outputUniConstructComp.md)
  : A Function That Fits and Compares Competing Dyadic Uni-construct
  Models

### Calculators

These functions are used to calculate various auxiliary metrics relevant
to dyadic SEM

- [`getDyReliability()`](https://jsakaluk.github.io/dySEM/reference/getDyReliability.md)
  : A Function Calculates Omega Total Coefficients from a Dyadic CFA
- [`getDydmacs()`](https://jsakaluk.github.io/dySEM/reference/getDydmacs.md)
  : Calculates dmacs difference in expected indicator scores for between
  dyad members
- [`getIndistFit()`](https://jsakaluk.github.io/dySEM/reference/getIndistFit.md)
  : A Function that Computes Corrected Fit Indexes According to the ISAT
  and INULL Models of Olsen & Kenny (2006)

### Deprecated functions

These functions are deprecated and will be removed in future versions of
*dySEM*

- [`getConstraintTests()`](https://jsakaluk.github.io/dySEM/reference/getConstraintTests.md)
  : A function that performs a score test for relaxing each invariance
  equality constraint between partners in a given dyadic SEM model.
- [`getInvarCompTable()`](https://jsakaluk.github.io/dySEM/reference/getInvarCompTable.md)
  : Compare model fit of nested dyadic invariance models in order from
  most parsimonious (residual) to least parsimonious (configural)
- [`outputModel()`](https://jsakaluk.github.io/dySEM/reference/outputModel.md)
  : A Function That Exports Tables and/or SEM Diagrams based on dySEM
  models
