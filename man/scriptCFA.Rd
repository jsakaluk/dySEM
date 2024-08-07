% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scriptCFA.R
\name{scriptCFA}
\alias{scriptCFA}
\title{A Function That Writes, Saves, and Exports Syntax for
Fitting Latent Dyadic Confirmatory Factor Analysis (CFA) Models}
\usage{
scriptCFA(
  dvn,
  scaleset = "FF",
  lvname = "X",
  constr_dy_meas = c("loadings", "intercepts", "residuals"),
  constr_dy_struct = c("variances", "means"),
  model = lifecycle::deprecated(),
  writeTo = NULL,
  fileName = NULL
)
}
\arguments{
\item{dvn}{input dvn list from scrapeVarCross}

\item{scaleset}{input character to specify how to set the scale of the latent variable(s). Default is
"FF" (fixed-factor; see Details for rationale), but user can specify "MV" (Marker Variable)}

\item{lvname}{input character to (arbitrarily) name LV in lavaan syntax}

\item{constr_dy_meas}{input character vector detailing which measurement model parameters to constrain across dyad members.
Default is c("loadings", "intercepts", "residuals")(in combination with defaults for constr_dy_struct, an indistinguishable dyadic CFA),
but user can specify any combination of "loadings", "intercepts", and "residuals",
#or "none" to specify an otherwise unconstrained dyadic configural invariance model}

\item{constr_dy_struct}{input character vector detailing which structural model parameters to constrain across dyad members.
Default is c("variances", "means")(in combination with defaults for constr_dy_meas, an indistinguishable dyadic CFA),
but user can specify any combination of "variances" and "means", or "none".}

\item{model}{Depreceated input character used to specify which level of invariance is
modeled ("configural", "loading", "intercept", "residual", or "indist"). Users should rely upon constr_dy_meas and
constr_dy_struct instead, for making constraints to the measurement and/or structural portions of the model.}

\item{writeTo}{A character string specifying a directory path to where a .txt file of the resulting lavaan script should be written.
If set to “.”, the .txt file will be written to the current working directory.
The default is NULL, and examples use a temporary directory created by tempdir().}

\item{fileName}{A character string specifying a desired base name for the .txt output file.
The default is NULL. The specified name will be automatically appended with the .txt file extension.
If a file with the same name already exists in the user's chosen directory, it will be overwritten.}
}
\value{
character object of lavaan script that can be passed immediately to
lavaan functions
}
\description{
This function takes the outputted object from scrapeVarCross()
and automatically writes, returns, and exports (.txt) lavaan() syntax
for specifying dyadic configural, loading, and intercept invariant
measurement models for either a specified X or Y factor.
}
\details{
By default, many dySEM:: functions (including scriptCFA() default to
a fixed-factor method of scale-setting, whereby the latent variance of
a given factor is constrained to 1 for both partners in the configurally invariant
#model, and then one of these variances is freely estimated in subsequent
#models of the invariance testing sequence. We have selected this default
for two reasons: (1) the selection of a marker-variable is usually arbitrary,
yet can have a large influence on the estimation and testing of of structural
parameters (see https://stats.stackexchange.com/questions/402133/in-cfa-does-it-matter-which-factor-loading-is-set-to-1/402732#402732); and (2) the selection of a non-invariant marker-variable
can have disastrous down-stream consequences for the identification of
non-invariant measurement parameters, following a the rejection of an omnibus
#invariance constraint set (see Lee, Preacher, & Little, 2011).
}
\examples{
dvn <- scrapeVarCross(dat = commitmentQ, x_order = "spi", x_stem = "sat.g", x_delim1 = ".",
x_delim2="_", distinguish_1="1", distinguish_2="2")

sat.indist.script <- scriptCFA(dvn, lvname = "Sat")

sat.lvars.script <- scriptCFA(dvn, lvname = "Sat",
constr_dy_meas = "loadings",
constr_dy_struct = "variances")

sat.resids.script <- scriptCFA(dvn, lvname = "Sat",
constr_dy_meas = c("loadings", "intercepts", "residuals"),
constr_dy_struct = "none",
writeTo = tempdir(),
fileName = "dCFA_residual")

sat.ints.script <- scriptCFA(dvn, lvname = "Sat",
constr_dy_meas = c("loadings", "intercepts"),
constr_dy_struct = "none",
writeTo = tempdir(),
fileName = "dCFA_intercept")

sat.loads.script <- scriptCFA(dvn, lvname = "Sat",
constr_dy_meas = c("loadings"),
constr_dy_struct = "none",
writeTo = tempdir(),
fileName = "dCFA_loading")

sat.config.script <- scriptCFA(dvn, lvname = "Sat",
constr_dy_meas = "none",
constr_dy_struct = "none",
writeTo = tempdir(),
fileName = "dCFA_configural")
}
\seealso{
\code{\link{scrapeVarCross}} which this function relies on

Other script-writing functions: 
\code{\link{scriptAPIM}()},
\code{\link{scriptBiDy}()},
\code{\link{scriptCFM}()},
\code{\link{scriptDyEFA}()},
\code{\link{scriptINULL}()},
\code{\link{scriptISAT}()},
\code{\link{scriptMIM}()}
}
\concept{script-writing functions}
