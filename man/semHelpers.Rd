% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semHelpers.R
\name{semHelpers}
\alias{semHelpers}
\alias{lregs}
\alias{cfloads}
\alias{cfvars}
\alias{cfmeans}
\title{Functions for scripting structural parameters}
\usage{
lregs(dvn, param, lvxname, lvyname, type = "free")

cfloads(dvn, lvxname, lvyname, type = "equated")

cfvars(lvname, type)

cfmeans(lvname, type, lvar = "X")
}
\arguments{
\item{dvn}{input dvn list from scrapeVarCross}

\item{param}{input character for what kind of parameter is being scripted ("act", "apim_part", "mim_part", "cf")}

\item{lvxname}{input character to (arbitrarily) name LV X in lavaan syntax}

\item{lvyname}{input character to (arbitrarily) name LV Y in lavaan syntax}

\item{type}{input character to indicate whether parameters "free" or "equated" in estimation}

\item{lvar}{input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn}

\item{partner}{input character to indicate parameters for first or second dyad member}
}
\description{
Functions for scripting structural parameters
}
\seealso{
Other helpers: 
\code{\link{esHelpers}},
\code{\link{outHelpers}},
\code{\link{outParamHelpers}},
\code{\link{scrapeHelpers}},
\code{\link{scriptHelpers}}
}
\concept{helpers}
