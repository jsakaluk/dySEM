% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outHelpers.R
\name{outHelpers}
\alias{outHelpers}
\alias{makeTable}
\alias{makeFigure}
\title{Helpers for creating output files}
\usage{
makeTable(dvn, fit, model, tabletype)

makeFigure(fit, type)
}
\arguments{
\item{dvn}{input dvn list from scrapeVarCross}

\item{type}{input character to indicate whether parameters "free" or "equated" in estimation}

\item{param}{input character for what kind of parameter is being scripted ("act", "apim_part", "mim_part", "cf")}

\item{lvar}{input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn}

\item{lvxname}{input character to (arbitrarily) name LV X in lavaan syntax}

\item{lvyname}{input character to (arbitrarily) name LV Y in lavaan syntax}

\item{partner}{input character to indicate parameters for first or second dyad member}
}
\description{
Helpers for creating output files
}
\seealso{
Other helpers: 
\code{\link{esHelpers}},
\code{\link{outParamHelpers}},
\code{\link{scrapeHelpers}},
\code{\link{scriptHelpers}},
\code{\link{semHelpers}}
}
\concept{helpers}
