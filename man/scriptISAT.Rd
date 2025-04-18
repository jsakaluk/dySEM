% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scriptISAT.R
\name{scriptISAT}
\alias{scriptISAT}
\title{A Function That Writes, Saves, and Exports Syntax for
Fitting the I-SAT model for indistinguishable dyads}
\usage{
scriptISAT(dvn, lvxname = "X", lvyname = NULL, writeTo = NULL, fileName = NULL)
}
\arguments{
\item{dvn}{input dvn list from scrapeVarCross}

\item{lvxname}{input character to (arbitrarily) name X LV in lavaan syntax}

\item{lvyname}{(optional) input character to (arbitrarily) name X LV in lavaan syntax}

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
for the I-SAT model described in Olsen & Kenny (2006)
}
\examples{
dvn <- scrapeVarCross(dat = DRES, x_order = "sip", x_stem = "PRQC", x_delim1 = "_",
x_delim2=".", x_item_num="\\\\d+", distinguish_1="1", distinguish_2="2")

qual.isat.script <- scriptISAT(dvn, lvxname = "Qual",
writeTo = tempdir(),
fileName = "I-SAT_script")
}
\seealso{
\code{\link{scrapeVarCross}} which this function relies on

Other indistinguishable script-writing functions: 
\code{\link{scriptINULL}()}
}
\concept{indistinguishable script-writing functions}
