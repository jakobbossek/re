% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catf.R
\name{catf}
\alias{catf}
\alias{stopf}
\alias{warnf}
\alias{warningf}
\title{Formatted output}
\usage{
catf(fmt, ...)

stopf(fmt, ...)

warnf(fmt, ...)

warningf(fmt, ...)
}
\arguments{
\item{fmt}{[\code{character(1)}]\cr
Format string.}

\item{...}{[any]\cr
Further arguments passed down to \code{sprintf}.}
}
\value{
Invisibly returns the return value of the respective function, e.g.
the character warning message in case of \code{warnf}.
}
\description{
Simple wrappers around \code{cat}, \code{stop}, \code{warning}
and \code{sprintf}.
}
