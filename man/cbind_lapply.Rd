% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbind_lapply.R
\name{cbind_lapply}
\alias{cbind_lapply}
\alias{rbind_lapply}
\title{Bind lapply results}
\usage{
cbind_lapply(X, FUN, ...)

rbind_lapply(X, FUN, ...)
}
\arguments{
\item{X}{[any]\cr
Iterable objects, e.g. list or vector.}

\item{FUN}{[\code{function}]\cr
Function applied to each element of \code{X} (see \code{\link[base]{lapply}}).}

\item{...}{[any]\cr
Furhter parameters passed down to \code{\link[base]{lapply}}.}
}
\value{
See return value of \code{\link[base]{cbind}} and \code{\link[base]{rbind}}
respectively.\cr
}
\description{
Simple shortcut for \code{do.call(cbind, lapply(...))}.
}
