% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_doc.R
\docType{data}
\name{e_data}
\alias{e_data}
\alias{e_data_src1}
\alias{e_data_src2}
\alias{t_groups}
\alias{s_data}
\alias{tnsc_data}
\title{Data of safety meta-analysis.}
\format{
An object of class \code{data.frame}.

An object of class \code{data.frame} with 4 rows and 5 columns.

An object of class \code{data.frame} with 3 rows and 5 columns.

An object of class \code{list} of length 2.

An object of class \code{data.frame} with 3 rows and 4 columns.

An object of class \code{data.frame} with 4 rows and 8 columns.
}
\source{
None
}
\usage{
data(e_data)

e_data_src1

e_data_src2

t_groups

s_data

tnsc_data
}
\description{
Safety meta-analysis of aggregated datasets rely on 2 major compounds:
\strong{study}-level data, and \strong{event}-level data.
During the data management process, event-level data is grouped according to
safety terms, and becomes \strong{term}-level data.
The study-level data begin with \strong{\code{s_}}.
\itemize{
\item \code{s_data} study-level data, including total number of subjects
in each study arm, and other aggregated features (median age, median follow-up)
}
The event-level data begin with \strong{\code{e_}}.
\itemize{
\item \verb{e_data_src*} event-level dataset from a specific source (src1, src2)
\item \code{e_data}      event-level dataset, where all sources have been reconciled.
}
The term-level data begin with \strong{\code{t_}}.
\itemize{
\item \code{t_groups}    A named list. Names are the terms, contents are the
events composing the term.
\item \code{tnsc_data}   term-level dataset, also joined with some study
characteristics (subgroups).
}
}
\examples{
data(e_data)
e_data[e_data$study_id == "NCT1", ]
}
\references{
There is none
}
\keyword{datasets}
