% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_functions.R
\name{read_Ctrax}
\alias{read_Ctrax}
\title{Read Ctrax data}
\usage{
read_Ctrax(file = NULL, folder = NULL, type = "fixed",
  file_type = ifelse(is.null(file), "mat", tools::file_ext(file)),
  recursive = TRUE, animals = NULL)
}
\arguments{
\item{file}{Either directly supply a path to a file, a list/vector of paths,}

\item{folder}{or supply a path to a folder.}

\item{type}{If folder is supplied, use type to indicate which track type
should be read. When 'fixed', will try to find files with a 'fixed_'
suffix, when 'raw' those with the suffix will be excluded.}

\item{file_type}{Whether to use mat files or csv files. Will guess from the
extension if file is used, defaults to 'mat' if folder is used.}

\item{recursive}{If TRUE, will also try to find files in the subfolders of
folder and combine them into one long data.frame. Only use when using folder.}

\item{animals}{Optional vector of names (ordered in the same order as the
tracking id's). Otherwise file names are used instead.}
}
\value{
A long \code{data.frame} (with class \code{tbl_df} and source
\code{idTracker}).
}
\description{
Loads a csv or mat file containing tracking data from Ctrax into R.
}
\details{
This function always filters out (0, 0) coordinates, as they are a Ctrax bug.
}
\seealso{
Other trackr read functions: \code{\link{read_idTracker}}
}
\concept{trackr read functions}
