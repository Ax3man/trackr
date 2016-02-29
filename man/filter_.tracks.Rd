% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr_verbs.R
\name{filter}
\alias{filter}
\alias{filter_}
\alias{filter_.tracks}
\title{Return a subset of a tracks object with matching conditions.}
\usage{
\method{filter_}{tracks}(.data, ..., drop = FALSE, .dots)
}
\arguments{
\item{.data}{A tracks object.}

\item{...}{The conditions. Do not use \code{&} to combine different
varialbles. Do use \code{&} to combine different conditions on the same
variable.}

\item{drop}{Whether to drop conflicting aggregate data. If the tracks object
contains data that was aggregated over a variable that is now being used to
filter, drop must be TRUE or an error will be raised. Have to opt in, since
aggregation may have been expensive.}

\item{.dots}{Used to work around non-standard evaluation. See
\code{vignette("nse")} for details.}
}
\value{
The subsetted tracks object
}
\description{
Method for the dplyr verb filter. You can use them as \code{filter()} and
\code{filter_()}, as \code{drop} will be passed on accordingly. Allows to
  filter a tracks object on the following variables:
\itemize{
  \item Frame number
  \item Trial
  \item Animal
  \item Animal pair (not implemented)
  \item xbin and ybin for spatial data (not implemented)
}
}
\section{Policy on conflicting dependecies}{


  It will apply the fitering to all the applicable sections of the tracks
  object. It will also attempt to check for any dependency problems. This is
  crucial in order to maintain internal consistency in the tracks object. For
  example, if you select a sequence of frames, any data that was aggregated
  over time will now no longer match. If conflicts are found, by default, an
  error will be raised. By setting \code{drop = TRUE}, you can allow for any
  conflicting data to be deleted from the tracks object.
}
\seealso{
\link[dplyr]{filter}
}
