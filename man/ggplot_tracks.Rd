% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracks_plots.R
\name{ggplot_tracks}
\alias{ggplot_tracks}
\title{ggplot wrapper for tracks objects.}
\usage{
ggplot_tracks(tracks, table, ...)
}
\arguments{
\item{tracks}{A tracks object.}

\item{table}{Which table is plotting. Give the name as a character vector,
e.g. 'tr'.}

\item{...}{Arguments to be passed on to \code{ggplot}.}
}
\value{
A ggplot object.
}
\description{
Instead of using one of the more streamlined plotting functions such as
\code{plot_tracks}, this wrapper gives full control over the visualization.
All it does, is select a table from the tracks object, and pull it from the
nodes if necessary.
}
\examples{
if (require(ggplot2)){
  tr <- as_tracks(guppies, 30, 1080)
  ggplot_tracks(tr, 'tr', aes(X, Y, col = animal)) +
    geom_path() + facet_wrap(~trial)
}
}
