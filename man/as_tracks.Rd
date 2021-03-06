% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracks_object.R
\name{as_tracks}
\alias{as_tracks}
\alias{tracks}
\alias{is.tracks}
\title{The \code{tracks} object}
\usage{
as_tracks(tr, frame_rate, resolution, meta_data = NULL, px_per_cm = 1,
  minimal = TRUE)

is.tracks(x)
}
\arguments{
\item{tr}{Data.frame with tracking data (from read in functions).}

\item{frame_rate}{Frames per second.}

\item{resolution}{Provide standard name for resolution (e.g. "720") or actual
resolution in pixels (e.g. "1920x1080"). Stored for later use, also
used as default tracking window (boundary).}

\item{meta_data}{Optionally supply a data.frame with meta data about the
trials, such as treatments. Can include a column named \code{trial} that
needs to match the trial names listed in the \code{trial} table. Otherwise
this column is created, with identical ordering as the \code{trial} table
assumed.}

\item{px_per_cm}{Length scaling factor. When left at 1, all measurements will
be per pixel.}

\item{minimal}{When TRUE, the minimum amount of components (see list below)
are including in the object. When FALSE, the group, pairs and trial
components will already be generated.}

\item{x}{An object.}
}
\value{
A tracks object.
}
\description{
The \code{tracks} object
}
\section{Functions}{
\itemize{
\item \code{is.tracks}: Test if object is of class tracks.
}}

\section{Object structure}{


The tracks object is a \code{list} with the following tables:
\describe{
  \item{\code{tr}}{\code{party_df} organized by trial, animal, frame.
  Useful for storing individual variables such as location, speed,
  orientation, distance to object etc.}
  \item{\code{soc}}{\code{party_df} organized by trial, pair of animals,
  frame. Useful for storing social variables, such as distance, relative
  orientation etc.}
  \item{\code{group}}{\code{tbl_df} organized by trial, frame. Useful for
  storing group level variables, such as polarization, centrality, group
  size, mean neighbour distance etc.}
  \item{\code{location}}{\code{tbl_df} organized by trial, xbin, ybin.
  Useful for aggregate data for spatial heatmaps etc.}
  \item{\code{meta_data}}{\code{tbl_df} organized by trial. Contains
  information about the trial, such as group origin, treatment, time of
  testing etc.}
  \item{\code{animal}}{\code{tbl_df} organized by trial and animal,
  containing animal level measurements (nested in trial), such as average
  speed.}
  \item{\code{pair}}{\code{tbl_df} organized by trial and pair, containing
  pair level measurements (nested in trial), such as average distance.}
  \item{\code{trial}}{\code{tbl_df} organized by trial, containing trial
  level measurements, such as average speed.}
  \item{\code{params}}{A \code{list} with general experiment parameters, such
  as frame_rate, scale, resolution etc.}
}
 \code{tracks}, \code{meta_data}, and \code{params} are always present,
 others may not be. This is to preserve memory space when they are not
 needed. Order is not garantueed, refer by name if you need to access them.

 Methods for common generics may be available.
}

\seealso{
\code{\link{read_idTracker}}, \code{\link{read_Ctrax}}
}
