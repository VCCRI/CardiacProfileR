% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activity_extraction.R
\name{get_activities}
\alias{get_activities}
\title{Extract active parts of dataframe}
\usage{
get_activities(energy_df, buffer_start, buffer_end, scale,
  time_threshold = 15, rest_threshold = 5, all_activities = TRUE)
}
\arguments{
\item{energy_df}{An energy data frame as returned by \code{\link{load_tcx_file}}.}

\item{buffer_start}{The number seconds before activity included in profile.}

\item{buffer_end}{The number seconds after activity included in profile.}

\item{scale}{The number of data points in each third of the final profile.}

\item{time_threshold}{The minumum number of seconds duration for an activity.}

\item{rest_threshold}{The maximum number of resting seconds within an activity.}

\item{all_activities}{Find all the activities in a energy data frame, otherwise find only the first one.}
}
\value{
List of energy type data frames containing the activity, a buffer at the start and a buffer at the end
    See \code{\link{load_tcx_dir}} for a description of an energy dataframe.
}
\description{
extracts parts of an energy dataframe that include an activity and resting at the start and end of activity
}
\examples{
get_activities(df, 500, 500, 500)
get_activities(df, 200, 200, 1000, time_threshold = 60)
get_activities(df, 1500, 500, 100, rest_threshold = 10)

}
\seealso{
\code{\link{load_tcx_dir}} for a description of an energy dataframe.
    \code{\link{all_start_end_points}} for a method to get the start and end points of all activities.

Other get activity functions: \code{\link{all_start_end_points}},
  \code{\link{start_end_points}}
}
