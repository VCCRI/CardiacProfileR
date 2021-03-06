% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_features_profile}
\alias{plot_features_profile}
\title{Plots all the features of a profile}
\usage{
plot_features_profile(profile, buffer_end, p = NULL, label = "Features",
  join_feat = FALSE, col = get_color(0), annotate_max_hr = TRUE)
}
\arguments{
\item{profile}{A profile to get features from
See \code{\link{get_profile}} for a description of profile.}

\item{buffer_end}{The number seconds before activity included in profile.}

\item{p}{Plot to be added to.}

\item{label}{of the profile being plotted}

\item{join_feat}{plots all the features as one color and legend group, useful for plotting
features of multiple profiles}

\item{col}{Color of the features when join_feat is true}

\item{annotate_max_hr}{Add an annotation to the maximum heart rate}
}
\value{
plot with the features of a profile
}
\description{
plots:
\itemize{
\item resting heart rate
\item active heart rate
\item initial heart rate recovery
\item final heart rate recovery
\item heart rate recovery every minute post cessation of exercise
}
}
\examples{
   profile1 <- get_profile(df1, 300, 200, 500)
   plot_features(profile1, 200)

   profile2 <- get_profile(df2, 300, 200, 500)
   plot_features(profile2, 200, label = "The best features")

   p <- plot_features(profile1, 200, join_feat = TRUE)
   plot_features(profile2, 200, p = p, join_feat = TRUE, col = "orange")

}
\seealso{
\code{\link{get_profile}} for a description of profile.

Other plotting functions: \code{\link{add_sections}},
  \code{\link{get_color}},
  \code{\link{get_energy_profiles}},
  \code{\link{min_max_plot}},
  \code{\link{plot_3d_landscape}},
  \code{\link{plot_3d_profiles}},
  \code{\link{plot_active}},
  \code{\link{plot_features_profiles}},
  \code{\link{plot_profiles}}, \code{\link{plot_profile}}
}
