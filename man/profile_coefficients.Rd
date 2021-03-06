% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_extraction.R
\name{profile_coefficients}
\alias{profile_coefficients}
\title{Get coefficients of fitted polynomial}
\usage{
profile_coefficients(profile, start, end, degree, model_out = FALSE)
}
\arguments{
\item{profile}{A profile that the polynomial is fitted to.
See \code{\link{get_profile}} for a description of profile.}

\item{start}{Index to start fitting the polynomial.}

\item{end}{Index to end fitting the polynomial.}

\item{degree}{Degree of the polynomial.}

\item{model_out}{Return entire model or else just return the coefficients of the model.}
}
\value{
coefficients of the model fit the region of the profile specified.
}
\description{
get the coefficients of polynomial fitted to aspects of a heart rate dynamic profile
}
\seealso{
\code{\link{get_profile}} for a description of profile.

Other feature functions: \code{\link{get_features}},
  \code{\link{hr_change}}, \code{\link{max_hr}},
  \code{\link{resting_hr}}, \code{\link{resting_indeces}},
  \code{\link{slope}}
}
