# profile_generation.R
# generates scaled hrdp dataframes (also called profiles) from energy dataframes
# that were previously generated using tcx files (see tcx_loading.R)

#' Convert energy data frame into heart rate profile
#' @description scales final profile into 3 regions. If unable to create profile from energy data
#'      frame nothing is returned and a warning supplied.
#'
#' @param energy_df A dataframe with "energy" format as returned by \code{\link{load_tcx_file}}
#' @param buffer_start The number of seconds before activity included in profile
#' @param buffer_end The number of seconds after activity included in profile
#' @param scale The number of data points in each third of the final profile
#' @param rest_sec The number of seconds of recovery included in initial recovery period
#' @param points A list with start and end points of activities. If NULL then it will
#'     use \code{\link{start_end_points}} to caluculate the start and end points of
#'     the activity.
#' @return Named list of heart rates (bpm) scaled into profile consiting of 3 regions :
#' \itemize{
#' \item resting period before activity
#' \item activity and initial recovery period
#' \item recovery after activity and inital recovery
#' }
#' @examples
#' get_profile(df, 500, 500, 500)
#' get_profile(df, 200, 200, 500, rest_sec = 180)
#' get_profile(df, 100, 200, 500,
#'     points = list(
#'         "start_point" = 600,
#'         "end_point" = 1000
#'     )
#' )
#'
#' @family profile functions
#' @seealso \code{\link{load_tcx_file}} for format of input dataframe,
#'     \code{\link{start_end_points}} for more on start and end points of activities.
#' @export
get_profile <- function(energy_df, buffer_start, buffer_end, scale, rest_sec = 180, points = NULL) {


    # find start and end of activity
    if(is.null(points)) {
        energy_df <- padr::pad(energy_df, interval = "sec")
        energy_df <- subset(energy_df, select = -c(time))
        points <- start_end_points(energy_df)
    }

    # return nothing if the buffer_start too big
    if((points$start_point - buffer_start) < 0) {
        warning("not enough data buffer start too large")
        return()
    }

    # return nothing if the resting period too long
    if((points$end_point + buffer_end) > nrow(energy_df)) {
        warning("not enough data buffer end too large")
        return()
    }

    # find restSec seconds after activity
    post_end <- points$end_point + rest_sec

    # approximate missing data and scale regions of profile
    # part of profile where activity is taking place
    scaled_active <- data.frame("bpm" = approx(energy_df[points$start_point:points$end_point,]$bpm, n=scale/2)$y)

    # resting before activity
    scaled_resting <- data.frame("bpm" = approx(energy_df[(points$start_point - buffer_start):points$start_point,]$bpm, n=scale)$y)

    # restSec seconds after end of activity
    scaled_post_active <- data.frame("bpm" = approx(energy_df[points$end_point:post_end,]$bpm, n=scale/2)$y)

    # remaining recovery period after initial resting
    scaled_after_active <- data.frame("bpm" = approx(energy_df[post_end:(points$end_point + buffer_end),]$bpm, n=scale)$y)

    # bind all parts of profile into one
    profile <- rbind(scaled_resting, scaled_active)
    profile <- rbind(profile, scaled_post_active)
    profile <- rbind(profile, scaled_after_active)

    return(profile$bpm)
}

#' Converts multiple energy dataframes into heart rate profiles
#' @description Scales final profiles into 3 regions. If unable to create profiles from some
#' dataframes the profile is not included and a warning is supplied.
#'
#' @param energy_dfs list of energy data frames such as returned by \code{\link{load_tcx_file}}
#' @param buffer_start The number of seconds before activity included in profile
#' @param buffer_end The number of seconds after activity included in profile
#' @param scale The number of data points in each third of the final profile
#' @param rest_sec The number of seconds in initial resting part of final profile. Must be less
#' than buffer_end
#' @param norm Normalise profile heart rates between 0 and 1
#' @param all_activities Finds profiles for all activities in the energy dataframes or else
#'     finds only the first activity in each dataframe
#' @param time_threshold The minumum number of seconds duration for an activity
#' @param rest_threshold The maximum number of resting seconds within and activity
#' @return List containing profiles
#'      See \code{\link{get_profile}} for a description of profile.
#' @examples
#' get_profiles(df, 500, 500, 500)
#' get_profiles(df, 200, 200, 1000, norm = TRUE)
#' get_profiles(df, 1500, 500, 100, rest_sec = 80, all_activities = FALSE)
#'
#' @family profile functions
#' @seealso \code{\link{load_tcx_dir}} for a description of an energy dataframe.
#' @export
get_profiles <- function(energy_dfs, buffer_start, buffer_end, scale, rest_sec = 180, norm = FALSE, norm_resting = FALSE,
                         all_activities = TRUE, time_threshold = 15, rest_threshold = 5) {

    # rest_sec must be shorter than buffer_end
    if (rest_sec > buffer_end) {
        stop("rest sec length should be included in buffer end length")
    }

    # get profiles for each energy dataframe
    profiles <- lapply(energy_dfs, function(e) {
            # for each energy dataframe find all activities
            activities <- get_activities(e, buffer_start, buffer_end, scale, time_threshold, rest_threshold,
                                         all_activities = all_activities)

            # get profiles for every activity
            profile <- lapply(activities, function(a) {
                points <- list("start_point" = buffer_start + 1, "end_point" = (nrow(a) - buffer_end))
                p <- get_profile(a, buffer_start, buffer_end, scale, rest_sec = rest_sec, points = points)

                if (norm && !is.null(p)) {
                    p <- normalise(p)
                } else if (norm_resting && !is.null(p)) {
                    p <- p - resting_hr(p)
                }
                return(p)
            })
        return(profile)
    })
    return(unlist(profiles, recursive = FALSE))
}

#' Aggregates multiple heart rate profiles
#' @description Finds mean and standard deviation of multiple heart rate profiles
#'
#' @param profiles List of profiles to be aggregated.
#'     See \code{\link{get_profile}} for a description of profile.
#'     The profiles must be of the same length.
#' @return List of means and standard deviations of heart rate at all data points in a profile.
#' @examples
#' profiles <- get_profiles(df, 500, 500, 500)
#' aggregate_profiles(profiles)
#'
#' @family profile functions
#' @seealso \code{\link{get_profiles}}
#' @export
aggregate_profiles <- function(profiles) {
    # transform list of list into matrix
    profiles <- do.call(cbind, profiles)
    # get mean and standard deviation of rows
    means <- rowMeans(profiles)
    std_dev <- apply(profiles, 1, sd)
    return(list("mean"= means, "sd"=std_dev))
}


#' Normalise heart rate in profile
#' @description normalises heart rate between 0 and 1 in heart rate dynamic profile
#'
#' @param profile profile to be nomalised.
#'     See \code{\link{get_profile}} for a description of profile.
#' @return profile that has been normalised.
#'     See \code{\link{get_profile}} for a description of profile.
#'
#' @family profile functions
#' @seealso See \code{\link{get_profile}} for a description of profile.
normalise <- function(profile){
    nProfile <- (profile - min(profile))/(max(profile) - min(profile))
    return(nProfile)
}

