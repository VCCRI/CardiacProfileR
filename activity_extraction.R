# activity_extraction.R
# takes in energy data frames (see tcx_loading.R)
# finds activities within those energy data frames

#' Finds the activity in an energy dataframe
#' @description finds the first start and end points of activity in energy dataframe.
#'
#' @param energy_df An energy data frame as returned by \code{\link{load_tcx_file}}.
#' @param time_threshold The minimum number of seconds duration of an activity.
#' @param rest_threshold The maximum number of resting seconds within and activity.
#' @return named list with \code{start_point} and \code{end_point} for the activity.
#' @examples
#' start_end_points(df)
#' start_end_points(df, time_threshold = 30, rest_threshold = 2)
#' start_end_points(df, rest_threshold = 2)
#' @family get activity functions
#' @seealso \code{\link{load_tcx_file}} for format of input dataframe
start_end_points <- function(energy_df, time_threshold = 15, rest_threshold = 5) {

    # find when energy is being expended
    cc <- which(energy_df$totalE > 0)

    # if there are no activites
    if(length(cc) == 0) {
        return(list(start_point=NA, end_point=NA))
    }

    # identify start and end of activity
    start_point <- min(cc)
    potential_end_points <- cc[which(diff(cc)>rest_threshold)]
    end_point <- potential_end_points[1]
    # if there is only one activity take the last index where energy is being expended
    if (is.na(end_point)) {
        end_point <- max(cc)
    }

    #  avoid short periods of energy expenditure
    if ((end_point - start_point) < time_threshold) {
        energy_df[start_point:end_point,]$totalE <- NA
        points <- start_end_points(energy_df, time_threshold, rest_threshold)
        start_point = points$start_point
        end_point = points$end_point
    }

    return(list(start_point = start_point, end_point = end_point))
}


#' Find start and end points of all activities
#' @description find index for start and end of all active periods in an energy data frame
#'
#' @param energy_df An energy dataframe as returned by \code{\link{load_tcx_file}}.
#' @param time_threshold The minumum number of seconds duration for an activity.
#' @param rest_threshold The maximum number of resting seconds within and activity.
#' @return List containing :
#' \itemize{
#' \item data frame of all the starting and ending indeces of the activities.
#' \item energy data frame where missing data points have been interpolated.
#' }
#' @family get activity functions
#' @seealso \code{\link{load_tcx_file}} for format of input dataframe
#' @export
all_start_end_points <- function(energy_df, time_threshold = 15, rest_threshold = 5) {
    # add NA for missing data
    padded_df <- padr::pad(energy_df, interval = "sec")

    # interpolate missing data
    interpolated_df <- data.frame("totalE" = approx(padded_df$totalE, n = nrow(padded_df))$y,
                                  "deltaE" = approx(padded_df$deltaE, n = nrow(padded_df))$y,
                                  "bpm" = approx(padded_df$bpm, n = nrow(padded_df))$y,
                                  "deltaBPM" = approx(padded_df$deltaBPM, n = nrow(padded_df))$y,
                                  "rawbpm" = approx(padded_df$rawbpm, n = nrow(padded_df))$y,
                                  "time" = padded_df$time)

    # get subset that does not have time
    df <- subset(interpolated_df, select = -c(time))

    points <- list()
    new_points <- list()
    while(!anyNA(new_points)) {
        new_points <- start_end_points(df, time_threshold, rest_threshold)
        if(!anyNA(new_points)) {
            df[new_points$start_point:new_points$end_point,] <- NA
            points <- rbind(points, new_points)
        }
    }
    return(list("points" = points, "df" = interpolated_df))
}


#' Extract active parts of dataframe
#' @description extracts parts of an energy dataframe that include an activity and resting at the start and end of activity
#'
#' @param energy_df An energy data frame as returned by \code{\link{load_tcx_file}}.
#' @param buffer_start The number seconds before activity included in profile.
#' @param buffer_end The number seconds after activity included in profile.
#' @param scale The number of data points in each third of the final profile.
#' @param time_threshold The minumum number of seconds duration for an activity.
#' @param rest_threshold The maximum number of resting seconds within an activity.
#' @param all_activities Find all the activities in a energy data frame, otherwise find only the first one.
#' @return List of energy type data frames containing the activity, a buffer at the start and a buffer at the end
#'     See \code{\link{load_tcx_dir}} for a description of an energy dataframe.
#' @examples
#' get_activities(df, 500, 500, 500)
#' get_activities(df, 200, 200, 1000, time_threshold = 60)
#' get_activities(df, 1500, 500, 100, rest_threshold = 10)
#'
#' @family get activity functions
#' @seealso \code{\link{load_tcx_dir}} for a description of an energy dataframe.
#'     \code{\link{all_start_end_points}} for a method to get the start and end points of all activities.
#' @export
get_activities <- function(energy_df, buffer_start, buffer_end, scale, time_threshold = 15, rest_threshold = 5,
                           all_activities = TRUE) {

    results <- all_start_end_points(energy_df, time_threshold, rest_threshold)

    df <- results$df
    all_points <- results$points

    activities <- list()

    if(all_activities) range_points <- c(1:nrow(all_points))
    else range_points <- 1

    j <- 1
    for(i in range_points) {
        points <- all_points[i,]
        if (points$start_point > buffer_start && (points$end_point + buffer_end) < nrow(df)) {
            activities[[j]] <- df[(points$start_point - buffer_start):(points$end_point + buffer_end),]
            j <- j + 1
        }
        else {
            warning("activity not included buffers too large")
        }
    }

    return(activities)
}
