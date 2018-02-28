# feature_extraction.R
# extracts features from profiles and converts them into a feature dataframe


#' Find all the features in a profile
#' @description find all the features of a profile
#'
#' @param profiles A list of profiles such as returned by \code{\link{get_profile}} used to find features.
#' @param buffer_end The number seconds after an activity included in the profiles.
#' @param rest_sec The number of seconds of recovery included in initial recovery period.
#' @param degree_coef_active The degree of the polynomial fitted to active period of a profile,
#'     see \code{\link{profile_coefficients}} for details on fitting the polynomial
#'     and \code{\link{get_profile}} for a description of a profile
#' @param degree_coef_init_rec The degree of the polynomial fitted to initial recovery period of a profile
#'     see \code{\link{profile_coefficients}} for details on fitting the polynomial
#'     and \code{\link{get_profile}} for a description of a profile
#' @return Data frame containing features and labels with features from each profile on a new row.
#' @examples
#' profiles <- get_profiles(dfs, 200, 300, 500)
#' get_features(profiles, 300, 500)
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of a profile.
#'
#' \code{\link{profile_coefficients}} for details of fitting the polynomials.
#'
#' @export
get_features <- function(profiles, buffer_end, rest_sec = 180,
                         degree_coef_active = 5, degree_coef_init_rec = 5) {

    # set resting indices on length of time available
    num_rest_ind <- floor(buffer_end/60)
    num_coef_active <- degree_coef_active  + 1
    num_coef_init_rec <- degree_coef_init_rec  + 1
    num_rest_slope = 2

    # get labels for features
    labels = c(
        "Y",
        "maxHR",
        "restingHR",
        "HRChange",
        replicate(num_rest_ind, "resting_indices"),
        replicate(num_coef_active, "coefficients_active"),
        replicate(num_coef_init_rec, "coefficients_init_recov"),
        replicate(num_rest_slope, "slope")
    )
    labels = make.unique(as.character(labels), sep = "_")


    # find features
    dfs <- list()
    for (i in c(1:length(profiles))) {

        label <- names(profiles)[[i]]
        if (is.null(label)) label <- i

        if (is.atomic(profiles[[i]])) prof <- profiles[[i]]
        else prof <- profiles[[i]]$mean

        scale <- length(prof)/3

        # generate features from profile
        resting_inds = c(as.matrix(resting_indeces(prof, num_rest_ind, buffer_end, rest_sec)))
        coeffs_active = c(as.matrix(profile_coefficients(prof, scale, scale*1.5, degree_coef_active)))
        coeffs_init_rec = c(as.matrix(profile_coefficients(prof, scale*1.5, scale*2, degree_coef_init_rec)))
        rest_slope = c(as.matrix(slope(prof)))

        data = c(
            max_hr(prof),
            resting_hr(prof),
            hr_change(prof),
            resting_inds,
            coeffs_active,
            coeffs_init_rec,
            rest_slope
        )
        class(data) <- "numeric"

        df <- data.frame(label, t(data))
        colnames(df) <- labels

        dfs[[label]] <- df
    }

    finaldf <- do.call(rbind, dfs)

    return(finaldf)
}

#' Find maximum heart rate
#' @description find the maximum heart rate in the profile
#'
#' @param profile A profile used to find maximum heart rate.
#'     See \code{\link{get_profile}} for a description of profile.
#' @return The value of the maximum heart rate in the profile
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#' @export
max_hr <- function(profile) {
    return(max(profile))
}

#' Find average resting heart rate
#' @description find the resting heart rate by averaging the resting period before activity
#'
#' @param profile A profile used to find resting heart rate.
#'     See \code{\link{get_profile}} for a description of profile.
#' @return The value of the average resting heart rate
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#' @export
resting_hr <- function(profile) {
    scale <- length(profile)/3
    return(mean(profile[1:(scale-1)]))
}

#' Find the heart rate every minute after cessation of exercise
#' @description find the heart rates after cessation of exercise every minutes
#'
#' @param profile A profile to be nomalised.
#'     See \code{\link{get_profile}} for a description of profile.
#' @param min The number of minutes used to find heart rate recovery indicies.
#' @param buffer_end The number seconds after activity included in the profile.
#' @param rest_sec The number of seconds of recovery included in initial recovery period.
#' @param from_max Start taking the resting indices from maximum heart rate, otherwise start at
#' start of resting period.
#' @return Heart rate at min number of minutes after cessation of exercise
#' @examples
#' profile <- get_profile(df, 500, 500, 500)
#' resting_indices(profile, 8, 500, 500)
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#' @export
resting_indeces <- function(profile, min, buffer_end, rest_sec = 180, from_max = FALSE) {

    scale <- length(profile)/3

    # how many seconds per index in profile
    recovery_scale <- scale/(buffer_end - (rest_sec/60))
    init_rec_scale <- scale/2/(rest_sec)

    # initial
    init_rec <- profile[(scale*1.5):(scale*2)]
    final_rec <- profile[(scale*2):(length(profile))]

    if (from_max) {
        max <- max_hr(profile)

        # recovery heart rates for initial recovery in profile
        init_rec_ind <- unlist(lapply(1:(rest_sec/60), function(i) {
            init_rec[which(init_rec == max) + (init_rec_scale * 60 * i)]
        }))

        # find the amount of time remaining
        index_rec_rem <- (scale/2) - which(init_rec == (init_rec_ind[length(na.omit(init_rec_ind))]))
        extra_time <- 60 - index_rec_rem/init_rec_scale

        # number of recovery minutes still needed
        minutes_remaining <- min - length(na.omit(init_rec_ind))

        # remaining recovery heart rates in profile
        final_rec_ind <- unlist(lapply(0:(minutes_remaining - 1), function(i) {
            final_rec[(extra_time * recovery_scale) + (recovery_scale * 60 * i)]
        }))
    }
    else {
        # recovery heart rates for initial recovery in profile
        init_rec_ind <- unlist(lapply(1:(rest_sec/60), function(i) {
            init_rec[(init_rec_scale * 60 * i)]
        }))

        # if all recovery minutes have been found
        if (length(init_rec_ind) == min) {
            final_rec_ind <- NA
        }
        else {
            # find time remaining after last minute in initial recovery
            index_rec_rem <- (scale/2) - which(init_rec == (init_rec_ind[length(na.omit(init_rec_ind))]))
            extra_time <- 60 - index_rec_rem/init_rec_scale

            # number of recovery minutes still needed
            minutes_remaining <- min - length(na.omit(init_rec_ind))

            # remaining recovery heart rates in profile
            final_rec_ind <- unlist(lapply(0:(minutes_remaining - 1), function(i) {
                final_rec[(extra_time * recovery_scale) + (recovery_scale * 60 * i)]
            }))
        }
    }
    return(c(na.omit(init_rec_ind), na.omit(final_rec_ind)))
}

#' Get coefficients of fitted polynomial
#' @description get the coefficients of polynomial fitted to aspects of a heart rate dynamic profile
#'
#' @param profile A profile that the polynomial is fitted to.
#'     See \code{\link{get_profile}} for a description of profile.
#' @param start Index to start fitting the polynomial.
#' @param end Index to end fitting the polynomial.
#' @param degree Degree of the polynomial.
#' @param model_out Return entire model or else just return the coefficients of the model.
#' @return coefficients of the model fit the region of the profile specified.
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
profile_coefficients <- function(profile, start, end, degree, model_out=FALSE) {
    model <-(lm(profile[start:end] ~ poly(c(1:length(profile[start:end])), degree)))
    if(model_out){return(model)}
    return(model$coefficients)
}

#' Get change between resting heart rate and final heart rate
#' @description gets the difference between the average resting and the final heart rate of a profile.
#'
#' @param profile Profile to find heart rate change.
#'     See \code{\link{get_profile}} for a description of profile.
#' @return Returns a value of the change between resting and final heart rate
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
hr_change <- function(profile) {
    scale <- length(profile)/3
    return(resting_hr(profile) - profile[length(profile)])
}


#' Fit line to recovery slope
#' @description fit regression line to the slope of the final recovery in a profile.
#'
#' @param profile The profile used to find the recovery slope.
#'     See \code{\link{get_profile}} for a description of profile.
#' @param model_out Return entire model or else just return the coefficients
#' @return coefficients of the model fit the final recovery region of the profile
#' @family feature functions
#' @seealso \code{\link{get_profile}} for a description of profile.
slope <- function(profile, model_out=FALSE) {
    scale <- length(profile)/3
    start <- scale * 2
    end <- scale * 3
    line <- lm(profile[start:end] ~ c(1:length(profile[start:end])))
    if(model_out) {return(line)}
    return(line$coefficients)
}
