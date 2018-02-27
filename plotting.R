# plotting.R
# functions that can plot profiles

####################
# helper functions #
####################


#' Given an index returns a plotly color
#' @description returns a plotly color to be used in plotting functions
#'
#' @param i Index of color
#' @return Color to be used in plotting function
#' @family plotting functions
get_color <- function(i) {
    colors <- list(
        '#1f77b4',  # muted blue
        '#ff7f0e',  # safety orange
        '#2ca02c',  # cooked asparagus green
        '#d62728',  # brick red
        '#9467bd',  # muted purple
        '#8c564b',  # chestnut brown
        '#e377c2',  # raspberry yogurt pink
        '#7f7f7f',  # middle gray
        '#bcbd22',  # curry yellow-green
        '#17becf'   # blue-teal
    )
    nc = length(colors) # number of colors

    return(colors[[i %% nc + 1]])
}

#' Get ymin ymax from plot
#' @description returns the minimum and maximum y values from a plotly function
#'
#' @param p Plotly plot
#' @return list with:
#' \itemize{
#' \item min : minimum y value
#' \item max : maximum y value
#' }
#' @family plotting functions
min_max_plot <- function(p) {
    yvals <- unlist(lapply(p$x$attrs, function(x) {
        if ("y" %in% names(x)) return(x$y)
        if ("ymin" %in% names(x)) return(c(x$ymin, x$ymax))
    }))
    yvals <- yvals[!is.na(yvals)]
    bounds <- list("min" = floor(min(yvals))-1, "max" = ceiling(max(yvals))+1)
    return(bounds)
}


#####################
# plotting function #
#####################

#' Plot a single profile
#' @description plot single profiles individually or add to existing plot
#'
#' @param profile Profile or aggregate profile to be plotted.
#'     See \code{\link{get_profile}} for a description of profile.
#'     See \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @param label Label of the profile being plotted
#' @param title Title of the plot
#' @param color Color of the profile added to the plot.
#' @param p Plot to be added to, if none provided a new plot will be created.
#' @param sections Style used to plot sections of the profile.
#'    See \code{\link{get_profile}} for description of profile sections.
#'
#'    The style options are:
#'    \itemize{
#'    \item "shade" : shade the background of the different sections different colors
#'    \item "lines" : add lines to separate the different sections
#'    }
#' @param sec_opacity The opacity of the sections of the profile added to the plot.
#' @param ribbon_opacity The opacity of standard deviation ribbon when plotting aggregate profile.
#'    See \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @return Plotly object.
#' @examples
#'    profile1 <- get_profile(df, 500, 500, 500)
#'    p <- plot_profile(profile1, "profile 1", title = "Great Profiles")
#'    profile2 <- get_profile(df, 200, 300, 500)
#'    plot_profile(profile2, "profile 2", p = p)
#'
#'    profiles <- get_profiles(dfs, 500, 500, 500)
#'    agg_profile <- aggregate_profiles(profiles)
#'    plot_profile(agg_profile, "aggregated profile", ribbon_opacity = 0.2)
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#'     \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @export
plot_profile <- function(profile, label, title = NULL, color = get_color(0),
                         p = plot_ly(), sections = "shade",
                         sec_opacity = 0.3, ribbon_opactiy = 0.4) {

    # check if it's an aggregate profile or not
    if (Reduce("&", c("mean", "sd") %in% names(profile))) {
        is_aggregate = TRUE
    } else {
        is_aggregate = FALSE
    }

    # plot data
    profile <- data.frame(profile)
    xval = c(1:nrow(profile))
    if (is_aggregate) {
        # plot mean line
        p <- add_lines(p, x = xval, y = profile$mean,
                       text = paste('sd: ', profile$sd),
                       line = list(color = color),
                       legendgroup = label, name = label)
        # add sd ribbon
        p <- add_ribbons(p, x = xval,
                         ymin = profile$mean - profile$sd,
                         ymax = profile$mean + profile$sd,
                         line = list(color = color),
                         opacity = ribbon_opactiy, hoverinfo = "none",
                         legendgroup = label, name = label,
                         inherit = TRUE, showlegend = FALSE)
    } else {
        # plot individual profile
        p <- add_lines(p, x = xval, y = profile$profile, name = label)
    }

    p <- layout(p,
                xaxis = list(title = "Sections", showticklabels = FALSE),
                yaxis = list(title = "BPM"),
                title = title)

    # determine type of sectioning
    sections <- if (is.null(sections)) 'none' else sections
    section_option <- list("lines" = FALSE, "shade" = TRUE)[[sections]]

    # add sections (either as lines or shading)
    if (!is.null(section_option)) {
        p <- add_sections(p, shade = section_option, opacity = sec_opacity, scale = nrow(profile)/3)
    }

    return(p)
}

#' Plot a many individual profiles
#' @description plot many profiles or aggregate profiles on one plot
#'
#' @param profiles A list of profiles or aggregate profiles to be plotted.
#'     See \code{\link{get_profile}} for a description of profile.
#'     See \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @param labels A list of labels of the profiles being plotted.
#' @param title Title of the plot
#' @param p Plot to be added to, if none provided a new plot will be created.
#' @param sections Style used to show sections of the profile.
#'    See \code{\link{get_profile}} for description of profile sections.
#'
#'    The style options are:
#'    \itemize{
#'    \item "shade" : shade the background of the different sections different colors
#'    \item "lines" : add lines to separate the different sections
#'    }
#' @param sec_opacity The opacity of the sections of the profile added to the plot.
#' @return Plotly object.
#' @examples
#'    # Plot two profiles
#'    profile1 <- get_profile(df, 500, 500, 500)
#'    profile2 <- get_profile(df, 200, 300, 500)
#'    plot_profiles(list(profile1, profile2), list("profile 1" "profile 2"))
#'
#'    # Plot many profiles
#'    profiles <- get_profiles(df, 200, 300, 500)
#'    plot_profiles(profiles, names(profiles), sections = "shade")
#'
#'    # Plot aggregate profiles
#'    profiles1 <- get_profiles(dfs, 500, 500, 500)
#'    profiles2 <- get_profiles(df, 200, 300, 500)
#'    agg_profile1 <- aggregate_profiles(profiles1)
#'    agg_profile2 <- aggregate_profiles(profiles2)
#'    plot_profiles(list(agg_profile1, agg_profile2), list("aggregated profile 1",
#'                 "aggregated profile 2"), title = "Aggregate profiles")
#'
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of a profile.
#'     \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @export
plot_profiles <- function(profiles, labels, title = NULL,
                          p = plot_ly(), sections = "shade", sec_opacity = 0.3) {
    profiles <- profiles[!is.null(profiles)]
    last <- length(profiles)

    for (i in c(1:(last-1))) {
        p <- plot_profile(profiles[[i]], labels[[i]],
                          title = title, color = get_color(i-1), p = p,
                          sections = NULL)
    }

    # plot last profile with shading
    p <- plot_profile(profiles[[last]], labels[[last]],
                      title = title, color = get_color(last-1), p = p,
                      sections = sections, sec_opacity = sec_opacity)

    return(p)
}

#' Add section lines and/or shading
#' @description add shading or lines to show the sections of a profile for a plot
#'
#' @param p Plot to add shading to
#' @param scale The number of data points in each third of the profile.
#'    See \code{\link{get_profile}} for a description of profile and scale.
#' @param shade Add shading of sections or else add lines separating sections
#' @param ymin Minimum value of the y axis
#' @param ymax Maximum value of the y axis
#' @param opacity The opacity of the shading
#' @return Ploltly object that has section shading or lines
#' @examples
#'    profile <- get_profile(df, 300, 200, 500)
#'    p <- plot_profile(profile, "the best profile", sections = NULL)
#'    add_sections(p, scale = 500)
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of profile and scale.
#' @export
add_sections <- function(p, scale, shade = FALSE, ymin = NULL, ymax = NULL, opacity = 0.3) {

    # calculate yman and ymax
    if (is.null(ymin) || is.null(ymax)) {
        bounds <- min_max_plot(p)
        ymin <- if (is.null(ymin)) bounds$min else ymin
        ymax <- if (is.null(ymax)) bounds$max else ymax
    }

    # opacity to 1 if lines
    opacity <- if (shade) opacity else 1

    fillCol <- function(i) {
        colors <- list("tomato", "cornflowerblue", "chartreuse2", "tomato3", "tomato4")
        col_code <- col2rgb(colors[[i %% length(colors) + 1]])/255
        rgb(col_code[[1]], col_code[[2]], col_code[[3]], opacity)
    }

    restPre <- c(rep(ymax, scale), rep(NA, scale *2))
    active <- c(rep(NA, scale), rep(ymax, scale*0.5), rep(NA, scale*1.5))
    rest3min <- c(rep(NA, scale*1.5), rep(ymax, scale*0.5), rep(NA, scale))
    restPost <- c(rep(NA, scale*2), rep(ymax, scale))

    if (!shade) {
        p <- add_lines(p, y = c(ymin:ymax), x = scale, name = "Resting",
                       legendgroup = "Sections", text = "",
                       mode = "none", line = list(color = fillCol(1), width = 3),
                       hoverinfo = "none")
        p <- add_lines(p, y = c(ymin:ymax), x = (scale * 1.5), name = "Active",
                       legendgroup = "Sections", text = "",
                       mode = "none", line = list(color = fillCol(3), width = 3),
                       hoverinfo = "none")
        p <- add_lines(p, y = c(ymin:ymax), x = (scale * 2), name = "Initial Recovery",
                       legendgroup = "Sections", text = "",
                       mode = "none", line = list(color = fillCol(2), width = 3),
                       hoverinfo = "none")
        p <- layout(p, yaxis = list(range = c(ymin, ymax)))
    } else {
        p <- add_lines(p, y = restPre, x = c(1:length(restPre)), name = "Resting",
                       legendgroup = "Sections", text = "",
                       mode = "none", line = list(color = rgb(0, 0, 0, 0)),
                       hoverinfo = "none", fill = "tozeroy", fillcolor = fillCol(1)) %>%
            add_lines(y = active, x = c(1:length(active)), name = "Active",
                      legendgroup = "Sections", text = "",
                      mode = "none", line = list(color = rgb(0, 0, 0, 0)),
                      hoverinfo = "none",
                      fill = "tozeroy", fillcolor = fillCol(3)) %>%
            add_lines(y = rest3min, x = c(1:length(rest3min)), name = "Initial Recovery",
                      legendgroup = "Sections", text = "",
                      mode = "none", line = list(color = rgb(0, 0, 0, 0)),
                      hoverinfo = "none",
                      fill = "tozeroy", fillcolor = fillCol(2)) %>%
            add_lines(y = restPost, x = c(1:length(restPost)), name = "Recovery",
                      legendgroup = "Sections", text = "",
                      mode = "none", line = list(color = rgb(0, 0, 0, 0)),
                      hoverinfo = "none",
                      fill = "tozeroy", fillcolor = fillCol(1)) %>%
            layout(yaxis = list(range = c(ymin, ymax)))
    }

    return(p)
}



#' Plot all the activities
#' @description finds and plots all the activities in a energy data frame
#'
#' @param energy_df A dataframe with "energy" format as returned by \code{\link{load_tcx_file}}.
#' @param p A plot to be added to.
#' @param time_threshold The minumum number of seconds duration for an activity.
#' @param rest_threshold The maximum number of resting seconds within and activity.
#' @param type The type of data plotted:
#' \itemize{
#' \item "bpm" : plots the heart rate data in bpm
#' \item "deltaE" : plots the change in energy expenditure
#' \item "totalE" : plots the total energy expenditure
#' }
#' @param real_time Plots data in real time or else uses indexes.
#' @param plot_all Plots the activities and all the data from the data frame.
#' @return A plotly object.
#' @family plotting functions
#' @seealso \code{\link{load_tcx_file}} for details on input format.
#' @export
plot_active <- function(energy_df, p = plot_ly(), time_threshold = 15, rest_threshold = 5, type = "bpm",
                        real_time = TRUE, plot_all = TRUE) {

    # find starting and ending points for all activities in the data frame
    start_end_points <- all_start_end_points(energy_df, time_threshold, rest_threshold)
    all_points <- start_end_points$points
    df <- start_end_points$df

    # plot all the data not just activities
    if (plot_all) {
        if (real_time) index <- df$time
        else index <- c(1:nrow(df))

        if (type == "bpm") data <- df$bpm
        else if (type == "deltaE") data <- df$deltaE
        else if (type == "totalE") data <- df$totalE
        else stop("type not found")

        p <- add_lines(p, y = data, x = index, name = type)
    }

    # if there are no activities
    if(length(all_points) == 0) {
        warning("no activities")
        return(p)
    }

    # plot new line for each activity
    for(i in c(1:nrow(all_points))) {
        points <- all_points[i,]

        if (type == "bpm") data <- df[points$start_point:points$end_point,]$bpm
        else if (type == "deltaE") data <- df[points$start_point:points$end_point,]$deltaE
        else if (type == "totalE")data <- df[points$start_point:points$end_point,]$totalE
        else stop("type not found")

        if (real_time) index <- df[points$start_point:points$end_point,]$time
        else index <- c(points$start_point:points$end_point)

        p <- add_trace(p, x = index, y = data,
                       type = "scatter", mode = "lines",
                       name = paste("activity ", i))
    }
    return(p)
}

#' Plots all the features of a profile
#' @description plots:
#' \itemize{
#' \item resting heart rate
#' \item active heart rate
#' \item initial heart rate recovery
#' \item final heart rate recovery
#' \item heart rate recovery every minute post cessation of exercise
#' }
#'
#' @param profile A profile to get features from
#'     See \code{\link{get_profile}} for a description of profile.
#' @param buffer_end The number seconds before activity included in profile.
#' @param p Plot to be added to.
#' @param label of the profile being plotted
#' @param join_feat plots all the features as one color and legend group, useful for plotting
#'     features of multiple profiles
#' @param col Color of the features when join_feat is true
#' @param annotate_max_hr Add an annotation to the maximum heart rate
#' @return plot with the features of a profile
#' @examples
#'    profile1 <- get_profile(df1, 300, 200, 500)
#'    plot_features(profile1, 200)
#'
#'    profile2 <- get_profile(df2, 300, 200, 500)
#'    plot_features(profile2, 200, label = "The best features")
#'
#'    p <- plot_features(profile1, 200, join_feat = TRUE)
#'    plot_features(profile2, 200, p = p, join_feat = TRUE, col = "orange")
#'
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#' @export
plot_features_profile <- function(profile, buffer_end, p = NULL, label = "Features", join_feat = FALSE,
                           col = get_color(0), annotate_max_hr = TRUE) {

    mean = if (is.atomic(profile)) profile else profile$mean

    scale = length(mean)/3

    # max heart rate annotation
    y_val_max <- max_hr(mean)
    x_val_max <- match(y_val_max, mean)

    # get features as plottable objects

    # resting HR
    r <- resting_hr(mean)
    r <- c(rep(r, scale), rep(NA, scale*2))

    # active polynomial
    p1 <- as.numeric(predict(profile_coefficients(mean, start=scale, end=scale*1.5, degree=5, model_out = TRUE)))
    p1 <- c(rep(NA, (scale - 1)), p1, rep(NA, (scale*1.5)))

    # resting polynomial
    p2 <- as.numeric(predict(profile_coefficients(mean, start=scale*1.5, end=scale*2, degree=5, model_out = TRUE)))
    p2 <- c(rep(NA, (scale*1.5 - 1)), p2, rep(NA, scale))

    # slope
    s <- as.numeric(predict(slope(mean, model_out = TRUE)))
    s <- c(rep(NA, (scale*2 - 1)), s)

    # resting indicies
    x <- round(scale*1.5/18)
    gap <- 42

    yvals <- resting_indeces(mean, buffer_end/60, buffer_end)
    xvals <- which(mean %in% yvals)

    inds <- rep(NA, scale*3)
    inds[xvals] <- yvals


    # style helper functions
    line_style <- function(i) {
        # colors <- list("tomato", "tomato1", "tomato2", "tomato3", "tomato4")
        color <- if (join_feat) col else get_color(i)
        list(
            color = color,
            dash = "line",
            width = 2
        )
    }

    legend_group <- if (join_feat) paste(label, "feat") else ""
    show_legend  <- if (join_feat) FALSE else TRUE
    first_name   <- if (join_feat) label else "Resting Heart Rate"

    # plot all of the features
    if (is.null(p)) {
        title <- "Heart Rate Profile Features"
        title <- if (label != "Features") paste(label, title) else title
        p <- plot_ly(x = c(1:length(mean)), y = r, name = first_name, text = "",
                     mode = "lines", legendgroup = legend_group, showlegend = TRUE,
                     line = line_style(0), type = "scatter") %>%
            layout(title = title)
    } else {
        p <- add_lines(p, x = c(1:length(r)), y = r, name = first_name, text = "",
                       legendgroup = legend_group, showlegend = TRUE,
                       line = line_style(0))
    }

    # add features
    p <- add_lines(p, x = c(1:length(p1)), y = p1, name = "Active", text = "",
                   legendgroup = legend_group, showlegend = show_legend,
                   line = line_style(1)) %>%
        add_lines(x = c(1:length(p2)), y = p2, name = "3 min Resting", text = "",
                  legendgroup = legend_group, showlegend = show_legend,
                  line = line_style(2)) %>%
        add_lines(x = c(1:length(s)), y = s, name = "Recovery", text = "",
                  legendgroup = legend_group, showlegend = show_legend,
                  line = line_style(3)) %>%
        add_markers(x = xvals, y = yvals, name = "Resting Indices",
                    text = "", inherit = FALSE,
                    marker = list(
                        color = if (join_feat) col else get_color(4),
                        line = list(color = 'rgba(0,0,0,0)', width = 0)),
                    legendgroup = legend_group, showlegend = show_legend) %>%
        add_markers(x = x_val_max, y = y_val_max, name = "Maximum Heart Rate",
                    text = "", inherit = FALSE,
                    marker = list(
                        color = if (join_feat) col else get_color(5),
                        line = list(color = 'rgba(0,0,0,0)', width = 0)),
                    legendgroup = legend_group, showlegend = show_legend)


    # add layout options
    p <- layout(p, hovermode = "compare",
                yaxis = list(title = "BPM"),
                xaxis = list(title = "Sections"))

    return(p)
}

#' Plot features of multiple profiles
#' @description plot the features of all the profiles in a list on the same plot,
#'    to easily compare between features from different profiles
#'
#' @param profiles list of profiles or aggregate profiles to be plotted
#'     See \code{\link{get_profile}} for a description of profile.
#'     See \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @param buffer_end The number seconds before activity included in profile.
#' @return Plotly plot
#' @examples
#'    profiles <- get_profiles(dfs, 200, 300, 500)
#'    plot_features_profiles(profiles, names(profiles), 300)
#'
#'    agg_profile1 <- aggregate_profiles(select(profiles, "1"))
#'    agg_profile2 <- aggregate_profiles(select(profiles, "2"))
#'    plot_features_profiles(list(agg_profile1, agg_profile2), list("1", "2"), 300)
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#'     \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @export
plot_features_profiles <- function(profiles, labels, buffer_end) {
    p <- plot_ly()

    for (i in c(1:length(profiles))){
        p <- plot_features_profile(profiles[[i]], buffer_end, p = p, label = labels[[i]],
                           join_feat = TRUE, col = get_color(i), annotate_max_hr = FALSE)
    }
    p <- layout(p, title="Features of All Profiles",
                yaxis = list(title = "BPM"),
                xaxis = list(title = "Sections"))
    return(p)
}

#' Plot multiple profiles in 3d based on energy
#' @description plot multiple profiles in 3d against energy
#'
#' @inheritParams get_energy_profiles
#' @param plot_energy Plot energy curve instead of heart rate profile
#' @return Plotly plot
#' @examples plot_3d_profiles(energy_dfs, buffer_start, buffer_end, scale)
#' @family plotting functions
#' @seealso \code{\link{load_tcx_file}} for an explanation of energy dataframes
#' @export
plot_3d_profiles <- function(energy_dfs, buffer_start, buffer_end, scale,
                             time_threshold = 15, plot_energy = FALSE) {
    # step 1, get the profiles in a usable form

    # get profiles
    energy_profiles <- get_energy_profiles(energy_dfs, buffer_start, buffer_end,
                                           scale, time_threshold = time_threshold)

    # sort
    e_order <- order(unlist(lapply(energy_profiles, function(x) x$energy)))
    activities <- energy_profiles[e_order]

    # step 2, plot
    p <- plot_ly()
    line_type <- list(width = 6) #, color = ~BPM, colorscale = 'Viridis')

    for (i in c(1:length(activities))) {
        activity <- activities[[i]]
        name <- names(activities)[[i]]


        if (plot_energy) {
            data <- data.frame(
                Index = c(1:length(activity$totalE)),
                EnergyRank = activity$energy,
                Energy = activity$totalE
            )
            p <- add_trace(p, data = data, y = ~EnergyRank, x = ~Index, z = ~Energy,
                           line = line_type, name = paste(name, activity$energy),
                           type = "scatter3d", mode = "lines")
        } else {
            data <- data.frame(
                Index = c(1:length(activity$profile)),
                EnergyRank = activity$energy,
                BPM = activity$profile
            )
            p <- add_trace(p, data = data, y = ~EnergyRank, x = ~Index, z = ~BPM,
                           line = line_type, name = name,
                           type = "scatter3d", mode = "lines")
        }
    }

    return(p)
}

#' Plot multiple profiles in 3d landscape based on energy
#' @description plot multiple profiles in 3d landscape by interpolating heart rate data points
#'
#' @inheritParams get_energy_profiles
#' @return Plotly plot
#' @examples plot_3d_landscape(energy_dfs, buffer_start, buffer_end, scale)
#' @family plotting functions
#' @seealso \code{\link{load_tcx_file}} for an explanation of energy dataframes.
#' @export
plot_3d_landscape <- function(energy_dfs, buffer_start, buffer_end, scale,
                              time_threshold = 15, plot_energy = FALSE) {
    # step 1, get the profiles in a usable form

    # get profiles
    energy_profiles <- get_energy_profiles(energy_dfs, buffer_start, buffer_end,
                                           scale, time_threshold = time_threshold)

    # sort
    e_order <- order(unlist(lapply(energy_profiles, function(x) x$energy)))
    energies_ord <- energy_profiles[e_order]

    # step 2, interpolate
    es <- unlist(lapply(energies_ord, function(x) x$energy))
    matrix_indices = seq(from=round(range(es)[1]), to=round(range(es)[2]), by=1)
    profile_matrix <- matrix(NA, length(matrix_indices), scale*3)
    rownames(profile_matrix) <- matrix_indices

    lapply(energies_ord, function(X){
        tmpindex <- which(rownames(profile_matrix)==round(X$energy))
        profile_matrix[tmpindex,] <<- X$profile
        return("Yay")
    })

    interpolated_profile_matrix <- apply(profile_matrix, 2, function(Y){ return(approx(Y, n=length(Y))$y)})
    rownames(interpolated_profile_matrix) <- rownames(profile_matrix)

    # step 3, plot
    p <- plot_ly(y = as.numeric(rownames(interpolated_profile_matrix)),
                 z = ~interpolated_profile_matrix) %>% add_surface()
    return(p)
}


# plot_3d_profiles helper functions
#' Get profiles from energy dataframes with added information ready for plotting
#' with plot_3d_profiles
#' @description prepare data to be plotted by plot_3d_profiles
#'
#' @param energy_dfs list of profiles or aggregate profiles to be plotted
#'     See \code{\link{get_profile}} for a description of profile.
#'     See \code{\link{aggregate_profiles}} for a description of an aggregate profile.
#' @inheritParams get_profile
#' @return List of dataframes with profiles and other useful information
#' @family plotting functions
#' @seealso \code{\link{get_profile}} for a description of profile.
#'     \code{\link{aggregate_profiles}} for a description of an aggregate profile.
get_energy_profiles <- function(energy_dfs, buffer_start, buffer_end, scale,
                                time_threshold = 15) {
    activities <- lapply(energy_dfs, function(x) {
        act <- get_activities(x, buffer_start = buffer_start, buffer_end = buffer_end,
                              scale = scale, time_threshold = time_threshold,
                              all_activities = FALSE)
    })
    activities <- unlist(activities, recursive = FALSE)

    profiles <- lapply(activities, function(x) {
        result <- list()

        points <- list("start_point" = buffer_start + 1,
                       "end_point" = nrow(x) - buffer_end)
        range <- c((points$start_point-1):(points$end_point+1))

        result$profile <- get_profile(x, buffer_start = buffer_start,
                                      buffer_end = buffer_end, scale = scale,
                                      points = points)

        # calc energy area
        time <- as.numeric(x$time[range])
        energy <- x$totalE[range]
        quotient <- mapply('+', energy[1:length(energy)-1],
                           energy[2:length(energy)])/2
        product <- quotient * diff(time)

        time_diff <- range(x$time[range])
        # result$energy <- sum(product)
        # result$energy <- mean(energy)
        result$energy <- max(energy)

        result$totalE <- x$totalE
        result$bpm <- x$bpm
        result$range <- range
        result$time <- x$time

        return(result)
    })

    return(profiles)
}



