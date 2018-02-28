# Example pipeline that:
# 1. loads fitbit TCX files from a directory
# 2. converts each TCX files into a "energy" dataframe containing total energy,
#    delta energy, bpm, time, etc.
# 3. converts each energy dataframe into a "profile" which is a data frame
#    containing a scaled heart rate dynamic profile for info on profile scaling
#    see scaling documentation below
# 4. (OPTIONAL) converts individual profiles into and aggregated profile with
#    mean and standard deviation
# 5. (OPTIONAL) extract features from a profile/list of profiles
# 6. (OPTIONAL) plot profiles in various configurations
#       - a single profile
#       - multiple single profiles
#       - an aggregate profile
#       - multiple aggregate profiles
#       - can add optional labelled sections/shading

readline("press any key to continue")

################
# dependancies #
################

# allows padding of missing time series data
library(padr)

# allows plotting of profiles
library(plotly)

# XML for working with tcx data
library(XML)

# load this package
library(hrdp)

readline("press any key to continue")

############
# settings #
############

# paths to example files and directory
single_tcx_file_path <- system.file("extdata", "stair_climb.tcx", package = "CardiacProfileR")
directory_tcx_files <- system.file("extdata", "exercises/", package = "CardiacProfileR")


# used for profile creation
# see ?get_profile for description of these parameters
buffer_start <- 100
buffer_end <- 200
scale <- 500
rest_sec <- 180


readline("press any key to continue")


##################
# load tcx files #
##################

# load single file
energy_df <- load_tcx_file(single_tcx_file_path)

## OR ##

# load all tcx files in directory
# DEFAULT: recursive = TRUE
# set recursive = FALSE if you only want to load the files in the top level
# see ?load_tcx_dir
energy_dfs <- load_tcx_dir(directory_tcx_files, recursive = TRUE)
str(energy_dfs)

readline("press any key to continue")

# select subset of energy_dfs
# use a regex on the energy_dfs names to select certain loaded energy dataframes
names(energy_dfs)  # all energy dataframes
readline("press any key to continue")
jogging <- select_list(energy_dfs, "Person1.*Jog")
names(jogging)     # only jogging dataframes
readline("press any key to continue")

# merge multiple energy dataframes into one
# NOTE: possible because they share the same column names
jogging_merged <- merge_list(jogging)
str(jogging_merged)
readline("press any key to continue")


#################
# get profiles #
#################

# get a profile from a single energy dataframe
# call get_profiles (which takes a list of energy dataframes)
# see ?get_profiles for an explanation of all the parameters
energy_list <- list(energy_df)  # get profiles MUST take in a list
single_profile <- get_profiles(energy_list, buffer_start, buffer_end,
                               scale, rest_sec = rest_sec,
                               norm = FALSE, all_activities = FALSE,
                               time_threshold = 15, rest_threshold = 5)
readline("press any key to continue")

# get_profiles returns a list, to extract the first one do this:
single_profile <- single_profile[[1]]
str(single_profile)
readline("press any key to continue")

# get a list of profiles from a list of energy dataframes
# SIDE NOTE:
# A single energy dataframe (i.e. a loaded tcx file) may contain multiple
# activities. If you only want to extract the first activity out of each
# dataframe set all_activities = FALSE
many_profiles <- get_profiles(energy_dfs, buffer_start, buffer_end,
                              scale, rest_sec = rest_sec,
                              norm = FALSE, all_activities = TRUE,
                              time_threshold = 15, rest_threshold = 5)

# NOTE some energy dfs end contain multiple activities
str(many_profiles)  # all extracted profiles
readline("press any key to continue")

# example of energy df with multiple activities
names(select_list(many_profiles, "Person2/Run2/Sprint"))
readline("press any key to continue")

# example of energy df with a single activity
names(select_list(many_profiles, "Person1/Run1/Walk"))
readline("press any key to continue")


# get a list of profiles normalised between 0 and 1
many_norm_profiles <- get_profiles(energy_dfs, buffer_start, buffer_end,
                                   scale, rest_sec = rest_sec,
                                   norm_resting = TRUE, all_activities =TRUE,
                                   time_threshold = 15, rest_threshold = 5)

plot_profiles(many_norm_profiles, names(many_norm_profiles))
readline("press any key to continue")


##########################
# get aggregate profiles #
##########################

# Agreggate profiles to find the mean and standard deviation of all profiles
# Agreggate heart rate profiles for each person
person1 <- select_list(many_profiles, "Person1.*/Walk") # select all profiles for person 1
person2 <- select_list(many_profiles, "Person2.*/Walk") # select all profiles for person 2
agg_profile1 <- aggregate_profiles(person1)
agg_profile2 <- aggregate_profiles(person2)

str(person1)  # list of profiles
readline("press any key to continue")
str(agg_profile1)  # their aggregate
readline("press any key to continue")


#################
# plot profiles #
#################

# Plot a single profile
plot_profile(single_profile, "Stair Climb", sections = "shade",
             title = "Stair Climb (shaded sections)")
readline("press any key to continue")

# use lines instead of shading
plot_profile(single_profile, "Stair Climb", sections = "lines",
             title = "Stair Climb (line divided sections)")
readline("press any key to continue")

# no shading, or add shading manually
p <- plot_profile(single_profile, "Stair Climb", sections = "none",
                  title = "Stair Climb (line divided sections)")
p
readline("press any key to continue")

p <- add_sections(p, scale = scale, shade = TRUE, opacity = 0.4)  # add shade
p
readline("press any key to continue")

p <- add_sections(p, scale = scale, shade = FALSE)  # add lines
p
readline("press any key to continue")

# Plot many profiles
subset <- select_list(many_profiles, "Person1")
plot_profiles(subset, names(subset), sections = "shade",
              title = "Many Profiles")
readline("press any key to continue")

# Plot an aggregate profile
p <- plot_profile(agg_profile1, "Person1", sections = "lines",
                  title = "Person1 Aggregate")
p
readline("press any key to continue")

# you can also add another aggregate to the plot manually, simply pass in p
plot_profile(agg_profile2, "Person2", sections = "none",
             title = "Person1 Aggregate", p = p, color = "orange")
readline("press any key to continue")

# Plot many aggregate profiles - the better way
many_agg_profiles <- list(agg_profile1, agg_profile2)  # list them
labels <- c("Person1", "Person2")  # give them meaningful labels
plot_profiles(many_agg_profiles, labels, sections = "none",
              title = "Agg of Person 1 and 2")
readline("press any key to continue")


###################
# plot activities #
###################

# Plot heart rate during activities
p <- plot_active(energy_df, type = "bpm")
p
readline("press any key to continue")

# Add energy expenditure (pass in plot p)
plot_active(energy_df, p = p, type = "totalE")
readline("press any key to continue")

# Merge multiple data frames to plot them
merged_person1_run1 <- merge_list(select_list(energy_dfs, "Person1/Run1"))
plot_active(merged_person1_run1, real_time = TRUE,
            rest_threshold = 5, time_threshold = 15)
readline("press any key to continue")

# plot profiles in 3d based on energy
person1 <- select_list(energy_dfs, "Person1.*/(Walk|Sprint)")
person2 <- select_list(energy_dfs, "Person2")


# plot energies
plot_3d_profiles(person1, buffer_start, buffer_end, scale,
                 time_threshold = 30, plot_energy = F)
readline("press any key to continue")

# plot profiles
plot_3d_landscape(person1, buffer_start, buffer_end, scale,
                  time_threshold = 30)
readline("press any key to continue")

# plot profile landscape
plot_3d_landscape(person2, buffer_start, buffer_end, scale,
                  time_threshold = 30)
readline("press any key to continue")

plot_3d_landscape(energy_dfs, buffer_start, buffer_end, scale,
                  time_threshold = 30)
readline("press any key to continue")



########################################
# get features dataframe from profiles #
########################################

# get features for single profile
# give profiles used to find features as a list
single_features <- get_features(list("prof" = single_profile), buffer_end, rest_sec,
                                degree_coef_active = 5, degree_coef_init_rec = 5)
head(single_features)
readline("press any key to continue")

# get features for many profiles
# NOTE: many_profiles is already a named list
many_features <- get_features(many_profiles, buffer_end, rest_sec,
                  degree_coef_active = 5, degree_coef_init_rec = 5)
head(many_features)
readline("press any key to continue")


# get features from an aggregate profile
agg_profile1_features <- get_features(list("agg1" = agg_profile1), buffer_end, rest_sec,
                                      degree_coef_active = 5, degree_coef_init_rec = 5)
head(agg_profile1_features)
readline("press any key to continue")


# get features from multiple aggregate profiles
agg_profiles <- list("agg1" = agg_profile1, "agg2" = agg_profile2)
agg_profiles_features <- get_features(agg_profiles, buffer_end, rest_sec,
                      degree_coef_active = 5, degree_coef_init_rec = 5)
head(agg_profiles_features)
readline("press any key to continue")


##############################
# plot features of a profile #
##############################

# plot features of single profile
plot_features_profile(single_profile, buffer_end)
readline("press any key to continue")

# plot them on the profile
p <- plot_profile(single_profile, "Person 1", title = "Person 1 Features", sections = "none")
plot_features_profile(single_profile, buffer_end, p)
readline("press any key to continue")

# plot features on top of aggregate profile
p <- plot_profile(agg_profile1, "Person 1", sections = "none")
plot_features_profile(agg_profile1, buffer_end, p)
readline("press any key to continue")

# plot features of many profiles
plot_features_profiles(many_profiles, names(many_profiles), buffer_end)
readline("press any key to continue")

# plot features of multiple aggregate profiles
agg_profiles <- list(agg_profile1, agg_profile2)
names <- list("Person 1", "Person 2")
plot_features_profiles(agg_profiles, names, buffer_end)
readline("press any key to continue")


