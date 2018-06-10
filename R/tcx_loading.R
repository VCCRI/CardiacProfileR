# tcx_loading.R
# processing functions that
# load TCX files into energy dataframes

#' Process a single tcx file
#' @description process a single tcx file and return an energies dataframe.
#'
#' @param filename A filepath to a tcx file (.tcx)
#' @param mass The person's mass in kg. Default is 60
#' @return The energies dataframe from the file at \code{filename}
#' energy dataframe :
#' \itemize{
#' \item totalE  : total energy expenditure
#' \item deltaE  : delta energy expenditure
#' \item bpm     : heart rate in beats per minute (bpm)
#' \item deltaBPM: delta heart rate in beats per minute (bpm)
#' \item time    : POSIXct timestamps
#' \item rawbpm  : raw unsmoothed heart rate}
#' @examples
#' load_tcx_file("topDir/secondLevel/fitbitdata.tcx")
#' load_tcx_file("./fitbitdata.tcx")
#' load_tcx_file("fitbitdata.tcx")
#' @family load tcx functions
#' @seealso \code{\link{load_tcx_dir}} for loading multiple tcx files
#' @export
#' @references
#'    Energy expenditure calculations used are described in:
#'
#'    Weyand, P. G., Smith, B. R., Puyau, M. R., & Butte, N. F. (2010).
#'    The mass-specific energy cost of human walking is set by stature.
#'    The Journal of Experimental Biology, 213(Pt 23), 3972–3979.
#'
#'    Kawata, A., Shiozawa, N., & Makikawa, M. (2007).
#'    Estimation of Energy Expenditure during Walking Including UP/Down Hill.
#'    In R. Magjarevic & J. H. Nagel (Eds.), World Congress on Medical Physics And Biomedical Engineering 2006,
#'    441-444.
load_tcx_file <- function(filename, mass = 60) {
    data <- XML::xmlParse(filename)
    xml_data <- XML::xmlToList(data)

    ##############
    #grab dates and times

    activity <- xml_data$Activities[[1]]

    results <- list()

    id<-activity[["Id"]];
    start_time<-activity[["Lap"]]$.attrs;
    total_time<-as.numeric( activity[["Lap"]][["TotalTimeSeconds"]])
    total_distance<-as.numeric( activity[["Lap"]][["DistanceMeters"]])

    trackpoints<-activity[["Lap"]][["Track"]];

    ##############
    #grab dates and times
    time<-sapply(trackpoints,function(x){
        as.character(x[["Time"]]);
    });

    clock <- gsub(".+T(.{8}).*","\\1",time)
    date <- gsub("(.{10}).*","\\1",time)

    combo <- gsub("(.*)\\..*","\\1",time)
    combo <- gsub("T"," ",combo)

    ## convert to real dates / times and seconds
    ## this accounts for the difference in 1second / 5 second measurement resolution
    postime <- as.POSIXlt(combo)

    ############
    # capture full latitutde and longitude
    options(digits=20)

    distance<-sapply(trackpoints,function(x){
        as.numeric(x[["DistanceMeters"]]);
    });

    latitude<-sapply(trackpoints,function(x){
        as.numeric(x[["Position"]][["LatitudeDegrees"]]);
    });

    longitude<-sapply(trackpoints,function(x){
        as.numeric(x[["Position"]][["LongitudeDegrees"]]);
    });
    altitude<-sapply(trackpoints,function(x){
        as.numeric(x[["AltitudeMeters"]]);
    });

    bpm<-sapply(trackpoints,function(x){
        as.numeric(x[["HeartRateBpm"]][["Value"]]);
    });

    badpoints<-setdiff(which(distance==0),1);
    distance[badpoints] <- (distance[badpoints-1] + distance[badpoints+1]) / 2

    ## smoth the altitude
    smooth_altitude <- smooth.spline(altitude, penalty = 2)$y

    #######################################
    ## Energy calculation

    ## Mass is passed as a parameter, default is 60kg

    #h_displacement <- smooth.spline(unlist(hav.distances), penalty=3)$y

    h_displacement <- diff(distance)
    v_displacement <- diff(smooth_altitude)

    vh_slope <- v_displacement / h_displacement
    vh_slope[vh_slope>0.2] <- 0.2
    vh_slope[vh_slope< -0.2] <- -0.2

    # from literature
    Etm <- 7.98*(mass^(-0.29))*mass

    h_energy <- h_displacement * Etm
    total_energy <- h_energy * (1+vh_slope)
    delta_energy <- diff(total_energy)

    #######################################

    smooth_bpm <- smooth.spline(bpm, penalty=4)$y
    delta_bpm <- diff(smooth_bpm)
    delta_delta_bpm <- diff(delta_bpm)

    ######################################

    energy_combo <- data.frame(totalE = total_energy[-1], deltaE = delta_energy,
                               bpm = smooth_bpm[-(1:2)], deltaBPM = delta_bpm[-1],
                               time = postime[-c(1,2)], rawbpm = bpm[-(1:2)])
    return(energy_combo)
}


#' Process all tcx files in a directory
#' @description process all tcx files in a directory and produce a list of energy dataframes.
#'
#' @param directory A filepath to a directory.
#' @param recursive Process tcx files in all levels of the directory or only the top level.
#' @return named list of energy dataframes where the name is the path to the tcx file.
#'     See \code{\link{load_tcx_file}} for a description of an energy dataframe.
#' @examples
#' load_tcx_dir("topDir/secondLevel/")
#' load_tcx_dir("../")
#' load_tcx_dir(".", recursive = FALSE)
#' @family load tcx functions
#' @seealso \code{\link{load_tcx_file}} for loading multiple tcx files
#' @export
load_tcx_dir <- function(directory, recursive = TRUE) {
    # rm trailing /
    directory <- gsub("/$", "", directory)
    dirSlash <- paste0(directory, "/")

    # get all files
    files <- list.files(directory, full.names = F, recursive = recursive, pattern = "\\.tcx$")

    # start loading files into list of data frames
    loaded <- list()
    for (file in files) {
        print(file)
        path <- gsub(".tcx$", "", file)  # rm .tcx
        loaded[[path]] <- load_tcx_file(paste0(dirSlash, file))
    }

    return(loaded)
}


#' Select elements of a named list
#' @description select elements of a named list using a regex on the name.
#'
#' @param list A named list
#' @param regex A regular expression used to select elements
#' @return subset of the original list where the name matches \code{regex}
#' @examples
#' select(list("a" = 1, "abba" = 2, "b" = 3), "a")
#'     $ a   : num 1
#'     $ abba: num 2
#' select(list("a" = 1, "abba" = 2, "b" = 3), "b")
#'     $ abba: num 2
#'     $ b   : num 3
#' select(list("a" = 1, "abba" = 2, "b" = 3), "[[:alpha:]]{4}")
#'     $ abba: num 2
#' @family load tcx functions
#' @export
select_list <- function(list, regex) {
    return(list[grep(regex, names(list), perl = T)])
}

#' Select rows of a dataframe
#' @description select rows of a dataframe using a regex on the rownames
#'
#' @param df A dataframe with named rows
#' @param regex A regular expression used to select elements
#' @return subset of the original dataframe where the rowname matches \code{regex}
#' @examples
#' df <- t(data.frame("a" = c(1:3), "abba" = c(2:4), "b" = c(3:5)))
#' df
#'          [,1] [,2] [,3]
#'     a       1    2    3
#'     abba    2    3    4
#'     b       3    4    5
#' selectdf(df, "a")
#'          [,1] [,2] [,3]
#'     a       1    2    3
#'     abba    2    3    4
#' selectdf(df, "b")
#'          [,1] [,2] [,3]
#'     abba    2    3    4
#'     b       3    4    5
#' selectdf(df, "[[:alpha:]]{4}")
#'     [1] 2 3 4
#' @family load tcx functions
#' @export
selectdf <- function(list, regex) {
    return(list[grep(regex, rownames(list), perl = T),])
}

#' Merges a list of energy dataframes into one single dataframe
#' @description merges a list of energy dataframes into one single dataframe
#'
#' @param dflist A list of dataframes all with the same column names and
#'     with a time column for sorting
#' @return a single dataframe
#' @family load tcx functions
#' @export
merge_list <- function(dflist) {
    df <- do.call(rbind, dflist)  # merge Runs together into 1 df
    df <- df[order(df$time),]
    return(df)
}
