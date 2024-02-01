## This file defines utility functions for interacting with weather data once
## has been loaded and processed

source("utilities/process_SURFRAD_settings.R")
source("utilities/process_SURFRAD_utilities.R")
source("utilities/process_WARM_settings.R")
source("utilities/process_WARM_utilities.R")

## Define vectors of years
ALL_YEARS     <- seq(1989, 2022)           # all years where we have any weather data
SURFRAD_YEARS <- seq(1995, max(ALL_YEARS)) # all years with complete SURFRAD data
WARM_YEARS    <- seq(1990, max(ALL_YEARS)) # all years with complete WARM data
FULL_YEARS    <- SURFRAD_YEARS             # all years where we have complete data from both sources

## Load the stored weather data
load(file=paste0(SURFRAD_PROCESSED_DIR, "/surfrad_hourly_local_time.RData"))
load(file=paste0(WARM_PROCESSED_DIR, "/warm_hourly_local_time.RData"))

## Define a function that finds the longest nan-free interval in a vector
find_longest_interval <- function(data_vector) {
    # First to see if there are any non-NA values
    if (sum(!is.na(data_vector)) == 0 ) {
        # This vector is all NA
        return(list(start_index=0,end_index=0))
    }

    # Set up tracking items and lists
    num_intervals <- 0
    interval_starts <- c()
    interval_ends <- c()
    prev_val_is_na <- TRUE

    # Search through the data
    for (i in 1:length(data_vector)) {
        if (prev_val_is_na & !is.na(data_vector[i])) {
            # The previous value was NA but this value is not. This means that
            # we have found the start of a new interval. Store this index as the
            # start index of an interval.
            interval_starts[num_intervals+1] <- i
        }
        else if (!prev_val_is_na & is.na(data_vector[i])) {
            # The previous value was not NA but this value is. This means that
            # we have found the end of the interval. Store the previous index as
            # the interval end and increase the number of intervals.
            interval_ends[num_intervals+1] <- i - 1
            num_intervals <- num_intervals + 1
        }

        if (i == length(data_vector) & length(interval_starts) > length(interval_ends)) {
            # We reached the end of the data vector before ending an interval.
            # Store this index
            interval_ends[num_intervals+1] <- i
            num_intervals <- num_intervals + 1
        }

        # Store the NA status of the current data value
        prev_val_is_na <- is.na(data_vector[i])
    }

    # Get the length of each interval
    interval_lengths <- interval_ends - interval_starts

    # Get the index of the longest interval
    longest_index <- which.max(interval_lengths)

    # Get the start of the longest interval
    start_index <- interval_starts[longest_index]

    # Get the end index of the longest interval
    end_index <- interval_ends[longest_index]

    # Return the start and end indices of the longest NA-free interval
    return(list(start_index=start_index,end_index=end_index))
}

## Define a function for converting some clean hourly SURFRAD data into the
## format required for BioCro.
convert_to_biocro_surfrad <- function(clean_hourly_data) {
    # Get the year, doy, and hour fields as-is
    result <- clean_hourly_data[,c("year", "doy", "hour")]

    # Just rename the cleaned zen, par, direct, diffuse, temperature, and
    # windspeed fields
    result$zen <- clean_hourly_data$zen_clean
    result$par_watts <- clean_hourly_data$par_clean
    result$direct_watts <- clean_hourly_data$direct_n_clean
    result$diffuse_watts <- clean_hourly_data$diffuse_clean
    result$temp <- clean_hourly_data$temp_clean
    result$windspeed <- clean_hourly_data$windspeed_clean

    # Include a copy of direct_n, diffuse, and par_watts with the original NAs
    result$diffuse_dirty <- clean_hourly_data$diffuse
    result$direct_n_dirty <- clean_hourly_data$direct_n
    result$par_watts_dirty <- clean_hourly_data$par

    # Convert rh to a 0-1 scale rather than a percentage
    result$rh <- clean_hourly_data$rh_clean / 100.0

    # Get the cosine of the solar zenith angle
    result$cosine_zenith_angle <- cos(pi * clean_hourly_data$zen_clean / 180.0)

    # Add a global radiation value (=direct*cosine_zenith_angle + diffuse)
    result$global_watts <-
        clean_hourly_data$direct_n_clean * result$cosine_zenith_angle +
        clean_hourly_data$diffuse_clean

    result$global_watts_dirty <- clean_hourly_data$direct_n +
        clean_hourly_data$diffuse

    # Convert pressure to Pascals
    result$atmospheric_pressure <- clean_hourly_data$pressure_clean * 100

    # Get the solar PAR value (in umol photons m^-2 s^-1)
    result$solar <- result$year
    result$solar_source <- result$year
    for (i in 1:length(result$year)) {
        if (!is.na(clean_hourly_data$par[i])) {
            # Try to use the Licor PAR data first, converting from Watts to
            # umol/s using the conversion specified in the SURFRAD readme file
            result$solar[i] <- clean_hourly_data$par[i] * 4.6
            result$solar_source[i] <- 1
        }
        else if (!is.na(clean_hourly_data$direct_n[i]) &&
                 !is.na(clean_hourly_data$diffuse[i])) {
            # If the PAR data is not available, try to use the global radiation
            # data, converting from Watts to umol/s using the conversion
            # specified in the SURFRAD readme file and assuming 43% of the
            # radiation lies in the PAR band
            result$solar[i] <- 0.43 *
                (clean_hourly_data$direct_n[i] * result$cosine_zenith_angle[i] +
                    clean_hourly_data$diffuse[i]) * 4.6
            result$solar_source[i] <- 2
        }
        else {
            # If neither type of original data is available, store an NA, to be
            # interpolated later
            result$solar[i] <- NA
            result$solar_source[i] <- 3
        }
    }

    # Interpolate to fill any missing values
    result <- replace_nans_surfrad(result, c("solar"))
    result$solar <- result$solar_clean

    # Unfortunately, some years have gaps in the data that can't be easily
    # filled. To deal with this, we search through the data to find the longest
    # NA-free interval. For now, we just use solar as a proxy for all the data.
    interval_info <- find_longest_interval(result$solar)

    if(interval_info$end_index > interval_info$start_index) {
        # Get the longest NA-free interval of the data
        result <- result[interval_info$start_index:interval_info$end_index,]
    }
    else {
        # There is no acceptable interval! Just get the first point
        result <- result[1,]
    }

    # Remove the rownames that appear after subsetting
    rownames(result) <- c()

    # Return the result
    return(result)

}

## Define a function for converting some clean hourly WARM data into the
## format required for BioCro.
convert_to_biocro_warm <- function(clean_hourly_data) {
    # Get some fields as-is
    result <- clean_hourly_data[,c(
        "year",
        "doy",
        "hour",
        "solar",
        "solar_source",
        "cosine_zenith_angle",
        "temp",
        "precip"
    )]

    # Unfortunately, some years have gaps in the data that can't be easily
    # filled. To deal with this, we search through the data to find the longest
    # NA-free interval. For now, we just use solar as a proxy for all the data.
    interval_info <- find_longest_interval(result$solar)

    if(interval_info$end_index > interval_info$start_index) {
        # Get the longest NA-free interval of the data
        result <- result[interval_info$start_index:interval_info$end_index,]
    }
    else {
        # There is no acceptable interval! Just get the first point
        result <- result[1,]
    }

    # Remove the rownames that appear after subsetting
    rownames(result) <- c()

    # Return the result
    return(result)
}
