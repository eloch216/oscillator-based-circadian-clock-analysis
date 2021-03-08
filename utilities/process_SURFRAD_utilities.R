## This file defines utility functions used for loading, processing, and storing
## the raw SUFRAD data

## Define a function for getting a year's worth of surfrad data from a directory
## without doing any data modification or processing
get_yearly_surfrad <- function(directory_path, columns, column_names) {
    # Store the current directory
    current_directory <- getwd()

    # Move to the target directory
    setwd(directory_path)

    # Get a list of all surfrad data files
    files <- list.files(pattern="\\.dat$")

    # Make an empty data frame to store the results
    result <- setNames(
        data.frame(matrix(ncol = length(column_names), nrow = 0)),
        column_names
    )

    # Go through the list of files
    for (i in 1:length(files)) {
        # Load the data into a temporary data frame
        temporary <- read.delim(files[i], sep="", skip=2, header=FALSE)

        # Subset the data fram to the desired columns
        temporary <- temporary[,columns]

        # Set the column names
        colnames(temporary) <- column_names

        # Add the data to the main dataframe
        result <- rbind(result, temporary)
    }

    # Go back to the original directory
    setwd(current_directory)

    # Make sure all columns contain numeric data
    result <- as.data.frame(sapply(result, as.numeric))

    # Return the result
    return(result)
}

## Define a function for converting a year's worth of surfrad data to hourly
## averages.
##
## The data value for hour = N should represent the average over that hour,
## i.e., all data values in the full set where hour = N
##
## The variables in nqc_variable_names are not quality controlled.
## The variables in qc_variable_names are quality controlled, with indicators
## specified by the values in qc_names
get_hourly_from_yearly_surfrad <- function(
    surfrad_data,
    nqc_variable_names,
    qc_variable_names,
    qc_names
)
{
    # Get the year
    year <- surfrad_data[1,1]

    # Create the lists of DOY and hour values
    num_doy <- 365 + 1 * ((year %% 4) ==  0)
    doys <- 1:num_doy
    hours <- 0:23

    # Get the full column list
    column_names <- c(
        "year",
        "doy",
        "hour",
        nqc_variable_names,
        qc_variable_names
    )

    # Make an empty data frame to store the results
    result <- setNames(
        data.frame(matrix(ncol = length(column_names), nrow = 0)),
        column_names
    )

    # Make a data frame with one row to store individual hourly time points
    temporary <- setNames(
        data.frame(matrix(ncol = length(column_names), nrow = 1)),
        column_names
    )

    # Go through each day, getting average data values at each hour
    for (doy in doys) {
        for (hour in hours) {
            # Start filling in the temporary data frame
            temporary[1] = year
            temporary[2] = doy
            temporary[3] = hour

            # Get the desired non-quality-controlled variables
            for (i in 1:length(nqc_variable_names)) {
                # Get a subset of the total data that corresponds to this doy,
                # hour, and variable
                data_subset <- surfrad_data[
                    which(surfrad_data$doy == doy & surfrad_data$hour == hour),
                    c(nqc_variable_names[i])
                ]

                # Check to see if any data was found
                # If it was, store it. Otherwise, store NA
                if(!is.na(data_subset[1])) {
                    temporary[i+3] <- mean(data_subset)
                }
                else {
                    temporary[i+3] <- NA
                }
            }

            # Get the desired quality-controlled variables
            for (i in 1:length(qc_variable_names)) {
                # Get a subset of the total data that corresponds to this doy,
                # hour, and variable
                data_subset <- surfrad_data[
                    which(surfrad_data$doy == doy & surfrad_data$hour == hour),
                    c(qc_variable_names[i], qc_names[i])
                ]

                # Check to see if any data was found
                # If it was, start to average it. Otherwise, store NA
                if (!is.na(data_subset[1,1])) {
                    # Start adding up all the values that passed the quality
                    # control check
                    counter <- 0
                    total <- 0
                    for (j in 1:length(data_subset[,1])) {
                        if (data_subset[j,2] == 0) {
                            counter <- counter + 1
                            total <- total + data_subset[j,1]
                        }
                    }

                    # Store the result if one or more good values was found
                    # Otherwise, store NA
                    if (counter > 0) {
                        temporary[i + length(nqc_variable_names) + 3] <- total /
                            counter
                    }
                    else {
                        temporary[i + length(nqc_variable_names) + 3] <- NA
                    }
                }
                else {
                    temporary[i + length(nqc_variable_names) + 3] <- NA
                }
            }

            # Add the this point to the total result
            result <- rbind(result, temporary)
        }
    }

    # Make sure all columns contain numeric data
    result <- as.data.frame(sapply(result, as.numeric))

    # Return the result
    return(result)
}

## Define a function that loads full yearly data from a directory, saves the
## data as a csv, and returns it as a data frame
load_and_save_directory_surfrad <- function(
    path,
    subdirectory_name,
    columns,
    column_names,
    output_dir
)
{
    # Get the year's data at the original timestep
    year_data <- get_yearly_surfrad(path, columns, column_names)

    # Save it
    write.csv(
        year_data,
        paste0(output_dir, "/", subdirectory_name, "_original.csv"),
        row.names=FALSE
    )

    return(year_data)
}

## Define a function that retrieves data from a SURFRAD subdirectory that was
## previously stored in a csv file, and then converts it to hourly values
hourly_data_for_year_surfrad <- function(
    subdirectory_name,
    output_dir,
    nqc_variable_names,
    qc_variable_names,
    qc_names
)
{
    # Load the data from the csv file
    year_data <- read.csv(
        file=paste0(output_dir, "/", subdirectory_name, "_original.csv")
    )

    # Get the corresponding hourly data
    hourly_data <- get_hourly_from_yearly_surfrad(
        year_data,
        nqc_variable_names,
        qc_variable_names,
        qc_names
    )

    # Return the hourly data
    return(hourly_data)
}

## Define a function that converts a year, DOY, hour triplet from UTC to CDT
utc_to_cdt <- function(year, doy, hour) {
    # CDT is UTC-5, so we just need to subtract 5 from the hour
    offset <- 5
    newhour <- hour - offset

    # Check to see if the DOY has changed
    if (newhour < 0) {
        # This is actually in the previous day
        newdoy <- doy - 1

        # Update the hour
        newhour <- newhour + 24
    }
    else newdoy <- doy

    # Check to see if the year has changed
    if (newdoy < 1) {
        # This is actually in the previous year
        newyear <- year - 1

        # Check to see if the previous year was a leap year
        leapyear <- (newyear %% 4) == 0

        # Update the doy
        if(leapyear) newdoy <- 366
        else newdoy <- 365
    }
    else newyear <- year

    # Return the results
    return(list(year=newyear, doy=newdoy, hour=newhour))
}

## Define a function that converts hourly data to CDT.
##
## Here we assume we are dealing with the raw output of
## get_hourly_from_yearly_surfrad, where the value at hour = N is the average
## over that hour.
##
## For BioCro, we want hourly values to represent the average over the preceding
## hour, so add 1 to the hour value before sending to utc_to_cdt.
convert_raw_surfrad_hourly_to_cdt <- function(hourly_data) {
    for (i in 1:length(hourly_data$year)) {
        new_time <- utc_to_cdt(
            hourly_data$year[i],
            hourly_data$doy[i],
            hourly_data$hour[i]+1 # add 1 to hour, as explained above
        )

        hourly_data$year[i] <- new_time$year
        hourly_data$doy[i] <- new_time$doy
        hourly_data$hour[i] <- new_time$hour
    }

    return(hourly_data)
}

## Define a function for estimating missing values in an hourly SURFRAD dataset
##
## This procedure is based on the assumption that nearby days have similar
## hourly profiles to each other. For each hour of the day, we extract a series
## of daily values for the parameter in question. Then we interpolate to fill in
## missing values on particular days.
##
## column_names contains the variables we wish to fix
replace_nans_surfrad <- function(hourly_data, column_names) {
    # Add a new column expressing days since 1 January 1993, so each day can be
    # specified in a unique way with a single column
    hourly_data$doy_1993 <- (hourly_data$year - 1993) * 365 +
                                floor(0.25 * (hourly_data$year - 1993)) +
                                hourly_data$doy

    # Do the procedure for each column / parameter name
    for (icol in 1:length(column_names)) {
        # Make the name for a new "clean" version of this column
        clean_name <- paste(column_names[icol], "_clean", sep="")

        # Add the clean column
        hourly_data[,clean_name] <- hourly_data[,column_names[icol]]

        # Get the index of the original parameter column and the clean version
        col_index <- grep(
            paste0("^", column_names[icol], "$"),
            colnames(hourly_data)
        )
        col_index_clean <- grep(
            paste0("^", clean_name, "$"),
            colnames(hourly_data)
        )

        # Go through each hour
        for(hour in 0:23) {
            # Get the daily values of the parameter at this hour
            data_at_hour <- hourly_data[
                which(hourly_data$hour == hour),
                col_index
            ]

            # Check to see if there are at least two values that aren't NA. If
            # there aren't, just skip this hour since we won't be able to fix
            # such a serious problem with the input data
            if(sum(!is.na(data_at_hour)) <= 2) next
            else {
                # Get the corresponding doy values
                doy_values <-
                        hourly_data[which(hourly_data$hour == hour),]$doy_1993

                # Create a function that interpolates the daily values
                f <- approxfun(
                    x = doy_values,
                    y = data_at_hour,
                    method = "linear",
                    rule = 1
                )

                # Replace any nans
                for(i in 1:length(hourly_data[,1])) {
                    if (hourly_data$hour[i] == hour &&
                            is.na(hourly_data[i,col_index])) {
                        hourly_data[i,col_index_clean] =
                            f(hourly_data$doy_1993[i])
                    }
                }
            }
        }
    }

    # Remove the doy_1993 column
    hourly_data$doy_1993 <- NULL

    # Return the result
    return(hourly_data)
}
