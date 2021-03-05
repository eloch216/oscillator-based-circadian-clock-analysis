## This script uses the functions in 'process_SURFRAD_utilities.R' to load,
## process, and save all of the raw SURFRAD data according to the settings
## specified in 'process_SURFRAD_settings.R'. The loading and processing proceed
## in steps, with results saved as files along the way to save time in case
## the process gets interrupted or parts of it need to be redone.

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/process_SURFRAD_utilities.R")
source("utilities/process_SURFRAD_settings.R")

## Specify which operations to perform. Set all of these to TRUE to completely
## reprocess the data.
LOAD_AND_SAVE_SURFRAD <- TRUE   # produces 'YYYY_original.csv' files
COMBINE_HOURLY_SURFRAD <- TRUE  # produces 'all_raw_hourly_data.Rdata'
CLEAN_HOURLY_SURFRAD <- TRUE    # produces 'all_clean_hourly_data.Rdata'
SPLIT_HOURLY_SURFRAD <- TRUE    # produces 'YYYY_processed.csv' files
                                # and 'surfrad_hourly_cdt.Rdata'

## Combine the raw SURFRAD data into one CSV file for each year
if (LOAD_AND_SAVE_SURFRAD) {
    for (i in 1:length(SURFRAD_SUBDIRECTORY_PATHS)) {
        load_and_save_directory_surfrad(
            SURFRAD_SUBDIRECTORY_PATHS[i],
            SURFRAD_SUBDIRECTORY_NAMES[i],
            SUFRAD_COLUMNS,
            SUFRAD_COLUMN_NAMES,
            SURFRAD_PROCESSED_DIR
        )
    }
}

## Bin the SURFRAD data into hourly values and combine everything into one giant
## data frame that includes data from all years
if (COMBINE_HOURLY_SURFRAD) {
    # Get hourly data from the first directory
    all_raw_hourly_data <- hourly_data_for_year_surfrad(
        SURFRAD_SUBDIRECTORY_NAMES[1],
        SURFRAD_PROCESSED_DIR,
        NQC_VARIABLE_NAMES,
        QC_VARIABLE_NAMES,
        QC_NAMES
    )

    # Add data from subsequent years
    for (i in 2:length(SURFRAD_SUBDIRECTORY_NAMES)) {
        all_raw_hourly_data <- rbind(
            all_raw_hourly_data,
            hourly_data_for_year_surfrad(
                SURFRAD_SUBDIRECTORY_NAMES[i],
                SURFRAD_PROCESSED_DIR,
                NQC_VARIABLE_NAMES,
                QC_VARIABLE_NAMES,
                QC_NAMES
            )
        )
    }

    # Save the resulting data frame
    save(
        all_raw_hourly_data,
        file=paste0(SURFRAD_PROCESSED_DIR, "/all_raw_hourly_data.Rdata")
    )
}

## Convert the hourly data from UTC to CDT, try to fill in any missing values,
## and split the big data frame into yearly ones. We need to do the processing
## with the full data set because the original yearly divisions are in UTC time
## rather than CDT, and there are some gaps at the beginning or ends of years
## that otherwise wouldn't be possible to fill.
if (CLEAN_HOURLY_SURFRAD) {
    # Load the big data frame with all the raw hourly data
    load(file=paste0(SURFRAD_PROCESSED_DIR, "/all_raw_hourly_data.Rdata"))

    # Convert to CDT
    all_clean_hourly_data <-
                convert_raw_surfrad_hourly_to_cdt(all_raw_hourly_data)

    # Choose variables that we should attempt to "clean"
    #  by replacing NA values with estimations
    clean_variable_names <- c(
        NQC_VARIABLE_NAMES,
        QC_VARIABLE_NAMES
    )

    # Clean the hourly data
    all_clean_hourly_data <- replace_nans_surfrad(
        all_clean_hourly_data,
        clean_variable_names
    )

    # Save the resulting data frame
    save(
        all_clean_hourly_data,
        file=paste0(SURFRAD_PROCESSED_DIR, "/all_clean_hourly_data.Rdata")
    )
}

if (SPLIT_HOURLY_SURFRAD) {
    # Load the big data frame with all the clean hourly data
    load(file=paste0(SURFRAD_PROCESSED_DIR, "/all_clean_hourly_data.Rdata"))

    # Split the hourly data into yearly chunks, saving each as a csv file and
    # storing them in the workspace
    years <- unique(all_clean_hourly_data$year)
    for (year in years) {
        # Get the subset
        year_chunk <- all_clean_hourly_data[which(all_clean_hourly_data$year == year),]

        # Remove the rownames that appear after subsetting
        rownames(year_chunk) <- c()

        # Save it as a csv file
        write.csv(
            year_chunk,
            paste(SURFRAD_PROCESSED_DIR, "/", year, "_processed.csv", sep=""),
            row.names = FALSE
        )

        # Store it as a global variable.  The <<- operator makes a variable in
        # the parent scope (not necessarily the global environment).
        # eval(parse(text=smthng)) evaluates the string smthng as if it were a
        # direct command. (I'm not sure if there's any other way to create
        # variables with names based on a number or string).
        command <- paste("surfrad", year, " <<- year_chunk", sep="")
        eval(parse(text=command))
    }

    # Save all the data frames
    save(
        list = ls(pattern=glob2rx("surfrad*")),
        file = paste0(SURFRAD_PROCESSED_DIR, "/surfrad_hourly_cdt.RData")
    )
}
