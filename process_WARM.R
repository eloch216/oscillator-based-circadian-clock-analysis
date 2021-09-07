## This script uses functions in 'process_WARM_utilities.R' to load, process,
## and save all of the raw WARM data according to the settings specified in
## 'process_WARM_settings.R'. The loading and processing proceeds in steps, with
## results saved as files along the way to save time in case the process gets
## interrupted or parts of it need to be redone.

source("set_defaults.R")
source("utilities/process_WARM_utilities.R")
source("utilities/process_WARM_settings.R")

## Specify which operations to perform. Set all of these to TRUE to completely
## reprocess the data.
LOAD_AND_SAVE_WARM <- TRUE      # produces 'warm_data_daily_raw.RData'
CLEAN_DAILY_WARM <- TRUE        # produces 'warm_data_daily_clean.RData'
CONVERT_WARM_TO_HOURLY <- TRUE  # produces 'warm_data_hourly.RData'
SPLIT_HOURLY_WARM <- TRUE       # produces 'YYYY_processed.csv' files and
                                # the 'warm_hourly_cdt.RData' file

## Read the data from the file
if (LOAD_AND_SAVE_WARM) {
    warm_data_daily_raw <- load_warm_dataset(
        paste0(WARM_RAW_DIR, "/", WARM_RAW_FILE),
        WARM_DATA_COLUMNS,
        WARM_ERROR_COLUMNS
    )

    save(
        warm_data_daily_raw,
        file=paste0(WARM_PROCESSED_DIR, "/warm_data_daily_raw.RData")
    )
}

## Try to replace missing values in the daily data settings
if (CLEAN_DAILY_WARM) {
    load(file=paste0(WARM_PROCESSED_DIR, "/warm_data_daily_raw.RData"))

    warm_data_daily_clean <- replace_nans_warm(
        warm_data_daily_raw,
        WARM_CLEAN_COLUMNS,
        WARM_SOLAR_COLUMN_NAME
    )

    save(
        warm_data_daily_clean,
        file=paste0(WARM_PROCESSED_DIR, "/warm_data_daily_clean.RData")
    )
}

## Convert the daily values to hourly ones
if (CONVERT_WARM_TO_HOURLY) {
    load(file=paste0(WARM_PROCESSED_DIR, "/warm_data_daily_clean.RData"))

    warm_data_hourly <- get_hourly_warm_data(
        warm_data_daily_clean,
        DEF_LONGITUDE,
        DEF_LATITUDE,
        DEF_PAR_PERCENT,
        DEF_SUNRISE_THRESHOLD_ANGLE,
        WARM_CLEAN_MAX_TEMP_COLUMN_NAME,
        WARM_CLEAN_MIN_TEMP_COLUMN_NAME,
        WARM_CLEAN_SOLAR_COLUMN_NAME,
        DEF_ATMOSPHERIC_TRANSMITTANCE,
        DEF_ATMOSPHERIC_PRESSURE,
        WARM_PRECIPITATION_COLUMN_NAME
    )

    save(
        warm_data_hourly,
        file=paste0(WARM_PROCESSED_DIR, "/warm_data_hourly.RData")
    )
}

## Split the big data frame into yearly chunks, saving them as csv files and in
## an R data file
if (SPLIT_HOURLY_WARM) {
    # Load the big data frame with all the clean hourly data
    load(file=paste0(WARM_PROCESSED_DIR, "/warm_data_hourly.RData"))

    # Split the hourly data into yearly chunks, saving each as a csv file and
    # storing them in the workspace
    years <- unique(warm_data_hourly$year)
    for (year in years) {
        # Get the subset
        year_chunk <- warm_data_hourly[which(warm_data_hourly$year == year),]

        # Remove the rownames that appear after subsetting
        rownames(year_chunk) <- c()

        # Save it as a csv file
        write.csv(
            year_chunk,
            paste(WARM_PROCESSED_DIR, "/", year, "_processed.csv", sep=""),
            row.names = FALSE
        )

        # Store it as a global variable.  The <<- operator makes a variable in
        # the parent scope (not necessarily the global environment).
        # eval(parse(text=smthng)) evaluates the string smthng as if it were a
        # direct command. (I'm not sure if there's any other way to create
        # variables with names based on a number or string).
        command <- paste("warm", year, " <<- year_chunk", sep="")
        eval(parse(text=command))
    }

    # Save all the data frames
    save(
        list = ls(pattern=glob2rx("warm*")),
        file = paste0(WARM_PROCESSED_DIR, "/warm_hourly_cdt.RData")
    )
}
