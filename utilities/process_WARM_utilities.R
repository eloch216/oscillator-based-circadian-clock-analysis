## This file defines functions used for loading, processing, and storing weather
## data from WARM

source("utilities/celestial_mechanics_utilities.R")
source("utilities/temperature_utilities.R")
source("utilities/solar_utilities.R")

## Define a function that determines day of year (DOY) from a Y/M/D triplet
day_of_year <- function(year, month, day) {
    # First check if this is a leap year, and then get the offset for the start
    # of the month, which is the DOY corresponding to the first of the month
    month_offset <- 0
    
    if((year %% 4) == 0) {
        # This is a leap year
        month_offset <- switch(
            month,
            1,
            32,
            61,
            92,
            122,
            153,
            183,
            214,
            245,
            275,
            306,
            336
        )
    }
    else {
        # This is not a leap year
        month_offset <- switch(
            month,
            1,
            32,
            60,
            91,
            121,
            152,
            182,
            213,
            244,
            274,
            305,
            335
        )
    }
    
    return(month_offset + day - 1)
}

## Define a function that loads all the information in the WARM data file
load_warm_dataset <- function(
    data_file_name,
    data_column_names,
    error_column_names
)
{
    # Load the data from the text file
    cmiday_names <- read.delim(
        data_file_name,
        sep = "\t",
        header = TRUE,
        nrows = 1
    )
    
    cmiday <- read.delim(
        data_file_name,
        sep = "\t",
        header = FALSE,
        skip = 2,
        stringsAsFactors = FALSE
    )
    
    colnames(cmiday) <- colnames(cmiday_names)
    
    # Search for the first row where year is blank
    # This indicates the end of the data
    end_index <- 1
    for (i in 1:length(cmiday$year)) {
        if(cmiday$year[i] == "") {
            end_index <- i - 1
            break
        }
    }
    
    # Get rid of the end of the table
    cmiday <- cmiday[1:end_index,]
    
    # Subset the table to the data columns
    cmiday_data <- cmiday[,data_column_names]
    
    # Get the corresponding error indicators
    cmiday_error <- cmiday[,error_column_names]
    
    # Go through the data and replace any missing entries by NA. Some missing
    # entries are identified by an "M" in the corresponding error column. Others
    # are identified by an empty string in the data column. Still others are
    # already shown as NA.
    for (i in 1:length(cmiday_data[1,])) {      # Columns
        for (j in 1:length(cmiday_data[,1])) {  # Rows
            if (cmiday_error[j,i] == "M" ||
                    (!is.na(cmiday_data[j,i]) && cmiday_data[j,i] == "")) {
                cmiday_data[j,i] <- NA
            }
        }
    }
    
    # Add a column for DOY
    cmiday_data$doy <- cmiday_data$day
    for (i in 1:length(cmiday_data$day)) {
        cmiday_data$doy[i] <- day_of_year(
            as.numeric(cmiday_data$year[i]),
            cmiday_data$month[i],
            cmiday_data$day[i]
        )
    }
    
    return(cmiday_data)
}

## Define a function for estimating missing values in a daily dataset. We just
## interpolate to fill in missing values on particular days.
replace_nans_warm <- function(daily_data, column_names, solar_column_name) {
    # Add a new column expressing days since 1 January 1985, so each day can be
    # specified in a unique way with a single column
    daily_data$doy_1985 <- (as.numeric(daily_data$year) - 1985) * 365 + 
                            floor(0.25 * (as.numeric(daily_data$year) - 1985)) +
                            as.numeric(daily_data$doy)
    
    
    # Do the procedure for each column / parameter name
    for (icol in 1:length(column_names)) {
        # Make the name for a new "clean" version of this column
        clean_name <- paste(column_names[icol], "_clean", sep="")
        
        # Add the clean column
        daily_data[,clean_name] <- daily_data[,column_names[icol]]
        
        # Get the index of the original parameter column and the clean version
        col_index <- grep(
            paste0("^", column_names[icol], "$"),
            colnames(daily_data)
        )
        col_index_clean <- grep(
            paste0("^", clean_name, "$"),
            colnames(daily_data)
        )
        
        # Get the daily values of this parameter
        idata <- daily_data[,col_index]
        
        # Check to see if there are at least two values that aren't NA. If there
        # aren't, just skip this column since we won't be able to fix such a
        # serious problem with the input data.
        if(sum(!is.na(idata)) <= 2) next
        else {
            # Get the doy values
            doy_values <- daily_data$doy_1985
            
            # Create a function that interpolates the daily values
            f <- approxfun(doy_values, idata, method="linear", rule=1)
            
            # Replace any NAs
            for(i in 1:length(idata)) {
                if (is.na(daily_data[i,col_index])) {
                    daily_data[i,col_index_clean] = f(daily_data$doy_1985[i])
                }
            }
        }
    }
    
    # Remove the doy_1985 column
    daily_data$doy_1985 <- NULL
    
    # Add a solar_source column (indicating where values were interpreted for
    # the solar column)
    daily_data$solar_source <- 1
    for (i in 1:length(daily_data$solar_source)) {
        if(is.na(daily_data[[solar_column_name]][i])) {
            daily_data$solar_source[i] <- 2
        }
    }
    
    # Return the result
    return(daily_data)
}

## Define a function that generates hourly profiles from a set of daily data
generate_hourly_profiles <- function(
    longitude,
    latitude,
    year,
    doy,
    total_daily_radiation,
    par_percent,
    solar_source,
    sunrise_threshold_angle,
    t_yesterday_max,
    t_today_min,
    t_today_max,
    t_tomorrow_min,
    atmospheric_transmittance,
    atmospheric_pressure
)
{
    # Get the hourly solar coordinates for the first point
    hourly_sun_position_profile <- generate_hourly_sun_position_profile(
        longitude,
        latitude,
        year[1],
        doy[1]
    )
    
    # Get the hourly solar radiation profile for the first point
    solar_result <- generate_hourly_solar_profile(
        hourly_sun_position_profile,
        total_daily_radiation[1],
        par_percent,
        atmospheric_transmittance,
        atmospheric_pressure
    )
    solar_result$solar_source <- solar_source[1]
    
    # Get the hourly temperature profile for the first point
    temp_result <- generate_hourly_temperature_profile(
        year[1],
        doy[1],
        latitude,
        longitude,
        sunrise_threshold_angle,
        t_yesterday_max[1],
        t_today_min[1],
        t_today_max[1],
        t_tomorrow_min[1]
    )
    
    # Combine all the profiles
    result <- cbind(
        hourly_sun_position_profile,
        solar_result,
        temp_result
    )
    
    # Get the rest of the results and add them
    for (i in 2:length(year)) {
        # Get the hourly solar coordinates for this point
        hourly_sun_position_profile <- generate_hourly_sun_position_profile(
            longitude,
            latitude,
            year[i],
            doy[i]
        )
        
        # Get the hourly solar radiation profile for this point
        solar_result <- generate_hourly_solar_profile(
            hourly_sun_position_profile,
            total_daily_radiation[i],
            par_percent,
            atmospheric_transmittance,
            atmospheric_pressure
        )
        solar_result$solar_source <- solar_source[i]
        
        # Get the hourly temperature profile for this point
        temp_result <- generate_hourly_temperature_profile(
            year[i],
            doy[i],
            latitude,
            longitude,
            sunrise_threshold_angle,
            t_yesterday_max[i],
            t_today_min[i],
            t_today_max[i],
            t_tomorrow_min[i]
        )
        
        # Combine all the profiles
        new_result <- cbind(
            hourly_sun_position_profile,
            solar_result,
            temp_result
        )
        
        # Add them to the total result
        result <- rbind(result, new_result)
    }
    
    # Return the result
    return(result)
}

## Define a function that gets a year of hourly data from a set of daily values
get_hourly_warm_data <- function(
    daily_data,
    longitude,
    latitude,
    par_percent,
    sunrise_threshold_angle,
    max_air_temp_col,
    min_air_temp_col,
    sol_col,
    atmospheric_transmittance,
    atmospheric_pressure
)
{   
    # Get yesterday's maximum temperature, just using zero for the first day
    daily_data$t_yesterday_max <- 0
    for (i in 2:length(daily_data$t_yesterday_max)) {
        daily_data$t_yesterday_max[i] <- daily_data[[max_air_temp_col]][i-1]
    }
    
    # Get tomorrow's minimum temperature, just using zero for the last day
    daily_data$t_tomorrow_min <- 0
    for (i in 1:(length(daily_data$t_tomorrow_min)-1)) {
        daily_data$t_tomorrow_min[i] <- daily_data[[min_air_temp_col]][i+1]
    }
    
    # Calculate hourly values
    result <- generate_hourly_profiles(
        longitude,
        latitude,
        daily_data$year,
        daily_data$doy,
        daily_data[[sol_col]],
        par_percent,
        daily_data$solar_source,
        sunrise_threshold_angle,
        daily_data$t_yesterday_max,
        daily_data[[min_air_temp_col]],
        daily_data[[max_air_temp_col]],
        daily_data$t_tomorrow_min,
        atmospheric_transmittance,
        atmospheric_pressure
    )
    
    # Add a doy_dbl column
    result$doy_dbl <- result$doy + result$hour / 24.0
    
    # Return the result
    return(result)
}