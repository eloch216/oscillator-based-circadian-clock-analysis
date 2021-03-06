## Defines some functions used for testing the oscillator clock's accuracy

source("utilities/weather_utilities.R")
source("utilities/celestial_mechanics_utilities.R")
source("utilities/clock_utilities.R")

## Define a function that runs a poincare clock each year and combines all the
## results together
run_clock_all_years <- function(
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    WARM
)
{
    # Make the list of years
    if (WARM) {
        years <- 1989:2019
    }
    else {
        years <- 1995:2019
    }

    # Get the weather data for the first year
    if (WARM) {
        command <- paste0("weather_data <- warm", years[1])
    }
    else {
        command <- paste0("weather_data <- surfrad", years[1])
    }
    eval(parse(text=command))

    # Prepare it for BioCro
    if (WARM) {
        biocro_weather_data <- convert_to_biocro_warm(weather_data)
    }
    else {
        biocro_weather_data <- convert_to_biocro_surfrad(weather_data)
    }

    # Run the simulation
    result <- run_biocro_clock(
        kick_strength,
        clock_gamma,
        clock_r0,
        clock_period,
        dawn_phase_initial,
        dusk_phase_initial,
        light_threshold,
        light_exp_at_zero,
        tracker_rate,
        biocro_weather_data,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose
    )

    # Add the results for subsequent years
    for(i in 2:length(years)) {
        # Get the weather data for this year
        if (WARM) {
            command <- paste0("weather_data <- warm", years[i])
        }
        else {
            command <- paste0("weather_data <- surfrad", years[i])
        }
        eval(parse(text=command))

        # Prepare it for BioCro
        if (WARM) {
            biocro_weather_data <- convert_to_biocro_warm(weather_data)
        }
        else {
            biocro_weather_data <- convert_to_biocro_surfrad(weather_data)
        }

        # Get the simulation result
        simresult <- run_biocro_clock(
            kick_strength,
            clock_gamma,
            clock_r0,
            clock_period,
            dawn_phase_initial,
            dusk_phase_initial,
            light_threshold,
            light_exp_at_zero,
            tracker_rate,
            biocro_weather_data,
            solver_method,
            output_step_size,
            adaptive_error_tol,
            adaptive_max_steps,
            verbose
        )

        # Store the result
        result <- rbind(result, simresult)
    }

    # Return the output
    return(result)
}

## Define a function that produces an average photoperiod length for each day
## in a data set with sub-daily values
get_average_daily_photoperiod_length <- function(yearly_data) {
    # Get the list of all days in the data
    doy <- unique(floor(yearly_data$doy_dbl))

    # Go through each doy and get the average day length & sunrise time
    day_length <- doy
    sunrise <- doy
    for(i in 1:length(doy)) {
        day_length[i] <- mean(
            yearly_data[which(yearly_data$doy==doy[i]),
            c("day_length")]
        )
        sunrise[i] <- mean(
            yearly_data[which(yearly_data$doy==doy[i]),
            c("sunrise")]
        )
    }

    # Return a data frame with the result
    result <- data.frame(
        doy,
        day_length,
        sunrise
    )

    return(result)
}

## Define a function that compares the output of an oscillator clock to
## celestial mechanics calculations, where 'multi_year_data' should be the
## output from a call to 'run_clock_all_years'
compare_clock_to_celestial <- function(
    multi_year_data,
    longitude,
    latitude,
    sunrise_threshold_angle
)
{
    # Get the list of all years present in the big result
    year <- unique(multi_year_data$year)

    # Get the average daily photoperiod length for the first year
    result <- get_average_daily_photoperiod_length(
        multi_year_data[which(multi_year_data$year==year[1]),]
    )

    # Calculate the daily photoperiod length for this year using celestial
    # mechanics
    calculated <- calculate_day_length(
        year=year[1],
        longitude = longitude,
        latitude = latitude,
        sunrise_threshold_angle = sunrise_threshold_angle
    )

    # Truncate the calculated results to only include days in the
    # oscillator data
    calculated <- calculated[which(calculated$doy %in% unique(result$doy)),]

    # Add a column for calculated day length, and the difference between the
    # oscillators and the calculation
    result$day_length_calculated <- calculated$day_length
    result$day_length_diff <- result$day_length - result$day_length_calculated
    result$day_length_error <- 100 * result$day_length_diff /
                                result$day_length_calculated

    # Add a column for calculated sunrise, and the difference between the
    # oscillators and the calculation
    result$sunrise_calculated <- calculated$sunrise
    result$sunrise_diff <- result$sunrise - result$sunrise_calculated
    result$sunrise_error <- 100 * result$sunrise_diff /
                                result$sunrise_calculated

    # Add a column for the year
    result$year <- year[1]

    # Process the other years
    for (i in 2:length(year)) {

        # Get the average daily length
        temp_result <- get_average_daily_photoperiod_length(
            multi_year_data[which(multi_year_data$year==year[i]),]
        )

        # Calculate the day length for this year using celestial mechanics
        calculated <- calculate_day_length(
            year=year[i],
            longitude = longitude,
            latitude = latitude,
            sunrise_threshold_angle = sunrise_threshold_angle
        )

        # Truncate the calculated results to only include days in the oscillator
        # data
        calculated <- calculated[
            which(calculated$doy %in% unique(temp_result$doy)),]

        # Add a column for calculated day length, and the difference between the
        # oscillators and the calculation
        temp_result$day_length_calculated <- calculated$day_length
        temp_result$day_length_diff <- temp_result$day_length -
                                            temp_result$day_length_calculated
        temp_result$day_length_error <- 100 * temp_result$day_length_diff /
                                            temp_result$day_length_calculated

        # Add a column for calculated sunrise, and the difference between the
        # oscillators and the calculation
        temp_result$sunrise_calculated <- calculated$sunrise
        temp_result$sunrise_diff <- temp_result$sunrise -
                                        temp_result$sunrise_calculated
        temp_result$sunrise_error <- 100 * temp_result$sunrise_diff /
                                        temp_result$sunrise_calculated

        # Add a column for the year
        temp_result$year <- year[i]

        # Add it to the result
        result <- rbind(result, temp_result)
    }

    # Sort by doy
    result <- result[order(result$doy),]

    # Return
    return(result)
}

## Define a function that compares the output of an oscillator clock with the
## results of celestial mechanics calculations and determines statistics
## describing the differences, where 'multi_year_data' should be the output from
## a call to 'run_clock_all_years'
analyze_oscillator_stats <- function(
    multi_year_data,
    longitude,
    latitude,
    sunrise_threshold_angle
)
{
    # Get the day length vs doy for all years in the dataset
    total_series <- compare_clock_to_celestial(
        multi_year_data,
        longitude,
        latitude,
        sunrise_threshold_angle
    )

    # Get all the doy values
    doy <- unique(total_series$doy)

    # Initialize a bunch of vectors to the correct length
    day_length_average <- doy
    day_length_stdev <- doy
    day_length_range <- doy
    day_length_diff_average <- doy
    day_length_diff_stdev <- doy
    day_length_error_average <- doy
    day_length_error_stdev <- doy
    day_length_calc_average <- doy
    day_length_calc_stdev <- doy

    day_length_f1 <- doy
    day_length_f2 <- doy
    day_length_f3 <- doy
    day_length_f4 <- doy
    day_length_f5 <- doy
    day_length_IQR <- doy

    sunrise_range <- doy
    sunrise_diff_average <- doy

    sunrise_f1 <- doy
    sunrise_f2 <- doy
    sunrise_f3 <- doy
    sunrise_f4 <- doy
    sunrise_f5 <- doy
    sunrise_IQR <- doy

    # Get stats for each day of the year
    for (i in 1:length(doy)) {
        doy_subset <- total_series[which(total_series$doy==doy[i]),]

        day_length_average[i] <- mean(doy_subset$day_length)
        day_length_stdev[i] <- sd(doy_subset$day_length)
        day_length_range[i] <- max(doy_subset$day_length) -
                                min(doy_subset$day_length)
        day_length_diff_average[i] <- mean(doy_subset$day_length_diff)
        day_length_diff_stdev[i] <- sd(doy_subset$day_length_diff)
        day_length_error_average[i] <- mean(doy_subset$day_length_error)
        day_length_error_stdev[i] <- sd(doy_subset$day_length_error)
        day_length_calc_average[i] <- mean(doy_subset$day_length_calculated)
        day_length_calc_stdev[i] <- sd(doy_subset$day_length_calculated)

        day_length_five <- fivenum(doy_subset$day_length)
        day_length_f1[i] <- day_length_five[1]
        day_length_f2[i] <- day_length_five[2]
        day_length_f3[i] <- day_length_five[3]
        day_length_f4[i] <- day_length_five[4]
        day_length_f5[i] <- day_length_five[5]
        day_length_IQR[i] <- day_length_five[4] - day_length_five[2]

        sunrise_range[i] <- max(doy_subset$sunrise) - min(doy_subset$sunrise)
        sunrise_diff_average[i] <- mean(doy_subset$sunrise_diff)

        sunrise_five <- fivenum(doy_subset$sunrise)
        sunrise_f1[i] <- sunrise_five[1]
        sunrise_f2[i] <- sunrise_five[2]
        sunrise_f3[i] <- sunrise_five[3]
        sunrise_f4[i] <- sunrise_five[4]
        sunrise_f5[i] <- sunrise_five[5]
        sunrise_IQR[i] <- sunrise_five[4] - sunrise_five[2]
    }

    # Form a data frame and return the results
    result <- data.frame(
        doy,
        day_length_average,
        day_length_stdev,
        day_length_range,
        day_length_diff_average,
        day_length_diff_stdev,
        day_length_error_average,
        day_length_error_stdev,
        day_length_calc_average,
        day_length_calc_stdev,
        day_length_f1,
        day_length_f2,
        day_length_f3,
        day_length_f4,
        day_length_f5,
        day_length_IQR,
        sunrise_range,
        sunrise_diff_average,
        sunrise_f1,
        sunrise_f2,
        sunrise_f3,
        sunrise_f4,
        sunrise_f5,
        sunrise_IQR
    )

    return(result)
}
