## Define functions used for testing the CROPGRO model for soybean development
## as described in Grimm at al. Parameter Estimation for Predicting Flowering
## Date of Soybean Cultivars. Crop Science 33, 137–144 (1993).

library(BioCro)

source("utilities/weather_utilities.R")
source("utilities/celestial_mechanics_utilities.R")
source("utilities/EdR.R")

## Define a function that runs the Grimm model using the astronomical day length
run_astronomical_grimm <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    tbase,
    topt,
    tmax,
    grimm_sowing_doy,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min,
    grimm_N_opt,
    weather_data,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose
)
{
    # First we need to calculate the day length for each (year, doy) pair in the
    # weather data
    times <- unique(weather_data[,c("year","doy")])
    day_length <- numeric(length(times$year))
    for (i in 1:length(day_length)) {
        day_length_info <- calculate_sunrise(
            times$year[i],
            times$doy[i],
            latitude,
            longitude,
            sunrise_threshold_angle
        )
        day_length[i] <- day_length_info$day_length
    }
    times$day_length <- day_length

    # Now add the info to the weather data
    weather_data$day_length <- -1
    for (i in 1:length(weather_data$year)) {
        weather_data$day_length[i] <-
            times[which(times$year==weather_data$year[i] &
                times$doy==weather_data$doy[i]), c("day_length")]
    }

    # Define the system inputs

    grimm_ss_modules <- c("grimm_soybean_flowering_calculator")

    grimm_deriv_modules <- c(
        "thermal_time_bilinear",
        "grimm_soybean_flowering"
    )

    grimm_initial_state <- list(
        TTc = 0.0,
        grimm_physiological_age = 0.0
    )

    grimm_parameters <- list(
        timestep = 1.0,
        tbase = tbase,
        topt = topt,
        tmax = tmax,
        grimm_sowing_doy = grimm_sowing_doy,
        grimm_juvenile_pd_threshold = grimm_juvenile_pd_threshold,
        grimm_flowering_threshold = grimm_flowering_threshold,
        grimm_juvenile_T0 = grimm_juvenile_T0,
        grimm_juvenile_T1 = grimm_juvenile_T1,
        grimm_juvenile_T2 = grimm_juvenile_T2,
        grimm_juvenile_T3 = grimm_juvenile_T3,
        grimm_T_min = grimm_T_min,
        grimm_T_opt = grimm_T_opt,
        grimm_N_min = grimm_N_min,
        grimm_N_opt = grimm_N_opt
    )

    grimm_solver <- list(
        type = solver_method,
        output_step_size = output_step_size,
        adaptive_rel_error_tol = adaptive_error_tol,
        adaptive_abs_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps
    )

    # Run the simulation
    result <- Gro_solver(
        grimm_initial_state,
        grimm_parameters,
        weather_data,
        grimm_ss_modules,
        grimm_deriv_modules,
        grimm_solver,
        verbose
    )

    # Return the result
    return(result)
}

## Define a function that runs the Grimm model using a Poincare clock to
## photoperiod length from an input weather dataframe
run_clock_grimm <- function(
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_sowing_doy,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min,
    grimm_N_opt,
    weather_data,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose
)
{
    # Define the system inputs

    grimm_ss_modules <- c(
        "light_from_solar",
        "oscillator_clock_calculator",
        "grimm_soybean_flowering_calculator"
    )

    grimm_deriv_modules <- c(
        "night_and_day_trackers",
        "poincare_clock",
        "thermal_time_bilinear",
        "grimm_soybean_flowering"
    )

    grimm_initial_state <- list(
        dawn_b = clock_r0 * sin(dawn_phase_initial * pi / 180),
        dawn_a = clock_r0 * cos(dawn_phase_initial * pi / 180),
        dusk_b = clock_r0 * sin(dusk_phase_initial * pi / 180),
        dusk_a = clock_r0 * cos(dusk_phase_initial * pi / 180),
        ref_b = 0.0,
        ref_a = 1.0,
        night_tracker = 1.0,
        day_tracker = 0.0,
        TTc = 0.0,
        grimm_physiological_age = 0.0
    )

    grimm_parameters <- list(
        timestep = 1.0,
        kick_strength = kick_strength,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period,
        light_threshold = light_threshold,
        light_exp_at_zero = light_exp_at_zero,
        tracker_rate = tracker_rate,
        tbase = tbase,
        topt = topt,
        tmax = tmax,
        grimm_sowing_doy = grimm_sowing_doy,
        grimm_juvenile_pd_threshold = grimm_juvenile_pd_threshold,
        grimm_flowering_threshold = grimm_flowering_threshold,
        grimm_juvenile_T0 = grimm_juvenile_T0,
        grimm_juvenile_T1 = grimm_juvenile_T1,
        grimm_juvenile_T2 = grimm_juvenile_T2,
        grimm_juvenile_T3 = grimm_juvenile_T3,
        grimm_T_min = grimm_T_min,
        grimm_T_opt = grimm_T_opt,
        grimm_N_min = grimm_N_min,
        grimm_N_opt = grimm_N_opt
    )

    grimm_solver <- list(
        type = solver_method,
        output_step_size = output_step_size,
        adaptive_rel_error_tol = adaptive_error_tol,
        adaptive_abs_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps
    )

    # Run the simulation
    result <- Gro_solver(
        grimm_initial_state,
        grimm_parameters,
        weather_data,
        grimm_ss_modules,
        grimm_deriv_modules,
        grimm_solver,
        verbose
    )

    # Return the result
    return(result)
}

## Define a function that extracts flowering information from a simulation
## result (as calculated by 'run_clock_grimm' or 'run_astronomical_grimm')
get_grimm_flowering_info <- function(
    simulation_result,
    doy_summer_begin,
    doy_summer_end
)
{
    # Find the first doy_dbl when the plant flowers
    flowering_date <- 0
    for (i in 1:length(simulation_result$grimm_flowering)) {
        if (simulation_result$grimm_flowering[i] > 0) {
            flowering_date <- simulation_result$doy_dbl[i]
            break
        }
    }
    simulation_result$flowering_date <- flowering_date

    # Get the average temperature during the summer
    summer_T_avg <-
        mean(simulation_result[which(simulation_result$doy >= doy_summer_begin &
            simulation_result$doy <= doy_summer_end), c("temp")])

    simulation_result$summer_T_avg <- summer_T_avg

    return(simulation_result)
}

## Define a function that calculates flowering time using the CROPGRO model
## where the photoperiod length has been determined using two different methods
## (celestial mechanics and oscillator clock)
compare_oscillator_to_astronomical <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_sowing_doy,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min_astro,
    grimm_N_opt_astro,
    grimm_N_min_clock,
    grimm_N_opt_clock,
    doy_summer_begin,
    doy_summer_end,
    weather_data,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose
)
{
    # First run the Grimm model using the oscillator clock
    clock_result <- run_clock_grimm(
        kick_strength,
        clock_gamma,
        clock_r0,
        clock_period,
        dawn_phase_initial,
        dusk_phase_initial,
        light_threshold,
        light_exp_at_zero,
        tracker_rate,
        tbase,
        topt,
        tmax,
        grimm_sowing_doy,
        grimm_juvenile_pd_threshold,
        grimm_flowering_threshold,
        grimm_juvenile_T0,
        grimm_juvenile_T1,
        grimm_juvenile_T2,
        grimm_juvenile_T3,
        grimm_T_min,
        grimm_T_opt,
        grimm_N_min_clock,
        grimm_N_opt_clock,
        weather_data,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose
    )

    # Get flowering info
    clock_result <- get_grimm_flowering_info(
        clock_result,
        doy_summer_begin,
        doy_summer_end
    )

    # Now run the Grimm model for the same weather data using the astronomical
    # day length
    astronomical_result <- run_astronomical_grimm(
        latitude,
        longitude,
        sunrise_threshold_angle,
        tbase,
        topt,
        tmax,
        grimm_sowing_doy,
        grimm_juvenile_pd_threshold,
        grimm_flowering_threshold,
        grimm_juvenile_T0,
        grimm_juvenile_T1,
        grimm_juvenile_T2,
        grimm_juvenile_T3,
        grimm_T_min,
        grimm_T_opt,
        grimm_N_min_astro,
        grimm_N_opt_astro,
        weather_data,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose
    )

    # Get flowering info
    astronomical_result <- get_grimm_flowering_info(
        astronomical_result,
        doy_summer_begin,
        doy_summer_end
    )

    # Extract important variables from both
    time_variables <- c(
        "year",
        "doy",
        "hour",
        "doy_dbl",
        "summer_T_avg"
    )

    data_variables <- c(
        "grimm_flowering",
        "grimm_rate",
        "grimm_physiological_age",
        "grimm_rate_temperature",
        "grimm_rate_photoperiod",
        "temp",
        "day_length",
        "flowering_date"
    )

    clock_extract <- EdR.extract(
        clock_result,
        time_variables,
        data_variables
    )

    astronomical_extract <- EdR.extract(
        astronomical_result,
        time_variables,
        data_variables
    )

    # Combine the results
    combined_result <- EdR.merge(
        list(clock_extract, astronomical_extract),
        list("clock", "celestial_mechanics"),
        "day_length_source"
    )

    return(combined_result)
}

## Define a function that extracts some info from the result of
## 'compare_oscillator_to_astronomical'
extract_flowering_dates <- function(comp_result) {
    # Get the flowering date using the clock
    clock_flowering_date <-
        comp_result[which(comp_result$day_length_source=="clock" &
            comp_result$value_type=="flowering_date"),c("value")][1]

    # Get the flowering date using the celestial calculations
    celestial_flowering_date <-
        comp_result[which(comp_result$day_length_source=="celestial_mechanics" &
            comp_result$value_type=="flowering_date"),c("value")][1]

    # Get the average summertime temperature
    summer_T_avg <- comp_result$summer_T_avg[1]

    return(as.data.frame(list(
        clock_flowering_date=clock_flowering_date,
        celestial_flowering_date=celestial_flowering_date,
        summer_T_avg=summer_T_avg
    )))
}

## Define a function that runs 'compare_oscillator_to_astronomical' for all
## years in a weather data set, assuming a particular sowing date
do_comparison_all_years <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_sowing_doy,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min_astro,
    grimm_N_opt_astro,
    grimm_N_min_clock,
    grimm_N_opt_clock,
    doy_summer_begin,
    doy_summer_end,
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
        years <- 1990:2019
    }
    else {
        years <- 1995:2019
    }

    # Get the corresponding weather data
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
    oscillator_comparison <- compare_oscillator_to_astronomical(
        latitude,
        longitude,
        sunrise_threshold_angle,
        kick_strength,
        clock_gamma,
        clock_r0,
        clock_period,
        dawn_phase_initial,
        dusk_phase_initial,
        light_threshold,
        light_exp_at_zero,
        tracker_rate,
        tbase,
        topt,
        tmax,
        grimm_sowing_doy,
        grimm_juvenile_pd_threshold,
        grimm_flowering_threshold,
        grimm_juvenile_T0,
        grimm_juvenile_T1,
        grimm_juvenile_T2,
        grimm_juvenile_T3,
        grimm_T_min,
        grimm_T_opt,
        grimm_N_min_astro,
        grimm_N_opt_astro,
        grimm_N_min_clock,
        grimm_N_opt_clock,
        doy_summer_begin,
        doy_summer_end,
        biocro_weather_data,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose
    )

    # Extract the flowering dates
    flowering_dates <- extract_flowering_dates(oscillator_comparison)

    # Go through the remaining years
    for (i in 2:length(years)) {
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

        # Run the simulation
        temp_oscillator_comparison <- compare_oscillator_to_astronomical(
            latitude,
            longitude,
            sunrise_threshold_angle,
            kick_strength,
            clock_gamma,
            clock_r0,
            clock_period,
            dawn_phase_initial,
            dusk_phase_initial,
            light_threshold,
            light_exp_at_zero,
            tracker_rate,
            tbase,
            topt,
            tmax,
            grimm_sowing_doy,
            grimm_juvenile_pd_threshold,
            grimm_flowering_threshold,
            grimm_juvenile_T0,
            grimm_juvenile_T1,
            grimm_juvenile_T2,
            grimm_juvenile_T3,
            grimm_T_min,
            grimm_T_opt,
            grimm_N_min_astro,
            grimm_N_opt_astro,
            grimm_N_min_clock,
            grimm_N_opt_clock,
            doy_summer_begin,
            doy_summer_end,
            biocro_weather_data,
            solver_method,
            output_step_size,
            adaptive_error_tol,
            adaptive_max_steps,
            verbose
        )

        # Extract the flowering dates
        temp_flowering_dates <-
            extract_flowering_dates(temp_oscillator_comparison)

        # Store
        oscillator_comparison <- rbind(
            oscillator_comparison,
            temp_oscillator_comparison
        )

        flowering_dates <- rbind(
            flowering_dates,
            temp_flowering_dates
        )
    }

    # Add a year column to the flowering dates
    flowering_dates$year <- years

    # Add sowing date columns to both objects
    oscillator_comparison$sowing_doy <- grimm_sowing_doy
    flowering_dates$sowing_doy <- grimm_sowing_doy

    # Return
    return(list(
        oscillator_comparison=oscillator_comparison,
        flowering_dates=flowering_dates
    ))
}

## Define a function that calculates flowering dates using two different methods
## and multiple sowing dates for each year in a weather data set
test_grimm_sowing_dates <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min_astro,
    grimm_N_opt_astro,
    grimm_N_min_clock,
    grimm_N_opt_clock,
    doy_summer_begin,
    doy_summer_end,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    WARM,
    sowing_dates
)
{
    # Run the comparison for all years using the first sowing date
    big_comparison_list <- do_comparison_all_years(
        latitude,
        longitude,
        sunrise_threshold_angle,
        kick_strength,
        clock_gamma,
        clock_r0,
        clock_period,
        dawn_phase_initial,
        dusk_phase_initial,
        light_threshold,
        light_exp_at_zero,
        tracker_rate,
        tbase,
        topt,
        tmax,
        sowing_dates[1],
        grimm_juvenile_pd_threshold,
        grimm_flowering_threshold,
        grimm_juvenile_T0,
        grimm_juvenile_T1,
        grimm_juvenile_T2,
        grimm_juvenile_T3,
        grimm_T_min,
        grimm_T_opt,
        grimm_N_min_astro,
        grimm_N_opt_astro,
        grimm_N_min_clock,
        grimm_N_opt_clock,
        doy_summer_begin,
        doy_summer_end,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose,
        WARM
    )

    # Add the results for subsequent years
    for (i in 2:length(sowing_dates)) {
        temp_list <- do_comparison_all_years(
            latitude,
            longitude,
            sunrise_threshold_angle,
            kick_strength,
            clock_gamma,
            clock_r0,
            clock_period,
            dawn_phase_initial,
            dusk_phase_initial,
            light_threshold,
            light_exp_at_zero,
            tracker_rate,
            tbase,
            topt,
            tmax,
            sowing_dates[i],
            grimm_juvenile_pd_threshold,
            grimm_flowering_threshold,
            grimm_juvenile_T0,
            grimm_juvenile_T1,
            grimm_juvenile_T2,
            grimm_juvenile_T3,
            grimm_T_min,
            grimm_T_opt,
            grimm_N_min_astro,
            grimm_N_opt_astro,
            grimm_N_min_clock,
            grimm_N_opt_clock,
            doy_summer_begin,
            doy_summer_end,
            solver_method,
            output_step_size,
            adaptive_error_tol,
            adaptive_max_steps,
            verbose,
            WARM
        )

        big_comparison_list$flowering_dates <- rbind(
            big_comparison_list$flowering_dates,
            temp_list$flowering_dates
        )

        big_comparison_list$oscillator_comparison <- rbind(
            big_comparison_list$oscillator_comparison,
            temp_list$oscillator_comparison
        )
    }

    # Return
    return(big_comparison_list)
}

## Define a function that tests different values of the GRIMM_N_X parameters for
## one year of weather data and multiple sowing dates
test_grimm_sowing_dates_model_params <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min_astro,
    grimm_N_opt_astro,
    doy_summer_begin,
    doy_summer_end,
    weather_data,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    grimm_N_min_vec,
    grimm_N_opt_vec,
    sowing_date_vec
)
{
    # Make the output vectors
    total_length <- length(grimm_N_min_vec) *
                    length(grimm_N_opt_vec) *
                    length(sowing_date_vec)
    grimm_N_min <- numeric(total_length)
    grimm_N_opt <- numeric(total_length)
    sowing_date <- numeric(total_length)
    difference <- numeric(total_length)

    # Go through all the combinations of parameter values
    num = 1
    for (sow_doy in sowing_date_vec) {
        for (n_min in grimm_N_min_vec) {
            for (n_opt in grimm_N_opt_vec) {
                # Compare the two day length methods
                comparison <- compare_oscillator_to_astronomical(
                    latitude,
                    longitude,
                    sunrise_threshold_angle,
                    kick_strength,
                    clock_gamma,
                    clock_r0,
                    clock_period,
                    dawn_phase_initial,
                    dusk_phase_initial,
                    light_threshold,
                    light_exp_at_zero,
                    tracker_rate,
                    tbase,
                    topt,
                    tmax,
                    sow_doy,
                    grimm_juvenile_pd_threshold,
                    grimm_flowering_threshold,
                    grimm_juvenile_T0,
                    grimm_juvenile_T1,
                    grimm_juvenile_T2,
                    grimm_juvenile_T3,
                    grimm_T_min,
                    grimm_T_opt,
                    grimm_N_min_astro,
                    grimm_N_opt_astro,
                    n_min,
                    n_opt,
                    doy_summer_begin,
                    doy_summer_end,
                    weather_data,
                    solver_method,
                    output_step_size,
                    adaptive_error_tol,
                    adaptive_max_steps,
                    verbose
                )

                # Get the clock and celestial flowering date
                oscillator_fd <- comparison[which(
                    comparison$value_type=="flowering_date" &
                        comparison$day_length_source=="clock"),
                            c("value")][1]
                astronomy_fd <- comparison[which(
                    comparison$value_type=="flowering_date" &
                        comparison$day_length_source=="celestial_mechanics"),
                            c("value")][1]

                # Store the result
                grimm_N_min[num] = n_min
                grimm_N_opt[num] = n_opt
                sowing_date[num] = sow_doy
                difference[num] = oscillator_fd - astronomy_fd

                # Update the index
                num <- num + 1
            }
        }
    }

    # Get the year
    year <- weather_data$year[1]

    return(data.frame(
        year,
        sowing_date,
        grimm_N_min,
        grimm_N_opt,
        difference
    ))

}

## Define a function that applies 'test_grimm_sowing_dates_model_params' to all
## years in a weather data set
test_grimm_sowing_dates_model_params_all_years <- function(
    latitude,
    longitude,
    sunrise_threshold_angle,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    tbase,
    topt,
    tmax,
    grimm_juvenile_pd_threshold,
    grimm_flowering_threshold,
    grimm_juvenile_T0,
    grimm_juvenile_T1,
    grimm_juvenile_T2,
    grimm_juvenile_T3,
    grimm_T_min,
    grimm_T_opt,
    grimm_N_min_astro,
    grimm_N_opt_astro,
    doy_summer_begin,
    doy_summer_end,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    grimm_N_min_vec,
    grimm_N_opt_vec,
    sowing_date_vec,
    WARM
)
{
    # Make the list of years
    if (WARM) {
        years <- 1990:2019
    }
    else {
        years <- 1995:2019
    }

    # Get the corresponding weather data
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

    # Run the test
    result <- test_grimm_sowing_dates_model_params(
        latitude,
        longitude,
        sunrise_threshold_angle,
        kick_strength,
        clock_gamma,
        clock_r0,
        clock_period,
        dawn_phase_initial,
        dusk_phase_initial,
        light_threshold,
        light_exp_at_zero,
        tracker_rate,
        tbase,
        topt,
        tmax,
        grimm_juvenile_pd_threshold,
        grimm_flowering_threshold,
        grimm_juvenile_T0,
        grimm_juvenile_T1,
        grimm_juvenile_T2,
        grimm_juvenile_T3,
        grimm_T_min,
        grimm_T_opt,
        grimm_N_min_astro,
        grimm_N_opt_astro,
        doy_summer_begin,
        doy_summer_end,
        biocro_weather_data,
        solver_method,
        output_step_size,
        adaptive_error_tol,
        adaptive_max_steps,
        verbose,
        grimm_N_min_vec,
        grimm_N_opt_vec,
        sowing_date_vec
    )

    # Run the test for the remaining years
    for (i in 2:length(years)) {
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

        # Run the test
        temp_result <- test_grimm_sowing_dates_model_params(
            latitude,
            longitude,
            sunrise_threshold_angle,
            kick_strength,
            clock_gamma,
            clock_r0,
            clock_period,
            dawn_phase_initial,
            dusk_phase_initial,
            light_threshold,
            light_exp_at_zero,
            tracker_rate,
            tbase,
            topt,
            tmax,
            grimm_juvenile_pd_threshold,
            grimm_flowering_threshold,
            grimm_juvenile_T0,
            grimm_juvenile_T1,
            grimm_juvenile_T2,
            grimm_juvenile_T3,
            grimm_T_min,
            grimm_T_opt,
            grimm_N_min_astro,
            grimm_N_opt_astro,
            doy_summer_begin,
            doy_summer_end,
            biocro_weather_data,
            solver_method,
            output_step_size,
            adaptive_error_tol,
            adaptive_max_steps,
            verbose,
            grimm_N_min_vec,
            grimm_N_opt_vec,
            sowing_date_vec
        )

        result <- rbind(result, temp_result)
    }

    return(result)
}

## Define a function that calculates a total error value for each combinations
## of CROPGRO parameters included in the output of a call to the
## 'test_grimm_sowing_dates_model_params_all_years' function
test_grimm_sowing_dates_model_params_analyze <- function(combo_result) {
    # Get the unique values of grimm_N_min and grimm_N_opt
    grimm_N_min_vec <- unique(combo_result$grimm_N_min)
    grimm_N_opt_vec <- unique(combo_result$grimm_N_opt)

    # Make vectors to store the results of the analysis
    grimm_N_min <- numeric(length(grimm_N_min_vec) * length(grimm_N_opt_vec))
    grimm_N_opt <- grimm_N_min
    error <- grimm_N_min

    # Go through the data
    num = 1
    for (nm in grimm_N_min_vec) {
        for (no in grimm_N_opt_vec) {
            # Get the differences
            differences <- combo_result[which(combo_result$grimm_N_min==nm &
                combo_result$grimm_N_opt==no), c("difference")]

            # Get the squared differences
            squared_differences <- differences * differences

            # Store the results
            grimm_N_min[num] = nm
            grimm_N_opt[num] = no
            error[num] = sum(squared_differences)

            # Increment the counter
            num <- num + 1
        }
    }

    return(data.frame(grimm_N_min, grimm_N_opt, error))
}
