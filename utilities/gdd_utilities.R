## Define functions used for comparing the CROPGRO model for soybean development
## against a simpler one based on growing degree days (GDD)

library(BioCro)

source("utilities/cropgro_utilities.R")
source("utilities/weather_utilities.R")
source("utilities/EdR.R")

## Define a function that determines flowering date based on the CROPGRO model
## (using celestial mechanics or an oscillator clock to determine the
## photoperiod length) and using a simple GDD model
get_flowering_date <- function(
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
    verbose,
    astro_grimm,
    TTc_threshold
)
{
    # Run the simulation with either the astronomical day length or the clock day length
    if (astro_grimm) {
        result <- run_astronomical_grimm(
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
    }
    else {
        result <- run_clock_grimm(
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
    }

    # Get the average temperature during the summer
    summer_T_avg <- mean(result[which(
            result$doy >= doy_summer_begin &
                result$doy <= doy_summer_end), c("temp")])

    # Store the sowing date, juvenile stage ending threshold, flowering
    # threshold, TTc threshold, and experimental flowering date
    result$doy_sow <- grimm_sowing_doy
    result$juvenile_threshold <- grimm_juvenile_pd_threshold
    result$flowering_threshold <- grimm_juvenile_pd_threshold + grimm_flowering_threshold
    result$TTc_threshold <- TTc_threshold
    result$summer_T_avg <- summer_T_avg

    # Find TTc at sowing date and store it as another column
    sowing_TTc <- 0.0
    for (i in 1:length(result$doy_dbl)) {
        if (result$doy_dbl[i] >= grimm_sowing_doy) {
            sowing_TTc <- result$TTc[i]
            break
        }
    }
    result$sowing_TTc <- sowing_TTc

    # Find the accumulated thermal time during the plant's life
    result$plant_TTc <- (result$TTc - sowing_TTc) * ((result$TTc - sowing_TTc) > 0)

    # Add indicator columns for sowing, juvenile, and flowering
    result$sowed <- result$doy_dbl >= grimm_sowing_doy
    result$juvenile <- result$grimm_physiological_age >= grimm_juvenile_pd_threshold
    result$flowered <- result$grimm_flowering
    result$flowered_TTc <- result$plant_TTc >= TTc_threshold

    # Find the doy_dbl of first flowering (according to the Grimm model) and
    # store it as another column
    doy_r1 <- 0.0
    for (i in 1:length(result$flowered)) {
        if (result$flowered[i]) {
            doy_r1 <- result$doy_dbl[i]
            break
        }
    }
    result$doy_r1 <- doy_r1

    # Find the doy_dbl of first flowering (according to TTc) and store it as
    # another column
    doy_r1_TTc <- 0.0
    for (i in 1:length(result$flowered_TTc)) {
        if (result$flowered_TTc[i]) {
            doy_r1_TTc <- result$doy_dbl[i]
            break
        }
    }
    result$doy_r1_TTc <- doy_r1_TTc

    # Return the result
    return(result)
}

## Define a function that applies 'get_flowering_date' to all years in a weather
## data set
get_flowering_date_all_years <- function(
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
    astro_grimm,
    TTc_threshold,
    WARM
)
{
    # Make the list of years
    if (WARM) {
        years <- WARM_YEARS
    }
    else {
        years <- SURFRAD_YEARS
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

    # Run the test
    result <- get_flowering_date(
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
        verbose,
        astro_grimm,
        TTc_threshold
    )

    # Run the test for the remaining years
    for (i in 2:length(years)) {
        # Get the weather data
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
        temp_result <- get_flowering_date(
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
            verbose,
            astro_grimm,
            TTc_threshold
        )

        # Add it
        result <- rbind(result, temp_result)
    }

    return(result)
}

## Define a function that extracts flowering information from a multi-year data
## set
extract_flowering_dates <- function(all_year_result) {
    # Get the unique year values
    years <- unique(all_year_result$year)

    # Define columns to extract
    columns_to_get <- c(
        "year",
        "doy_sow",
        "TTc_threshold",
        "doy_r1",
        "doy_r1_TTc",
        "summer_T_avg"
    )

    # Get the results from the first year
    result <- all_year_result[which(
        all_year_result$year==years[1]),columns_to_get][1,]

    # Add the results from the remaining years
    for (i in 2:length(years)) {
        temp_result <- all_year_result[which(
            all_year_result$year==years[i]),columns_to_get][1,]

        result <- rbind(result, temp_result)
    }

    # Add a column for the difference
    result$difference <- result$doy_r1_TTc - result$doy_r1

    return(result)
}

## Define a function that compares the Grimm model to a GDD model using several
## sowing dates
comp_grimm_ttc_sowing_dates <- function(
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
    astro_grimm,
    TTc_threshold,
    WARM,
    sowing_dates
)
{
    # Get the results for the first sowing date
    all_year_result <- get_flowering_date_all_years(
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
        astro_grimm,
        TTc_threshold,
        WARM
    )

    flowering_info <- extract_flowering_dates(all_year_result)

    # Add the results for the other sowing dates
    for (i in 2:length(sowing_dates)) {
        all_year_result <- get_flowering_date_all_years(
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
            astro_grimm,
            TTc_threshold,
            WARM
        )

        flowering_info <- rbind(
            flowering_info,
            extract_flowering_dates(all_year_result)
        )
    }

    return(flowering_info)
}

## Define a function that determines an error value from the output of the
## 'comp_grimm_ttc_sowing_dates' function
comp_grimm_ttc_error <- function(flowering_info) {
    squared_differences <- flowering_info$difference * flowering_info$difference
    return(sum(squared_differences))
}

## Define a function that computes errors for a set of GDD threshold values
comp_grimm_ttc_test_thresholds <- function(
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
    astro_grimm,
    TTc_thresholds,
    WARM,
    sowing_dates
)
{
    # Get the errors
    error <- TTc_thresholds
    for (i in 1:length(TTc_thresholds)) {
        sowing_day_results <- comp_grimm_ttc_sowing_dates(
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
            astro_grimm,
            TTc_thresholds[i],
            WARM,
            sowing_dates
        )

        error[i] <- comp_grimm_ttc_error(sowing_day_results)
    }

    return(data.frame(TTc_thresholds, error, stringsAsFactors=FALSE))
}
