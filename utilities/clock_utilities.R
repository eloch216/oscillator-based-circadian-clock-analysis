## Define functions for running an oscillator clock

library(BioCro)

## Define a function that runs a poincare clock for an input weather dataframe
run_biocro_clock <- function(
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    light_exp_at_zero,
    tracker_rate,
    weather_data,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose
)
{
    # Define the system inputs

    poincare_clock_ss_modules <- c(
        "light_from_solar",
        "oscillator_clock_calculator"
    )

    poincare_clock_deriv_modules <- c(
        "night_and_day_trackers",
        "poincare_clock"
    )

    poincare_clock_initial_state <- list(
        dawn_b = clock_r0 * sin(dawn_phase_initial * pi / 180),
        dawn_a = clock_r0 * cos(dawn_phase_initial * pi / 180),
        dusk_b = clock_r0 * sin(dusk_phase_initial * pi / 180),
        dusk_a = clock_r0 * cos(dusk_phase_initial * pi / 180),
        ref_b = 0.0,
        ref_a = 1.0,
        night_tracker = 1.0,
        day_tracker = 0.0
    )

    poincare_clock_parameters <- list(
        timestep = 1.0,
        kick_strength = kick_strength,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period,
        light_threshold = light_threshold,
        light_exp_at_zero = light_exp_at_zero,
        tracker_rate = tracker_rate
    )

    poincare_solver <- list(
        type = solver_method,
        output_step_size = output_step_size,
        adaptive_rel_error_tol = adaptive_error_tol,
        adaptive_abs_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps
    )

    # Run the simulation
    result <- Gro_solver(
        poincare_clock_initial_state,
        poincare_clock_parameters,
        weather_data,
        poincare_clock_ss_modules,
        poincare_clock_deriv_modules,
        poincare_solver,
        verbose
    )

    # Return the result
    return(result)
}

## Define a function that runs a poincare clock using a Gaussian solar profile
run_biocro_clock_gaussian <- function(
    new_dl,
    target_doy_dbl,
    num_doy_to_run,
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
    verbose
)
{
    # Define the system inputs

    poincare_clock_ss_modules <- c(
        "gaussian_solar",
        "light_from_solar",
        "oscillator_clock_calculator"
    )

    poincare_clock_deriv_modules <- c(
        "night_and_day_trackers",
        "poincare_clock"
    )

    poincare_clock_initial_state <- list(
        dawn_b = clock_r0 * sin(dawn_phase_initial * pi / 180),
        dawn_a = clock_r0 * cos(dawn_phase_initial * pi / 180),
        dusk_b = clock_r0 * sin(dusk_phase_initial * pi / 180),
        dusk_a = clock_r0 * cos(dusk_phase_initial * pi / 180),
        ref_b = 0.0,
        ref_a = 1.0,
        night_tracker = 1.0,
        day_tracker = 0.0
    )

    poincare_clock_parameters <- list(
        timestep = 1.0,
        kick_strength = kick_strength,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period,
        light_threshold = light_threshold,
        light_exp_at_zero = light_exp_at_zero,
        tracker_rate = tracker_rate,
        target_doy_dbl = target_doy_dbl,
        new_dl = new_dl
    )

    poincare_solver <- list(
        type = solver_method,
        output_step_size = output_step_size,
        adaptive_rel_error_tol = adaptive_error_tol,
        adaptive_abs_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps
    )

    hour_seq <- seq(
        from = 0,
        to = num_doy_to_run * 24 - 1,
        by = 1
    )
    
    doy <- floor(hour_seq / 24.0)
    
    hour <- hour_seq - doy * 24.0
    
    weather_data <- list(
        doy = doy,
        hour = hour
    )

    # Run the simulation
    result <- Gro_solver(
        poincare_clock_initial_state,
        poincare_clock_parameters,
        weather_data,
        poincare_clock_ss_modules,
        poincare_clock_deriv_modules,
        poincare_solver,
        verbose
    )

    # Return the result
    return(result)
}