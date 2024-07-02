## Define functions for testing the clock's response to changes in initial
## conditions or day length

source("utilities/clock_utilities.R")

library(BioCro)

source('check_biocro_installation.R')

## Define a function that runs a poincare clock with different initial
## conditions using a gaussian solar profile
run_clock_initial_conditions_gaussian <- function(
    num_doy_to_run,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dusk_phase_initial,
    light_threshold,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    num_ic,
    light_exp_at_zero,
    tracker_rate
)
{
    # Make a list of initial dawn oscillator phase angles
    phase_angles <- seq(
        from = 0,
        to = 360,
        length.out = floor(num_ic) + 1
    )

    # Get the simulation result for the first angle
    result <- run_biocro_clock_gaussian(
        new_dl = 16.0,  # this value doesn't matter
        target_doy_dbl = num_doy_to_run + 1,
        num_doy_to_run = num_doy_to_run,
        kick_strength = kick_strength,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period,
        dawn_phase_initial = phase_angles[1],
        dusk_phase_initial = dusk_phase_initial,
        light_threshold = light_threshold,
        light_exp_at_zero = light_exp_at_zero,
        tracker_rate = tracker_rate,
        solver_method = solver_method,
        output_step_size = output_step_size,
        adaptive_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps,
        verbose = verbose
    )

    # Add a new column indicating the initial phase angle
    result$initial_phase <- phase_angles[1]

    # Go through the rest of the angles
    for(i in 2:num_ic) {
        simresult <- run_biocro_clock_gaussian(
            new_dl = 16.0,  # this value doesn't matter
            target_doy_dbl = num_doy_to_run + 1,
            num_doy_to_run = num_doy_to_run,
            kick_strength = kick_strength,
            clock_gamma = clock_gamma,
            clock_r0 = clock_r0,
            clock_period = clock_period,
            dawn_phase_initial = phase_angles[i],
            dusk_phase_initial = dusk_phase_initial,
            light_threshold = light_threshold,
            light_exp_at_zero = light_exp_at_zero,
            tracker_rate = tracker_rate,
            solver_method = solver_method,
            output_step_size = output_step_size,
            adaptive_error_tol = adaptive_error_tol,
            adaptive_max_steps = adaptive_max_steps,
            verbose = verbose
        )

        simresult$initial_phase <- phase_angles[i]

        result <- rbind(result, simresult)
    }

    # Return the result
    return(result)
}

## Define a function that runs a poincare clock with a suddent change in
## photoperiod length using a Gaussian solar profile
run_clock_jumps_gaussian <- function(
    target_doy_dbl,
    num_doy_to_run,
    kick_strength,
    clock_gamma,
    clock_r0,
    clock_period,
    dawn_phase_initial,
    dusk_phase_initial,
    light_threshold,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose,
    num_jumps,
    light_exp_at_zero,
    tracker_rate
)
{
    # Make a list of initial day length values
    new_dl <- seq(
        from = 11.0,
        to = 18.0,
        length.out = num_jumps
    )

    # Get the simulation result for the first day length
    result <- run_biocro_clock_gaussian(
        new_dl = new_dl[1],
        target_doy_dbl = target_doy_dbl,
        num_doy_to_run = num_doy_to_run,
        kick_strength = kick_strength,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period,
        dawn_phase_initial = dawn_phase_initial,
        dusk_phase_initial = dusk_phase_initial,
        light_threshold = light_threshold,
        light_exp_at_zero = light_exp_at_zero,
        tracker_rate = tracker_rate,
        solver_method = solver_method,
        output_step_size = output_step_size,
        adaptive_error_tol = adaptive_error_tol,
        adaptive_max_steps = adaptive_max_steps,
        verbose = verbose
    )

    # Add a new column indicating the final day length
    result$new_dl <- new_dl[1]

    # Go through the rest of the day lengths
    for(i in 2:length(new_dl)) {
        simresult <- run_biocro_clock_gaussian(
            new_dl = new_dl[i],
            target_doy_dbl = target_doy_dbl,
            num_doy_to_run = num_doy_to_run,
            kick_strength = kick_strength,
            clock_gamma = clock_gamma,
            clock_r0 = clock_r0,
            clock_period = clock_period,
            dawn_phase_initial = dawn_phase_initial,
            dusk_phase_initial = dusk_phase_initial,
            light_threshold = light_threshold,
            light_exp_at_zero = light_exp_at_zero,
            tracker_rate = tracker_rate,
            solver_method = solver_method,
            output_step_size = output_step_size,
            adaptive_error_tol = adaptive_error_tol,
            adaptive_max_steps = adaptive_max_steps,
            verbose = verbose
        )

        simresult$new_dl <- new_dl[i]

        result <- rbind(result, simresult)
    }

    # Return the result
    return(result)
}
