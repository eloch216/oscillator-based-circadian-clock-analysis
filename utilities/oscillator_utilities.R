## Define some functions for testing oscillators (not as part of a clock)

library(BioCro)

## Define a function that runs an oscillator driven by a sinusoidal kick
run_oscillator <- function(
    oscillator_deriv_module,
    kick_strength,
    kick_start,
    kick_end,
    clock_gamma,
    clock_r0,
    clock_period,
    num_doy_to_run,
    solver_method,
    output_step_size,
    adaptive_error_tol,
    adaptive_max_steps,
    verbose
)
{

    # Define the system inputs

    oscillator_ss_modules <- c("oscillator_clock_calculator_sk")

    oscillator_deriv_modules <- c(oscillator_deriv_module)

    oscillator_initial_state <- list(
        dawn_b = 0,
        dawn_a = 0.5 * clock_r0,
        dusk_b = 1e-2,
        dusk_a = 1e-2,
        ref_b = 0.0,
        ref_a = 1.0
    )

    oscillator_parameters <- list(
        timestep = 1.0,
        kick_strength = kick_strength,
        kick_start = kick_start,
        kick_end = kick_end,
        clock_gamma = clock_gamma,
        clock_r0 = clock_r0,
        clock_period = clock_period
    )

    oscillator_solver <- list(
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
        hour = hour,
        temp = hour
    )

    # Run the simulation
    result <- Gro_solver(
        oscillator_initial_state,
        oscillator_parameters,
        weather_data,
        oscillator_ss_modules,
        oscillator_deriv_modules,
        oscillator_solver,
        verbose
    )

    # Calculate an alternate version of the dawn phase that isn't confined to
    # [0, 2 * pi), expressed in units of pi
    result$dawn_phase_2 <- result$dawn_phase
    for (i in 2:length(result$dawn_phase_2)) {
        result$dawn_phase_2[i] <- result$dawn_phase_2[i-1] +
            (result$dawn_phase[i] - result$dawn_phase[i-1]) +
            2 * pi * ((result$dawn_phase[i] - result$dawn_phase[i-1]) < 0)
    }
    result$dawn_phase_2 <- result$dawn_phase_2 / pi

    # Return the result
    return(result)
}
