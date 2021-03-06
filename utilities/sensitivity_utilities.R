## Define some functions for sensitivity analysis

get_change <- function(
    initial_state,
    parameters,
    varying_parameters,
    steady_state_module_names,
    derivative_module_names,
    solver,
    verbose,
    x_name,
    x_values,   # should have three values: central, negative perturbation, and positive perturbation
    y_name
)
{
    # Use partial_gro_solver to make a function that just takes a value of
    # `x_name` as an input
    partial_func <- partial_gro_solver(
        initial_state,
        parameters,
        varying_parameters,
        steady_state_module_names,
        derivative_module_names,
        arg_names = c(x_name),
        solver,
        verbose
    )

    # Run a simulation with the central value of `x_name`
    result_central <- partial_func(c(x_values[1]))

    # Extract the column that represents `y_name`
    y_result_central <- result_central[[y_name]]

    # Get the time
    doy_dbl <- result_central[['doy_dbl']]

    # Run a simulation with the negative perturbation value of `x_name`
    result_neg <- partial_func(c(x_values[2]))

    # Extract the column that represents `y_name`
    y_result_neg <- result_neg[[y_name]]

    # Run a simulation with the positive perturbation value of `y_name`
    result_pos <- partial_func(c(x_values[3]))

    # Extract the column that represents `y_name`
    y_result_pos <- result_pos[[y_name]]

    # Get the change in `y_name`
    y_diff <- y_result_pos - y_result_neg

    # Get dy/dx
    dydx <- y_diff / (x_values[3] - x_values[2])

    # Get normalized dy/dx
    dydx_norm <- dydx / (y_result_central / x_values[1])

    return(data.frame(
        doy_dbl,
        y_result_pos,
        y_result_neg,
        y_diff,
        dydx,
        dydx_norm
    ))
}

get_change_clock_frequency <- function(
    initial_state,
    parameters,
    varying_parameters,
    steady_state_module_names,
    derivative_module_names,
    solver,
    verbose,
    clock_frequency_values,   # should have three values: central, negative perturbation, and positive perturbation
    y_name
)
{
    # Use partial_gro_solver to make a function that just takes a value of
    # `x_name` as an input
    partial_func <- partial_gro_solver(
        initial_state,
        parameters,
        varying_parameters,
        steady_state_module_names,
        derivative_module_names,
        arg_names = c("clock_period"),
        solver,
        verbose
    )

    # Run a simulation with the central value of `x_name`
    result_central <- partial_func(c(2 * pi / clock_frequency_values[1]))

    # Extract the column that represents `y_name`
    y_result_central <- result_central[[y_name]]

    # Get the time
    doy_dbl <- result_central[['doy_dbl']]

    # Run a simulation with the negative perturbation value of `x_name`
    result_neg <- partial_func(c(2 * pi / clock_frequency_values[2]))

    # Extract the column that represents `y_name`
    y_result_neg <- result_neg[[y_name]]

    # Run a simulation with the positive perturbation value of `y_name`
    result_pos <- partial_func(c(2 * pi / clock_frequency_values[3]))

    # Extract the column that represents `y_name`
    y_result_pos <- result_pos[[y_name]]

    # Get the change in `y_name`
    y_diff <- y_result_pos - y_result_neg

    # Get dy/dx
    dydx <- y_diff / (clock_frequency_values[3] - clock_frequency_values[2])

    # Get normalized dy/dx
    dydx_norm <- dydx / (y_result_central / clock_frequency_values[1])

    return(data.frame(
        doy_dbl,
        y_result_pos,
        y_result_neg,
        y_diff,
        dydx,
        dydx_norm
    ))
}
