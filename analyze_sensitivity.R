## Run some functions to analyze the clock's sensitivity to its parameters

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/weather_utilities.R")
source("utilities/sensitivity_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
SAVE_TO_FILE <- TRUE

## Choose some settings to use just for this script
SENSITIVITY_WEATHER_DATA <- convert_to_biocro_warm(warm2019)
SENSITIVITY_PARAMS <- c(
    "kick_strength",
    "clock_gamma",
    "clock_r0",
    "clock_period",
    "light_threshold",
    "tracker_rate"
)
SENSITIVITY_DEFAULTS <- c(
    DEF_KICK_STRENGTH,
    DEF_CLOCK_GAMMA,
    DEF_CLOCK_R0,
    DEF_CLOCK_PERIOD,
    DEF_LIGHT_THRESHOLD,
    DEF_TRACKER_RATE
)
SENSITIVITY_REL_STEP <- 1e-5
SENSITIVITY_Y <- "day_length"

## Perform sensitivity analysis to determine how to photoperiod changes through
## the year in response to perturbations around the default clock parameter
## values
{
    if (DO_NEW_CALCULATIONS) {
        # Set up simulation defaults
        poincare_clock_ss_modules <- c(
            "light_from_solar",
            "oscillator_clock_calculator"
        )

        poincare_clock_deriv_modules <- c(
            "night_and_day_trackers",
            "poincare_clock"
        )

        poincare_clock_initial_state <- list(
            dawn_b = DEF_CLOCK_R0 * sin(DEF_DAWN_PHASE_INITIAL * pi / 180),
            dawn_a = DEF_CLOCK_R0 * cos(DEF_DAWN_PHASE_INITIAL * pi / 180),
            dusk_b = DEF_CLOCK_R0 * sin(DEF_DUSK_PHASE_INITIAL * pi / 180),
            dusk_a = DEF_CLOCK_R0 * cos(DEF_DUSK_PHASE_INITIAL * pi / 180),
            ref_b = 0.0,
            ref_a = 1.0,
            night_tracker = 1.0,
            day_tracker = 0.0
        )

        poincare_clock_parameters <- list(
            timestep = 1.0,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE
        )

        poincare_solver <- list(
            type = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_rel_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_abs_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS
        )

        # Make a helping function
        change_function <- function(index) {
            change_result <- get_change(
                poincare_clock_initial_state,
                poincare_clock_parameters,
                SENSITIVITY_WEATHER_DATA,
                poincare_clock_ss_modules,
                poincare_clock_deriv_modules,
                poincare_solver,
                FALSE,
                x_name = SENSITIVITY_PARAMS[index],
                x_values = c(
                    SENSITIVITY_DEFAULTS[index],
                    SENSITIVITY_DEFAULTS[index] * (1.0 - SENSITIVITY_REL_STEP),
                    SENSITIVITY_DEFAULTS[index] * (1.0 + SENSITIVITY_REL_STEP)
                ),
                y_name = SENSITIVITY_Y
            )

            extracted_change_result <- EdR.extract(
                change_result,
                c("doy_dbl"),
                c(
                    "y_result_pos",
                    "y_result_neg",
                    "y_diff",
                    "dydx",
                    "dydx_norm"
                )
            )

            extracted_change_result[['param_name']] <- SENSITIVITY_PARAMS[index]

            return(extracted_change_result)
        }

        # Calculate the sensitivity of photoperiod length to the first parameter
        sensitivity_result <- change_function(1)

        # Proceed for the others
        for (i in 2:length(SENSITIVITY_PARAMS)) {
            sensitivity_result <- rbind(sensitivity_result, change_function(i))
        }

        # Do a separate version for the clock frequency
        clock_frequency_change <- get_change_clock_frequency(
            poincare_clock_initial_state,
            poincare_clock_parameters,
            SENSITIVITY_WEATHER_DATA,
            poincare_clock_ss_modules,
            poincare_clock_deriv_modules,
            poincare_solver,
            FALSE,
            clock_frequency_values = c(
                2 * pi / DEF_CLOCK_PERIOD,
                2 * pi / DEF_CLOCK_PERIOD * (1.0 - SENSITIVITY_REL_STEP),
                2 * pi / DEF_CLOCK_PERIOD * (1.0 + SENSITIVITY_REL_STEP)
            ),
            y_name = SENSITIVITY_Y
        )

        save(
            sensitivity_result,
            clock_frequency_change,
            file=paste0(DATA_DIR, "/sensitivity.RData")
        )

    }
    else {
        load(file=paste0(DATA_DIR, "/sensitivity.RData"))
    }

    # Set up some colors
	brewer_cols <- brewer.pal(8, "Dark2")
	cols <- c(
        "#000000",
        brewer_cols[1],
        brewer_cols[2],
        brewer_cols[3],
        brewer_cols[4],
        brewer_cols[6],
        brewer_cols[7]
    )

    xyplot(dydx_norm ~ doy_dbl, data=clock_frequency_change, type='l', grid=TRUE)

    # Plot the normalized control coefficient (dy / dx) / (y / x)
    normalized_plot <- xyplot(
        dydx_norm ~ doy_dbl,
        data=clock_frequency_change,
        type='l',
        auto=TRUE,
        grid=TRUE,
        ylab="(d_photoperiod / d_omega_0) / (photoperiod_0 / omega_0)",
        xlab="Day of year",
        xlim=c(0,365),
        ylim=c(-2.5,1.5),
		par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        )
    )

    EdR.plot(
        normalized_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s12_top_left.pdf"),
        width = 6,
        height = 6
    )

    # Plot the normalized control coefficient (dy / dx) / (y / x) for a few days
    normalized_plot_zoom <- xyplot(
        dydx_norm ~ doy_dbl,
        data=clock_frequency_change,
        type='l',
        auto=TRUE,
        grid=TRUE,
        ylab="(d_photoperiod / d_omega_0) / (photoperiod_0 / omega_0)",
        xlab="Day of year",
        xlim=c(200,205),
        ylim=c(-1.25,1.25),
		par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        )
    )

    EdR.plot(
        normalized_plot_zoom,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s12_top_right.pdf"),
        width = 6,
        height = 6
    )

    # Plot the normalized control coefficient (dy / dx) / (y / x), excluding the
    # clock period from the plot
    normalized_plot_no_clock_period <- xyplot(
        value ~ doy_dbl,
        group=param_name,
        data=sensitivity_result[which(sensitivity_result$value_type=="dydx_norm"
            & sensitivity_result$param_name != "clock_period"),],
        type='l',
        auto=TRUE,
        grid=TRUE,
        ylab="(d_photoperiod / d_x) / (photoperiod_0 / x_0)",
        xlab="Day of year",
        xlim=c(0,365),
        ylim=c(-0.25,0.1),
		par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        )
    )

    EdR.plot(
        normalized_plot_no_clock_period,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s12_bottom.pdf"),
        width = 6,
        height = 6
    )

    # Make the inset
    Y_OFFSET <- 0.5
    GAUSSIAN_SIGMA <- 0.2
    GAUSSIAN_MEAN <- 0.5
    GAUSSIAN_SHIFT <- 0.05
    x <- seq(from=0, to=1, length.out=1001)
    y1 <- exp(-(x - GAUSSIAN_MEAN)^2 / GAUSSIAN_SIGMA^2) + Y_OFFSET
    y2 <- exp(-(x - GAUSSIAN_MEAN + GAUSSIAN_SHIFT)^2 / GAUSSIAN_SIGMA^2) +
                                                                        Y_OFFSET
    ydiff <- y2 - y1
    inset_plot <- xyplot(
        y1 + y2 + ydiff ~ x,
        auto=TRUE,
        type='l',
        xlim=c(0.2,0.8),
        ylim=c(-0.5,2),
        grid=TRUE
    )

    EdR.plot(
        inset_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s12_bottom_inset.pdf"),
        width = 6,
        height = 6
    )
}
