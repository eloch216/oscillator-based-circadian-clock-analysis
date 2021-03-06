## Run some functions to get info about the light coupling in the clock model.
## This script produces figures 1b, 3a, and 3b

library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/weather_utilities.R")
source("utilities/clock_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
SAVE_TO_FILE <- TRUE

## Choose some settings to be used just for these calculations
COUPLING_STEP_SIZE <- 0.05
COUPLING_WEATHER_DATA <- convert_to_biocro_warm(warm2019)
COUPLING_DAY_ZEITGEBER <- 185
COUPLING_DAY_PHASE <- 200

## Run the clock and plot the light signal, trackers, coupling signal, etc for
## one day of the year
{
    # Either run calculations or load a previously stored result
    if (DO_NEW_CALCULATIONS) {
        # Run the clock using WARM data from 2019 and a small output step size
        hires_warm_clock <- run_biocro_clock(
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            weather_data = COUPLING_WEATHER_DATA,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = COUPLING_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE
        )

        # Add new columns for rescaled solar values
        hires_warm_clock$solar_norm <- hires_warm_clock$solar / 400
        hires_warm_clock$solar_norm2 <- hires_warm_clock$solar / 2000

        # Add a new column for 1-L
        hires_warm_clock$light_inverse <- 1.0 - hires_warm_clock$light

        # Save the result, since this simulation may take a long time to run and
        # it would be nice if we don't need to redo it
        save(hires_warm_clock, file=paste0(DATA_DIR, "/figure_3.RData"))
    }
    else {
        load(paste0(DATA_DIR, "/figure_3.RData"))
    }
}

day_subset_zeitgeber <- hires_warm_clock[which(
    hires_warm_clock$doy_dbl >= COUPLING_DAY_ZEITGEBER &
        hires_warm_clock$doy_dbl < COUPLING_DAY_ZEITGEBER + 1),]

day_subset_phase <- hires_warm_clock[which(
    hires_warm_clock$doy_dbl >= COUPLING_DAY_PHASE &
        hires_warm_clock$doy_dbl < COUPLING_DAY_PHASE + 1),]

# Plot dawn zeitgeber info for one day of WARM data
z_dawn_plot <- xyplot(
    light + night_tracker + dawn_kick + solar_norm2 ~ hour,
    data = day_subset_zeitgeber,
    type = 'l',
    auto = TRUE,
    xlim = c(0, 24),
    ylim = c(-0.1, 1.1),
    xlab = "Time (hr)",
    ylab = "Clock component state (arb. units)",
    scales=list(x=list(at=seq(from=0, to=24, by=6)))
)

EdR.plot(
    z_dawn_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_3a.pdf"),
    width = 6,
    height = 6
)

# Plot dusk zeitgeber info for one day of WARM data
z_dusk_plot <- xyplot(
    light_inverse + day_tracker + dusk_kick + solar_norm2 ~ hour,
    data = day_subset_zeitgeber,
    type = 'l',
    auto = TRUE,
    xlim = c(0, 24),
    ylim = c(-0.1, 1.1),
    xlab = "Time (hr)",
    ylab = "Clock component state (arb. units)",
    scales=list(x=list(at=seq(from=0, to=24, by=6)))
)

EdR.plot(
    z_dusk_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_3b.pdf"),
    width = 6,
    height = 6
)

# Plot phase info for one day of WARM data
phase_plot <- xyplot(
    dawn_phase + dusk_phase + solar_norm ~ hour,
    data = day_subset_phase,
    type = 'l',
    auto = TRUE,
    xlab = "Time (hr)",
    ylab = "Oscillator phase (rad)",
    xlim = c(0, 24),
    ylim = c(0, 2 * pi),
    scales=list(x=list(at=seq(from=0, to=24, by=6)))
)

EdR.plot(
    phase_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_1b.pdf"),
    width=6,
    height=6
)
