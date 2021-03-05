## Run some functions to get info about the clock's accuracy and reliability.
## This script produces figures 1c, 5a, 5b, s6, s7, and s8

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/accuracy_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
SAVE_TO_FILE <- TRUE

{
    if (DO_NEW_CALCULATIONS) {
        # Run the clocks for both types of weather data
        result_surfrad <- run_clock_all_years(
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = FALSE
        )
        
        result_warm <- run_clock_all_years(
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = TRUE
        )
        
        # Get some stats about the clock accuracy
        surfrad_accuracy <- analyze_oscillator_stats(
            result_surfrad,
            DEF_LONGITUDE,
            DEF_LATITUDE,
            DEF_SUNRISE_THRESHOLD_ANGLE
        )
        
        # Exclude 1989 from the WARM analysis since we don't have the beginning of
        # that year
        warm_accuracy <- analyze_oscillator_stats(
            result_warm[which(result_warm$year!=1989),],
            DEF_LONGITUDE,
            DEF_LATITUDE,
            DEF_SUNRISE_THRESHOLD_ANGLE
        )
        
        # Combine the accuracy results
        total_accuracy <- EdR.merge(
            list(surfrad_accuracy, warm_accuracy),
            list("SURFRAD", "WARM"),
            "data_source"
        )
        
        # Get stats excluding 2015 from the SURFRAD set, since it seems to beginning
        # "weird"
        surfrad_accuracy_no_2015 <- analyze_oscillator_stats(
            result_surfrad[which(result_surfrad$year!=2015),],
            DEF_LONGITUDE,
            DEF_LATITUDE,
            DEF_SUNRISE_THRESHOLD_ANGLE
        )
        
        # Calculate the day length in 2019 using celestial mechanics
        calculated_2019 <- calculate_day_length(
            2019,
            DEF_LONGITUDE,
            DEF_LATITUDE,
            DEF_SUNRISE_THRESHOLD_ANGLE
        )
        
        save(
            result_surfrad,
            result_warm,
            surfrad_accuracy,
            warm_accuracy,
            total_accuracy,
            surfrad_accuracy_no_2015,
            calculated_2019,
            file=paste0(DATA_DIR, "/clock_accuracy.RData")
        )
    }
    else {
        load(file=paste0(DATA_DIR, "/clock_accuracy.RData"))
    }
}

## Produce Figure 1c

sunrise_plot <- xyplot(
    day_length + sunrise + sunset ~ doy_dbl,
    data=result_warm[which(result_warm$year==2019),],
    type='l',
    auto=TRUE,
    xlim=c(1,365),
    ylim=c(0,24),
    xlab="Day of year",
    ylab="Hour",
    main="WARM"
)

EdR.plot(
    sunrise_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_1c_warm.pdf"),
    width=6,
    height=6
)


calculated_sunrise_plot <- xyplot(
    day_length + sunrise + sunset ~ doy,
    data=calculated_2019,
    type='l',
    auto=TRUE,
    xlim=c(1,365),
    ylim=c(0,24),
    xlab="Day of year",
    ylab="Hour",
    main="Celestial mechanics"
)

EdR.plot(
    calculated_sunrise_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_1c_celestial.pdf"),
    width=6,
    height=6
)

## Produce Figure 5a

ylab_5a <- paste0(
    "Oscillator photoperiod length - celestial photoperiod length",
    "\n",
    "averaged over all years (hours)"
)

diff_plot <- xyplot(
    day_length_diff_average ~ doy,
    group=factor(data_source),
    data=total_accuracy,
    type='l',
    auto=TRUE,
    ylim=c(-1.5,1.5),
    xlim=c(1,366),
    grid=TRUE,
    xlab="Day of year",
    ylab=ylab_5a
)

EdR.plot(
    diff_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_5a.pdf"),
    width=6,
    height=6
)

## Produce Figure 5b

day_length_fivnenum_warm_plot <- xyplot(
    day_length_f1 + day_length_f2 + day_length_f3 + day_length_f4 +
        day_length_f5 + day_length_calc_average ~ doy,
    data=total_accuracy[which(total_accuracy$data_source=="WARM"),],
    type='l',
    auto=TRUE,
    xlim=c(1,366),
    ylim=c(6,16),
    xlab="Day of year",
    ylab="Five number summary for day length (hours)"
)

EdR.plot(
    day_length_fivnenum_warm_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_5b.pdf"),
    width=6,
    height=6
)


## Produce Figure s6

day_length_fivnenum_plot <- xyplot(
    day_length_f1 + day_length_f2 + day_length_f3 + day_length_f4 +
        day_length_f5 + day_length_calc_average ~ doy | factor(data_source),
    data=total_accuracy,
    type='l',
    auto=TRUE,
    xlim=c(1,366),
    ylim=c(6,16),
    xlab="Day of year",
    ylab="Five number summary for day length (hours)"
)

EdR.plot(
    day_length_fivnenum_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_s6.pdf"),
    width=12,
    height=6
)

## Produce Figure s7

iqr_plot <- xyplot(
    day_length_IQR ~ doy,
    group=factor(data_source),
    data=total_accuracy,
    type='l',
    auto=TRUE,
    ylim=c(0,1.0),
    xlim=c(1,366),
    grid=TRUE,
    xlab="Day of year",
    ylab="IQR of photoperiod length values\ncalculated by oscillators (hours)"
)

EdR.plot(
    iqr_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_s7.pdf"),
    width=6,
    height=6
)

## Produce Figure s8

surfrad_no_2015_fivnenum_plot <- xyplot(
    day_length_f1 + day_length_f2 + day_length_f3 + day_length_f4 + 
        day_length_f5 + day_length_calc_average ~ doy,
    data=surfrad_accuracy_no_2015,
    type='l',
    auto=TRUE,
    xlim=c(1,366),
    ylim=c(6,16),
    xlab="Day of year",
    ylab="Five number summary for day length (hours)",
    main="SURFRAD excluding 2015"
)

EdR.plot(
    surfrad_no_2015_fivnenum_plot,
    SAVE_TO_FILE,
    paste0(FIGURE_DIR, "/figure_s8.pdf"),
    width=6,
    height=6
)
