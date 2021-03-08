## Run some functions to analyze the CROPGRO model

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/cropgro_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
ANALYZE_CLOCK_VS_CELESTIAL <- TRUE  # produces figures 6a, 6b, and s9
CROSS_VALIDATE_CROPGRO <- TRUE      # produces figure s10
TEST_CROPGRO_PARAMETERS <- TRUE     # produces table s3 (takes a LONG time)
SAVE_TO_FILE <- TRUE

## Define some settings to use just for this script
NUM_PARAM_TO_TEST <- 11
GRIMM_N_MINS_TO_TEST <- seq(
    from = DEF_GRIMM_N_MIN - 2,
    to = DEF_GRIMM_N_MIN + 2,
    length.out = NUM_PARAM_TO_TEST
)
GRIMM_N_OPTS_TO_TEST <- seq(
    from = DEF_GRIMM_N_OPT - 2,
    to = DEF_GRIMM_N_OPT + 2,
    length.out = NUM_PARAM_TO_TEST
)
CROSS_VALIDATION_SOWING_DATES <- DEF_SOWING_DATES - 5.0

## Compare flowering dates calculated using the CROPGRO model with photoperiod
## length determined in two different ways. Here we use N_min and N_opt values
## that were optimized to produce the closest agreement with the default set of
## sowing dates, and calculate the flowering dates corresponding to those sowing
## dates.
if (ANALYZE_CLOCK_VS_CELESTIAL) {
    if (DO_NEW_CALCULATIONS) {
        ## Do the big test for the WARM data set
        grimm_sowing_result_warm <- test_grimm_sowing_dates(
            latitude = DEF_LATITUDE,
            longitude = DEF_LONGITUDE,
            sunrise_threshold_angle = GRIMM_SUNRISE_THRESHOLD_ANGLE,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            tbase = DEF_TBASE,
            topt = DEF_TOPT,
            tmax = DEF_TMAX,
            grimm_juvenile_pd_threshold = DEF_GRIMM_JUVENILE_PD_THRESHOLD,
            grimm_flowering_threshold = DEF_GRIMM_FLOWERING_THRESHOLD,
            grimm_juvenile_T0 = DEF_GRIMM_JUVENILE_T0,
            grimm_juvenile_T1 = DEF_GRIMM_JUVENILE_T1,
            grimm_juvenile_T2 = DEF_GRIMM_JUVENILE_T2,
            grimm_juvenile_T3 = DEF_GRIMM_JUVENILE_T3,
            grimm_T_min = DEF_GRIMM_T_MIN,
            grimm_T_opt = DEF_GRIMM_T_OPT,
            grimm_N_min_astro = DEF_GRIMM_N_MIN,
            grimm_N_opt_astro = DEF_GRIMM_N_OPT,
            grimm_N_min_clock = OSCILLATOR_GRIMM_N_MIN,
            grimm_N_opt_clock = OSCILLATOR_GRIMM_N_OPT,
            doy_summer_begin = DEF_DOY_SUMMER_BEGIN,
            doy_summer_end = DEF_DOY_SUMMER_END,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = TRUE,
            sowing_dates = DEF_SOWING_DATES
        )

        ## Do the big test for the SURFRAD data set
        grimm_sowing_result_surfrad <- test_grimm_sowing_dates(
            latitude = DEF_LATITUDE,
            longitude = DEF_LONGITUDE,
            sunrise_threshold_angle = GRIMM_SUNRISE_THRESHOLD_ANGLE,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            tbase = DEF_TBASE,
            topt = DEF_TOPT,
            tmax = DEF_TMAX,
            grimm_juvenile_pd_threshold = DEF_GRIMM_JUVENILE_PD_THRESHOLD,
            grimm_flowering_threshold = DEF_GRIMM_FLOWERING_THRESHOLD,
            grimm_juvenile_T0 = DEF_GRIMM_JUVENILE_T0,
            grimm_juvenile_T1 = DEF_GRIMM_JUVENILE_T1,
            grimm_juvenile_T2 = DEF_GRIMM_JUVENILE_T2,
            grimm_juvenile_T3 = DEF_GRIMM_JUVENILE_T3,
            grimm_T_min = DEF_GRIMM_T_MIN,
            grimm_T_opt = DEF_GRIMM_T_OPT,
            grimm_N_min_astro = DEF_GRIMM_N_MIN,
            grimm_N_opt_astro = DEF_GRIMM_N_OPT,
            grimm_N_min_clock = OSCILLATOR_GRIMM_N_MIN,
            grimm_N_opt_clock = OSCILLATOR_GRIMM_N_OPT,
            doy_summer_begin = DEF_DOY_SUMMER_BEGIN,
            doy_summer_end = DEF_DOY_SUMMER_END,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = FALSE,
            sowing_dates = DEF_SOWING_DATES
        )

        save(
            grimm_sowing_result_warm,
            grimm_sowing_result_surfrad,
            file=paste0(DATA_DIR, "/cropgro_astro_comparison.RData")
        )
    }
    else {
        load(file=paste0(DATA_DIR, "/cropgro_astro_comparison.RData"))
    }

    # Get the flowering dates
    flowering_dates_warm <- grimm_sowing_result_warm$flowering_dates

    flowering_dates_surfrad <- grimm_sowing_result_surfrad$flowering_dates

    # Compute the difference
    flowering_dates_warm$diff <-
        flowering_dates_warm$clock_flowering_date -
                flowering_dates_warm$celestial_flowering_date

    flowering_dates_surfrad$diff <-
        flowering_dates_surfrad$clock_flowering_date -
            flowering_dates_surfrad$celestial_flowering_date

    # Get statistics about the differences
    warm_mean <- mean(flowering_dates_warm$diff)
    warm_stdev <- sd(flowering_dates_warm$diff)
    warm_stdev_mean <- warm_stdev /
        sqrt(length(flowering_dates_warm$diff))

    surfrad_mean <- mean(flowering_dates_surfrad$diff)
    surfrad_stdev <- sd(flowering_dates_surfrad$diff)
    surfrad_stdev_mean <- surfrad_stdev /
        sqrt(length(flowering_dates_surfrad$diff))

    # Set up some colors
    cols <- brewer.pal(8, "Dark2")

    cols <- c("#000000", cols)

    # Plot the clock date vs celestial date
    clock_v_celestial_plot_warm <- xyplot(
        clock_flowering_date ~ celestial_flowering_date,
        group = factor(sowing_doy),
        data = flowering_dates_warm[order(
            flowering_dates_warm$celestial_flowering_date),],
        type = 'p',
        pch = 20,
        par.settings = list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        auto = TRUE,
        main = "WARM",
        xlim = c(155, 215),
        ylim = c(155, 215),
        xlab = "Flowering day (celestial mechanics)",
        ylab = "Flowering day (oscillator clock)"
    )

    EdR.plot(
        clock_v_celestial_plot_warm,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_6a.pdf"),
        width = 8,
        height = 8
    )

    clock_v_celestial_plot_surfrad <- xyplot(
        clock_flowering_date ~ celestial_flowering_date,
        group = factor(sowing_doy),
        data = flowering_dates_surfrad[order(
            flowering_dates_surfrad$celestial_flowering_date),],
        type = 'p',
        pch = 20,
        par.settings = list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        auto = TRUE,
        main = "SURFRAD",
        xlim = c(155, 215),
        ylim = c(155, 215),
        xlab = "Flowering day (celestial mechanics)",
        ylab = "Flowering day (oscillator clock)"
    )

    EdR.plot(
        clock_v_celestial_plot_surfrad,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_6b.pdf"),
        width = 8,
        height = 8
    )

    # Plot the clock difference against average summer temperature
    warm_caption <- paste0(
        "WARM:\nMean difference = ",
        warm_mean,
        "\nSTDEV = ",
        warm_stdev,
        "\nSTDEV_OF_MEAN = ",
        warm_stdev_mean
    )

    date_difference_plot_warm <- xyplot(
        diff ~ summer_T_avg,
        data=flowering_dates_warm[order(flowering_dates_warm$summer_T_avg),],
        group=factor(sowing_doy),
        type='b',
        pch=20,
        auto=TRUE,
        grid=TRUE,
        par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        ylab="Clock flowering date - celestial flowering date (days)",
        xlab="Average summer temperature (degrees C)",
        main=warm_caption,
        ylim=c(-5,5)
    )

    EdR.plot(
        date_difference_plot_warm,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s9_left.pdf"),
        width=6,
        height=6
    )

    surfrad_caption <- paste0(
        "SURFRAD:\nMean difference = ",
        surfrad_mean,
        "\nSTDEV = ",
        surfrad_stdev,
        "\nSTDEV_OF_MEAN = ",
        surfrad_stdev_mean
    )

    date_difference_plot_surfrad <- xyplot(
        diff ~ summer_T_avg,
        data=flowering_dates_surfrad[order(flowering_dates_surfrad$summer_T_avg),],
        group=factor(sowing_doy),
        type='b',
        pch=20,
        auto=TRUE,
        grid=TRUE,
        par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        ylab="Clock flowering date - celestial flowering date (days)",
        xlab="Average summer temperature (degrees C)",
        main=surfrad_caption,
        ylim=c(-5,5)
    )

    EdR.plot(
        date_difference_plot_surfrad,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s9_right.pdf"),
        width=6,
        height=6
    )
}

## Compare flowering dates calculated using the CROPGRO model with photoperiod
## length determined in two different ways. Here we use N_min and N_opt values
## that were optimized to produce the closest agreement with the default set of
## sowing dates, and calculate the flowering dates corresponding to a different
## set of sowing dates to check whether the model has been overfitted.
if (CROSS_VALIDATE_CROPGRO) {
    if (DO_NEW_CALCULATIONS) {
        ## Do the big test for the WARM data set
        grimm_sowing_result_warm <- test_grimm_sowing_dates(
            latitude = DEF_LATITUDE,
            longitude = DEF_LONGITUDE,
            sunrise_threshold_angle = GRIMM_SUNRISE_THRESHOLD_ANGLE,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            tbase = DEF_TBASE,
            topt = DEF_TOPT,
            tmax = DEF_TMAX,
            grimm_juvenile_pd_threshold = DEF_GRIMM_JUVENILE_PD_THRESHOLD,
            grimm_flowering_threshold = DEF_GRIMM_FLOWERING_THRESHOLD,
            grimm_juvenile_T0 = DEF_GRIMM_JUVENILE_T0,
            grimm_juvenile_T1 = DEF_GRIMM_JUVENILE_T1,
            grimm_juvenile_T2 = DEF_GRIMM_JUVENILE_T2,
            grimm_juvenile_T3 = DEF_GRIMM_JUVENILE_T3,
            grimm_T_min = DEF_GRIMM_T_MIN,
            grimm_T_opt = DEF_GRIMM_T_OPT,
            grimm_N_min_astro = DEF_GRIMM_N_MIN,
            grimm_N_opt_astro = DEF_GRIMM_N_OPT,
            grimm_N_min_clock = OSCILLATOR_GRIMM_N_MIN,
            grimm_N_opt_clock = OSCILLATOR_GRIMM_N_OPT,
            doy_summer_begin = DEF_DOY_SUMMER_BEGIN,
            doy_summer_end = DEF_DOY_SUMMER_END,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = TRUE,
            sowing_dates = CROSS_VALIDATION_SOWING_DATES
        )

        ## Do the big test for the SURFRAD data set
        grimm_sowing_result_surfrad <- test_grimm_sowing_dates(
            latitude = DEF_LATITUDE,
            longitude = DEF_LONGITUDE,
            sunrise_threshold_angle = GRIMM_SUNRISE_THRESHOLD_ANGLE,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            tbase = DEF_TBASE,
            topt = DEF_TOPT,
            tmax = DEF_TMAX,
            grimm_juvenile_pd_threshold = DEF_GRIMM_JUVENILE_PD_THRESHOLD,
            grimm_flowering_threshold = DEF_GRIMM_FLOWERING_THRESHOLD,
            grimm_juvenile_T0 = DEF_GRIMM_JUVENILE_T0,
            grimm_juvenile_T1 = DEF_GRIMM_JUVENILE_T1,
            grimm_juvenile_T2 = DEF_GRIMM_JUVENILE_T2,
            grimm_juvenile_T3 = DEF_GRIMM_JUVENILE_T3,
            grimm_T_min = DEF_GRIMM_T_MIN,
            grimm_T_opt = DEF_GRIMM_T_OPT,
            grimm_N_min_astro = DEF_GRIMM_N_MIN,
            grimm_N_opt_astro = DEF_GRIMM_N_OPT,
            grimm_N_min_clock = OSCILLATOR_GRIMM_N_MIN,
            grimm_N_opt_clock = OSCILLATOR_GRIMM_N_OPT,
            doy_summer_begin = DEF_DOY_SUMMER_BEGIN,
            doy_summer_end = DEF_DOY_SUMMER_END,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            WARM = FALSE,
            sowing_dates = CROSS_VALIDATION_SOWING_DATES
        )

        save(
            grimm_sowing_result_warm,
            grimm_sowing_result_surfrad,
            file=paste0(DATA_DIR, "/cropgro_astro_comparison_cross.RData")
        )
    }
    else {
        load(file=paste0(DATA_DIR, "/cropgro_astro_comparison_cross.RData"))
    }

    # Get the flowering dates
    flowering_dates_warm <- grimm_sowing_result_warm$flowering_dates

    flowering_dates_surfrad <- grimm_sowing_result_surfrad$flowering_dates

    # Compute the difference
    flowering_dates_warm$diff <-
        flowering_dates_warm$clock_flowering_date -
                flowering_dates_warm$celestial_flowering_date

    flowering_dates_surfrad$diff <-
        flowering_dates_surfrad$clock_flowering_date -
            flowering_dates_surfrad$celestial_flowering_date

    # Get statistics about the differences
    warm_mean <- mean(flowering_dates_warm$diff)
    warm_stdev <- sd(flowering_dates_warm$diff)
    warm_stdev_mean <- warm_stdev /
        sqrt(length(flowering_dates_warm$diff))

    surfrad_mean <- mean(flowering_dates_surfrad$diff)
    surfrad_stdev <- sd(flowering_dates_surfrad$diff)
    surfrad_stdev_mean <- surfrad_stdev /
        sqrt(length(flowering_dates_surfrad$diff))


    # Set up some colors
    cols <- brewer.pal(8, "Dark2")

    cols <- c("#000000", cols)

    # Plot the clock date vs celestial date
    clock_v_celestial_plot_warm <- xyplot(
        clock_flowering_date ~ celestial_flowering_date,
        group = factor(sowing_doy),
        data = flowering_dates_warm[order(
            flowering_dates_warm$celestial_flowering_date),],
        type = 'p',
        pch = 20,
        par.settings = list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        auto = TRUE,
        main = "WARM",
        xlim = c(155, 215),
        ylim = c(155, 215),
        xlab = "Flowering day (celestial mechanics)",
        ylab = "Flowering day (oscillator clock)"
    )

    EdR.plot(
        clock_v_celestial_plot_warm,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s10_top_left.pdf"),
        width = 8,
        height = 8
    )

    clock_v_celestial_plot_surfrad <- xyplot(
        clock_flowering_date ~ celestial_flowering_date,
        group = factor(sowing_doy),
        data = flowering_dates_surfrad[order(
            flowering_dates_surfrad$celestial_flowering_date),],
        type = 'p',
        pch = 20,
        par.settings = list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        auto = TRUE,
        main = "SURFRAD",
        xlim = c(155, 215),
        ylim = c(155, 215),
        xlab = "Flowering day (celestial mechanics)",
        ylab = "Flowering day (oscillator clock)"
    )

    EdR.plot(
        clock_v_celestial_plot_surfrad,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s10_top_right.pdf"),
        width = 8,
        height = 8
    )

    # Plot the clock difference against average summer temperature
    warm_caption <- paste0(
        "WARM:\nMean difference = ",
        warm_mean,
        "\nSTDEV = ",
        warm_stdev,
        "\nSTDEV_OF_MEAN = ",
        warm_stdev_mean
    )

    date_difference_plot_warm <- xyplot(
        diff ~ summer_T_avg,
        data=flowering_dates_warm[order(flowering_dates_warm$summer_T_avg),],
        group=factor(sowing_doy),
        type='b',
        pch=20,
        auto=TRUE,
        grid=TRUE,
        par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        ylab="Clock flowering date - celestial flowering date (days)",
        xlab="Average summer temperature (degrees C)",
        main=warm_caption,
        ylim=c(-5,5)
    )

    EdR.plot(
        date_difference_plot_warm,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s10_bottom_left.pdf"),
        width=6,
        height=6
    )

    surfrad_caption <- paste0(
        "SURFRAD:\nMean difference = ",
        surfrad_mean,
        "\nSTDEV = ",
        surfrad_stdev,
        "\nSTDEV_OF_MEAN = ",
        surfrad_stdev_mean
    )

    date_difference_plot_surfrad <- xyplot(
        diff ~ summer_T_avg,
        data=flowering_dates_surfrad[order(flowering_dates_surfrad$summer_T_avg),],
        group=factor(sowing_doy),
        type='b',
        pch=20,
        auto=TRUE,
        grid=TRUE,
        par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        ylab="Clock flowering date - celestial flowering date (days)",
        xlab="Average summer temperature (degrees C)",
        main=surfrad_caption,
        ylim=c(-5,5)
    )

    EdR.plot(
        date_difference_plot_surfrad,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s10_bottom_right.pdf"),
        width=6,
        height=6
    )
}

if (TEST_CROPGRO_PARAMETERS) {
    if (DO_NEW_CALCULATIONS) {
        combo_result <- test_grimm_sowing_dates_model_params_all_years(
            latitude = DEF_LATITUDE,
            longitude = DEF_LONGITUDE,
            sunrise_threshold_angle = GRIMM_SUNRISE_THRESHOLD_ANGLE,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE,
            tbase = DEF_TBASE,
            topt = DEF_TOPT,
            tmax = DEF_TMAX,
            grimm_juvenile_pd_threshold = DEF_GRIMM_JUVENILE_PD_THRESHOLD,
            grimm_flowering_threshold = DEF_GRIMM_FLOWERING_THRESHOLD,
            grimm_juvenile_T0 = DEF_GRIMM_JUVENILE_T0,
            grimm_juvenile_T1 = DEF_GRIMM_JUVENILE_T1,
            grimm_juvenile_T2 = DEF_GRIMM_JUVENILE_T2,
            grimm_juvenile_T3 = DEF_GRIMM_JUVENILE_T3,
            grimm_T_min = DEF_GRIMM_T_MIN,
            grimm_T_opt = DEF_GRIMM_T_OPT,
            grimm_N_min_astro = DEF_GRIMM_N_MIN,
            grimm_N_opt_astro = DEF_GRIMM_N_OPT,
            doy_summer_begin = DEF_DOY_SUMMER_BEGIN,
            doy_summer_end = DEF_DOY_SUMMER_END,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            grimm_N_min_vec = GRIMM_N_MINS_TO_TEST,
            grimm_N_opt_vec = GRIMM_N_OPTS_TO_TEST,
            sowing_date_vec = DEF_SOWING_DATES,
            WARM = TRUE
        )

        error_result <-
            test_grimm_sowing_dates_model_params_analyze(combo_result)

        if (SAVE_TO_FILE) {
            write.csv(
                error_result,
                file=paste0(TABLE_DIR, "/table_s3.csv"),
                row.names=FALSE
            )
        }

        save(
            combo_result,
            error_result,
            file=paste0(DATA_DIR, "/cropgro_parameter_testing.RData")
        )
    }
    else {
        load(file=paste0(DATA_DIR, "/cropgro_parameter_testing.RData"))
    }
}
