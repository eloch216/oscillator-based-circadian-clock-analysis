## Run some functions to compare the CROPGRO model to a GDD threshold

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/gdd_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
DETERMINE_ERRORS <- TRUE     # produces figure s10
COMPARE_CROPGRO_GDD <- TRUE  # produces figures 7a and 7b
SAVE_TO_FILE <- TRUE

## Define some settings to use in this script
GDD_THRESHOLDS_TO_CHECK <- seq(from=450, to=650, by=25)

if (COMPARE_CROPGRO_GDD) {
    if (DO_NEW_CALCULATIONS) {
        cropgro_v_gdd_warm <- comp_grimm_ttc_sowing_dates(
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
            astro_grimm = TRUE,
            TTc_threshold = DEF_TTC_THRESHOLD,
            WARM = TRUE,
            sowing_dates = DEF_SOWING_DATES
        )

        cropgro_v_gdd_surfrad <- comp_grimm_ttc_sowing_dates(
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
            astro_grimm = TRUE,
            TTc_threshold = DEF_TTC_THRESHOLD,
            WARM = FALSE,
            sowing_dates = DEF_SOWING_DATES
        )

        save(
            cropgro_v_gdd_warm,
            cropgro_v_gdd_surfrad,
            file=paste0(DATA_DIR, "/cropgro_gdd_comparison.RData")
        )
    }
    else {
        load(file=paste0(DATA_DIR, "/cropgro_gdd_comparison.RData"))
    }

    # Set up some colors
	cols <- brewer.pal(8, "Dark2")
	cols <- c("#000000", cols)

    # Produce figure 7a
    ttc_v_grimm_plot <- xyplot(
		doy_r1_TTc ~ doy_r1,
        group=doy_sow,
        data=cropgro_v_gdd_warm[order(cropgro_v_gdd_warm$doy_r1),],
		type='p',
        pch=20,
        auto=TRUE,
		xlim=c(155,215),
        ylim=c(155,215),
		par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        xlab="Flowering day (celestial mechanics)",
        ylab="Flowering day (GDD model)"
	)

	EdR.plot(
        ttc_v_grimm_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_7a.pdf"),
        width=8,
        height=8
    )

    # Produce figure 7b
    ylabel <- paste0(
        "Flowering day difference",
        "\n",
        "(GDD model - celestial mechanics)"
    )

    diff_v_temp_plot <- xyplot(
		difference ~ summer_T_avg,
        group=doy_sow,
        data=cropgro_v_gdd_warm[order(cropgro_v_gdd_warm$summer_T_avg),],
		type='b',
        pch=20,
        auto=TRUE,
		ylim=c(-10,10),
		par.settings=list(
            superpose.line=list(col=cols),
            superpose.symbol=list(col=cols)
        ),
        xlab="Average summer temperature (degrees C)",
        ylab=ylabel
	)

	EdR.plot(
        diff_v_temp_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_7b.pdf"),
        width=6,
        height=6
    )
}

if (DETERMINE_ERRORS) {
    if (DO_NEW_CALCULATIONS) {
        gdd_errors <- comp_grimm_ttc_test_thresholds(
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
            astro_grimm = TRUE,
            TTc_thresholds = GDD_THRESHOLDS_TO_CHECK,
            WARM = TRUE,
            sowing_dates = DEF_SOWING_DATES,
            print_updates = TRUE
        )

        save(gdd_errors, file=paste0(DATA_DIR, "/figure_s10.RData"))
    }
    else {
        load(file=paste0(DATA_DIR, "/figure_s10.RData"))
    }

    # Fit a quadratic model
    model <- lm(error ~ poly(TTc_thresholds, 2, raw=TRUE), data=gdd_errors)

    # Get the coefficients
    a2 <- coef(model)[3]  # coefficient of x^2
    a1 <- coef(model)[2]  # coefficient of x
    a0 <- coef(model)[1]  # intercept

    # Get the value of TTc_threshold that minimizes the error
    best_threshold <- -a1 / (2 * a2)

    # Add a new column
    gdd_errors$parabolic_fit <- a0 + a1 * gdd_errors$TTc_thresholds +
                                a2 * gdd_errors$TTc_thresholds^2

    # Make a caption for the figure
    caption <- paste(
        "Parabolic fit:",
        "\n",
        "error =",
        a2,
        "* threshold^2 +",
        "\n",
        a1,
        "* threshold +",
        a0,
        "\n",
        "Minimum occurs for threshold =",
        best_threshold
    )

    fitting_plot <- xyplot(
        error + parabolic_fit ~ TTc_thresholds,
        data = gdd_errors,
        type='b',
        pch=20,
        main=caption,
        xlab="GDD threshold for flowering",
        ylab="Total error (arb. units)",
        auto=TRUE
    )

	EdR.plot(
        fitting_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s10.pdf"),
        width=6,
        height=7
    )
}
