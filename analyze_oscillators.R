## Run some functions to get info about oscillators

library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/oscillator_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
ANALYZE_KICK_RESPONSE <- TRUE   # produces figure 2
COMPARE_POINCARE_MAGIC <- TRUE  # produces figure s5
SAVE_TO_FILE <- TRUE

## Choose some oscillator settings to be used just for these calculations
OSCILLATOR_TEST_KICK_STRENGTH = 1.0
OSCILLATOR_TEST_KICK_END = 200.0
OSCILLATOR_TEST_DOY_MAX = 300.0
OSCILLATOR_TEST_CLOCK_PERIOD = 30.0
OSCILLATOR_TEST_STEP_SIZE = 0.2

## Calculate a phase trajectory for a Poincare oscillator subjected to a
## sinusoidal driving force
if (ANALYZE_KICK_RESPONSE) {
    # Either run calculations or load a previously stored result
    if (DO_NEW_CALCULATIONS) {
        # Run the simulation
        poincare_result <- run_oscillator(
            "poincare_clock",
            kick_strength = OSCILLATOR_TEST_KICK_STRENGTH,
            kick_start = 100.0,
            kick_end = OSCILLATOR_TEST_KICK_END,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = OSCILLATOR_TEST_CLOCK_PERIOD,
            num_doy_to_run = OSCILLATOR_TEST_DOY_MAX,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = OSCILLATOR_TEST_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE
        )
        
        # Save the result, since this simulation may take a long time to run and
        # it would be nice if we don't need to redo it
        save(poincare_result, file=paste0(DATA_DIR, "/figure_2.RData"))
    }
    else {
        load(paste0(DATA_DIR, "/figure_2.RData"))
    }
    
    # Add a new column that splits the result into three time intervals, chosen
    # to correspond to the intervals where the kick is either zero or nonzero
    poincare_result$interval <- floor(poincare_result$doy_dbl / 100.0)
    
    phase_space_plot <- xyplot(
        dawn_b ~ dawn_a,
        group = factor(interval),
        data = poincare_result,
        type = 'l',
        xlim = c(-4, 4),
        ylim = c(-4, 4),
        xlab = "Oscillator a",
        ylab = "Oscillator b",
        grid = TRUE
    )
    
	EdR.plot(
        phase_space_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_2a.pdf"),
        width=6,
        height=6
    )
    
    phase_progression_plot <- xyplot(
        dawn_phase_2 ~ doy_dbl,
        group = factor(interval),
        data = poincare_result,
        type = 'l',
        xlim = c(0, 300),
        ylim = c(0, 520),
        xlab = "Time (days)",
        ylab = "Oscillator phase (phi / pi)",
        grid = TRUE
    )
    
	EdR.plot(
        phase_progression_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_2b.pdf"),
        width=6,
        height=6
    )
    
}

## Compare a Poincare oscillator to a "magic" oscillator
if (COMPARE_POINCARE_MAGIC) {
    
    if (DO_NEW_CALCULATIONS) {
        # Run the simulations
        
        KICK_START_FOR_COMPARE <- 0.0
        
        poincare_result <- run_oscillator(
            "poincare_clock",
            kick_strength = OSCILLATOR_TEST_KICK_STRENGTH,
            kick_start = KICK_START_FOR_COMPARE,
            kick_end = OSCILLATOR_TEST_KICK_END,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = OSCILLATOR_TEST_CLOCK_PERIOD,
            num_doy_to_run = OSCILLATOR_TEST_DOY_MAX,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = OSCILLATOR_TEST_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE
        )
        
        magic_result <- run_oscillator(
            "magic_clock",
            kick_strength = OSCILLATOR_TEST_KICK_STRENGTH,
            kick_start = KICK_START_FOR_COMPARE,
            kick_end = OSCILLATOR_TEST_KICK_END,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = OSCILLATOR_TEST_CLOCK_PERIOD,
            num_doy_to_run = OSCILLATOR_TEST_DOY_MAX,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = OSCILLATOR_TEST_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE
        )
        
        total_result <- EdR.merge(
            list(poincare_result, magic_result),
            list("poincare", "magic"),
            "clock_type"
        )
        
        # Save the result, since this simulation may take a long time to run and
        # it would be nice if we don't need to redo it
        save(total_result, file=paste0(DATA_DIR, "/figure_s5.RData"))
    }
    else {
        load(paste0(DATA_DIR, "/figure_s5.RData"))
    }
    
    total_result_subset <- total_result[which(total_result$doy_dbl >= 180 &
        total_result$doy_dbl <= 220),]
    
    phase_space_plot <- xyplot(
        dawn_b ~ dawn_a | clock_type,
        data = total_result_subset,
        type = 'l',
        xlim = c(-4, 4),
        ylim = c(-4, 4),
        xlab = "Oscillator a",
        ylab = "Oscillator b",
        grid = TRUE
    )
    
	EdR.plot(
        phase_space_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s5_top.pdf"),
        width=12,
        height=6
    )
    
    phase_progression_plot <- xyplot(
        dawn_phase_2 ~ doy_dbl | clock_type,
        data = total_result_subset,
        type = 'l',
        xlim = c(180, 220),
        xlab = "Time (days)",
        ylab = "Oscillator phase (phi / pi)",
        grid = TRUE
    )
    
	EdR.plot(
        phase_progression_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s5_bottom.pdf"),
        width=12,
        height=6
    )
    
}