## Run some functions to get info about the clock's response to changes in
## initial conditions or day length

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/response_utilities.R")

## Choose which operations to perform
DO_NEW_CALCULATIONS <- TRUE
ANALYZE_IC <- TRUE     # produces figure 4a
ANALYZE_JUMP <- TRUE   # produces figure 4b
SAVE_TO_FILE <- TRUE

## Choose some settings to be used just for these calculations
RESPONSE_MAX_DOY <- 100
RESPONSE_NUM_IC <- 24
RESPONSE_NUM_JUMP <- 15
RESPONSE_DOY_JUMP <- 50
RESPONSE_HIRES_STEP_SIZE <- 0.01

## Helping function for making colormap image
make_colormap_image <- function(ncolors) {
    x <- 1
    y <- seq(from=0, to=1, length.out=ncolors)
    z <- matrix(ncol = ncolors, nrow = 1)
    z[1,] <- y
    return(list(x=x, y=y, z=z))
}

## Analyze the response to initial conditions
if (ANALYZE_IC) {
    # Either run calculations or load a previously stored result
    if (DO_NEW_CALCULATIONS) {

        ic_result <- run_clock_initial_conditions_gaussian(
            num_doy_to_run = RESPONSE_MAX_DOY,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            num_ic = RESPONSE_NUM_IC,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE
        )

        # Save the result, since this simulation may take a long time to run and
        # it would be nice if we don't need to redo it
        save(ic_result, file=paste0(DATA_DIR, "/figure_4a.RData"))
    }
    else {
        load(paste0(DATA_DIR, "/figure_4a.RData"))
    }

    # Make the plot
    cols <- brewer.pal(11, "RdYlBu")

    pal <- colorRampPalette(cols)

    ic_plot_gaussian <- xyplot(
        day_length ~ doy_dbl,
        group=factor(initial_phase),
        data=ic_result,
        type='l',
        ylim=c(0, 24),
        xlim=c(0, 100),
        xlab="Time (days)",
        ylab="Photoperiod length (hr)",
        par.settings=list(
            superpose.line=list(col=pal(RESPONSE_NUM_IC)),
            superpose.symbol=list(col=pal(RESPONSE_NUM_IC))
        ),
        grid=TRUE,
        auto.key=list(space="right")
    )

    EdR.plot(
        ic_plot_gaussian,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_4a.pdf"),
        width = 6,
        height = 5
    )

    # Make the colormap image
    colormap_image <- make_colormap_image(RESPONSE_NUM_IC)

    if (SAVE_TO_FILE) {
        pdf(
            file = paste0(FIGURE_DIR, "/figure_4a_colormap.pdf"),
            width = 2,
            height = 3,
            useDingbats = FALSE
        )
        image(colormap_image, col=pal(RESPONSE_NUM_IC))
        dev.off()
    }
    else {
        x11(
            width = 2,
            height = 3
        )
        image(colormap_image, col=pal(RESPONSE_NUM_IC))
    }
}

## Analyze the response to initial conditions
if (ANALYZE_JUMP) {
    # Either run calculations or load a previously stored result
    if (DO_NEW_CALCULATIONS) {
        
        # Do the calculations with the standard time step for plotting purposes
        jump_result <- run_clock_jumps_gaussian(
            target_doy_dbl = RESPONSE_DOY_JUMP,
            num_doy_to_run = RESPONSE_MAX_DOY,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = DEF_OUTPUT_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            num_jumps = RESPONSE_NUM_JUMP,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE
        )
        
        # Add a column for sigma
        jump_result$sigma <- jump_result$new_dl / 4.0
        
        # Do the calculations with a finer time step for determining entrainment
        # times
        jump_result_hires <- run_clock_jumps_gaussian(
            target_doy_dbl = RESPONSE_DOY_JUMP,
            num_doy_to_run = RESPONSE_MAX_DOY,
            kick_strength = DEF_KICK_STRENGTH,
            clock_gamma = DEF_CLOCK_GAMMA,
            clock_r0 = DEF_CLOCK_R0,
            clock_period = DEF_CLOCK_PERIOD,
            dawn_phase_initial = DEF_DAWN_PHASE_INITIAL,
            dusk_phase_initial = DEF_DUSK_PHASE_INITIAL,
            light_threshold = DEF_LIGHT_THRESHOLD,
            solver_method = DEF_SOLVER_METHOD,
            output_step_size = RESPONSE_HIRES_STEP_SIZE,
            adaptive_error_tol = DEF_ADAPTIVE_ERROR_TOL,
            adaptive_max_steps = DEF_ADAPTIVE_MAX_STEPS,
            verbose = FALSE,
            num_jumps = RESPONSE_NUM_JUMP,
            light_exp_at_zero = DEF_LIGHT_EXP_AT_ZERO,
            tracker_rate = DEF_TRACKER_RATE
        )
        
        # Get info about the timing
        dl_values <- unique(jump_result_hires$new_dl) # Get all target day length values
        final_photoperiod_values <- dl_values
        entrainment_times <- dl_values
        for (i in 1:length(dl_values)) {
            data_chunk <- jump_result_hires[which(jump_result_hires$new_dl==dl_values[i]),]
            late_data_chuck <- data_chunk[which(data_chunk$doy >= 80),]
            final_photoperiod <- mean(late_data_chuck$day_length)   # Get the final day length value
            final_photoperiod_values[i] <- final_photoperiod
            for (j in 1:length(data_chunk$day_length)) {
                if (abs(data_chunk$day_length[j] - final_photoperiod) < (1.0 / 60.0)) {
                    entrainment_times[i] <- data_chunk$doy_dbl[j]
                    break
                }
            }
        }

        # Save the result, since this simulation may take a long time to run and
        # it would be nice if we don't need to redo it
        save(
            jump_result,
            jump_result_hires,
            dl_values,
            final_photoperiod_values,
            entrainment_times,
            file=paste0(DATA_DIR, "/figure_4b.RData")
        )
    }
    else {
        load(paste0(DATA_DIR, "/figure_4b.RData"))
    }

    # Make the jump plot
    cols <- brewer.pal(11, "RdYlBu")

    pal <- colorRampPalette(cols)

    new_cols <- c("#000000", pal(14))

    jump_plot <- xyplot(
        day_length ~ doy_dbl,
        group=factor(sigma),
        data=jump_result,
        type='l',
        ylim=c(0,24),
        xlim=c(0,100),
        xlab="Time (days)",
        ylab="Photoperiod length (hr)",
        par.settings=list(
            superpose.line=list(col=new_cols),
            superpose.symbol=list(col=new_cols)
        ),
        grid=TRUE,
        auto.key=list(space="right")
    )

    EdR.plot(
        jump_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_4b.pdf"),
        width = 6,
        height = 5
    )
    
    # Plot the entrainment times
    jump_overlay_plot <- xyplot(
        final_photoperiod_values ~ entrainment_times,
        type='p',
        pch=20,
        ylim=c(0,24),
        xlim=c(0,100),
        xlab="Time (days)",
        ylab="Photoperiod length (hr)",
        par.settings=list(
            superpose.line=list(col=c("#000000"),
            superpose.symbol=list(col=c("#000000")))
        )
    )
    
    EdR.plot(
        jump_overlay_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_4b_overlay.pdf"),
        width = 6,
        height = 5
    )
    
    # Plot the solar profile for the longest and shortest days
    gaussian_solar_plot <- xyplot(
        solar ~ doy_dbl,
        group=factor(sigma),
        data=jump_result[which(
            jump_result$sigma==min(jump_result$sigma) | 
                jump_result$sigma==max(jump_result$sigma)),],
        type='l',
        xlim=c(70,71),
        auto=TRUE,
        xlab="day of year",
        ylab="PPFD (umol / m^2 / s)"
    )
    
    EdR.plot(
        gaussian_solar_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_4b_inset.pdf"),
        width=6,
        height=5
    )

    # Make the colormap image
    colormap_image <- make_colormap_image(length(new_cols))

    if (SAVE_TO_FILE) {
        pdf(
            file = paste0(FIGURE_DIR, "/figure_4b_colormap.pdf"),
            width = 2,
            height = 3,
            useDingbats = FALSE
        )
        image(colormap_image, col=new_cols)
        dev.off()
    }
    else {
        x11(
            width = 2,
            height = 3
        )
        image(colormap_image, col=new_cols)
    }
}