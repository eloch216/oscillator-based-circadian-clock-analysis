## Specify directories
SURFRAD_RAW_DIR <- "raw_SURFRAD"
WARM_RAW_DIR <- "raw_WARM"
OUTPUT_DIR <- "output"
SURFRAD_PROCESSED_DIR <- paste0(OUTPUT_DIR, "/processed_SURFRAD")
WARM_PROCESSED_DIR <- paste0(OUTPUT_DIR, "/processed_WARM")
DATA_DIR <- paste0(OUTPUT_DIR, "/RData")
FIGURE_DIR <- paste0(OUTPUT_DIR, "/figures")
TABLE_DIR <- paste0(OUTPUT_DIR, "/tables")

## Create directories if they don't exist
if (!dir.exists(SURFRAD_RAW_DIR)) {
    dir.create(SURFRAD_RAW_DIR)
}

if (!dir.exists(WARM_RAW_DIR)) {
    dir.create(WARM_RAW_DIR)
}

if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR)
}

if (!dir.exists(SURFRAD_PROCESSED_DIR)) {
    dir.create(SURFRAD_PROCESSED_DIR)
}

if (!dir.exists(WARM_PROCESSED_DIR)) {
    dir.create(WARM_PROCESSED_DIR)
}

if (!dir.exists(DATA_DIR)) {
    dir.create(DATA_DIR)
}

if (!dir.exists(FIGURE_DIR)) {
    dir.create(FIGURE_DIR)
}

if (!dir.exists(TABLE_DIR)) {
    dir.create(TABLE_DIR)
}

## Set default parameter values for other functions to use
DEF_ADAPTIVE_ERROR_TOL <- 1e-6
DEF_ADAPTIVE_MAX_STEPS <- 200
DEF_ATMOSPHERIC_PRESSURE <- 101592        # 30 inches of mercury in Pascals
DEF_ATMOSPHERIC_TRANSMITTANCE <- 0.65     # A  pretty clear day
DEF_CLOCK_GAMMA <- 0.1
DEF_CLOCK_PERIOD <- 24.0
DEF_CLOCK_R0 <- 1.0
DEF_DAWN_PHASE_INITIAL <- 200.0
DEF_DOY_SUMMER_BEGIN <- 120.0
DEF_DOY_SUMMER_END <- 210.0
DEF_DUSK_PHASE_INITIAL <- 80.0
DEF_GRIMM_FLOWERING_THRESHOLD <- 18.30
DEF_GRIMM_JUVENILE_PD_THRESHOLD <- 8.0
DEF_GRIMM_JUVENILE_T0 <- 6.0
DEF_GRIMM_JUVENILE_T1 <- 30.0
DEF_GRIMM_JUVENILE_T2 <- 35.0
DEF_GRIMM_JUVENILE_T3 <- 45.0
DEF_GRIMM_N_MIN <- 7.65
DEF_GRIMM_N_OPT <- 9.98
DEF_GRIMM_T_MIN <- 2.52
DEF_GRIMM_T_OPT <- 25.28
DEF_KICK_STRENGTH <- 0.5
DEF_LATITUDE <- 40.109665
DEF_LIGHT_EXP_AT_ZERO <- 10.0
DEF_LIGHT_THRESHOLD <- 60.0
DEF_LONGITUDE <- -88.204247
DEF_OUTPUT_STEP_SIZE <- 1.0
DEF_PAR_PERCENT <- 0.43
DEF_SOLVER_METHOD <- "Gro_rkck54"
DEF_SOWING_DATES <- c(120, 130, 140, 150, 160)
DEF_SUNRISE_THRESHOLD_ANGLE <- -0.56      # Sun clears the horizon
DEF_TBASE <- 10.0
DEF_TIME_ZONE_OFFSET <- -5                # Central Daylight Time
DEF_TMAX <- 40.0
DEF_TOPT <- 30.0
DEF_TRACKER_RATE <- 2.0
DEF_TTC_THRESHOLD <- 520.0                # Determined by optimization
GRIMM_SUNRISE_THRESHOLD_ANGLE <- 0.0      # Value used in Grimm paper
OSCILLATOR_GRIMM_N_MIN <- 6.05            # Determined by optimization
OSCILLATOR_GRIMM_N_OPT <- 11.18           # Determined by optimization
TWILIGHT_SUNRISE_THRESHOLD_ANGLE <- -6.0  # Civil twilight, i.e., sky begins to lighten
