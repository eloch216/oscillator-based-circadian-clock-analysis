## This file specifies directories and other settings required to load and
## process the WARM data

## Specify file that contains the raw WARM data
WARM_RAW_FILE <- "cmiday.txt"

## Specify the WARM data column names
WARM_DATA_COLUMNS <- c(
    "year", "month", "day",
    "max_wind_gust", "avg_wind_speed", "avg_wind_dir",
    "sol_rad",
    "max_air_temp", "min_air_temp", "avg_air_temp",
    "max_rel_hum", "min_rel_hum", "avg_rel_hum",
    "avg_dewpt_temp",
    "precip",
    "pot_evapot",
    "max_soiltemp_4in_sod", "min_soiltemp_4in_sod", "avg_soiltemp_4in_sod",
    "max_soiltemp_8in_sod", "min_soiltemp_8in_sod", "avg_soiltemp_8in_sod",
    "max_soiltemp_4in_bare", "min_soiltemp_4in_bare", "avg_soiltemp_4in_bare",
    "max_soiltemp_2in_bare", "min_soiltemp_2in_bare", "avg_soiltemp_2in_bare"
)

## Specify the corresponding error indicators that should be paired with each
## data column. Note: some columns, e.g. year, cannot have errors. For these
##columns, just use the data column as the error column since it will never
## equal M
WARM_ERROR_COLUMNS <- c(
    "year", "month", "day",
    "xwser", "awser", "awder",
    "soler",
    "xater", "nater", "aater",
    "xrher", "nrher", "arher",
    "adper",
    "pcer",
    "pevaper",
    "xst4soder", "nst4soder", "ast4soder",
    "xst8soder", "nst8soder", "ast8soder",
    "xst4bareer", "nst4bareer", "ast4bareer",
    "xst2bareer", "nst2bareer", "ast2bareer"
)

## Specify data columns that should be cleaned (most of them, with the exception
## of year, month, day)
WARM_CLEAN_COLUMNS <- c(
    "max_wind_gust", "avg_wind_speed", "avg_wind_dir",
    "sol_rad",
    "max_air_temp", "min_air_temp", "avg_air_temp",
    "max_rel_hum", "min_rel_hum", "avg_rel_hum",
    "avg_dewpt_temp",
    "precip",
    "pot_evapot",
    "max_soiltemp_4in_sod", "min_soiltemp_4in_sod", "avg_soiltemp_4in_sod",
    "max_soiltemp_8in_sod", "min_soiltemp_8in_sod", "avg_soiltemp_8in_sod",
    "max_soiltemp_4in_bare", "min_soiltemp_4in_bare", "avg_soiltemp_4in_bare",
    "max_soiltemp_2in_bare", "min_soiltemp_2in_bare", "avg_soiltemp_2in_bare"
)

## Specify the names of a few important columns
WARM_SOLAR_COLUMN_NAME <- "sol_rad"
WARM_CLEAN_SOLAR_COLUMN_NAME <- "sol_rad_clean"
WARM_CLEAN_MAX_TEMP_COLUMN_NAME <- "max_air_temp_clean"
WARM_CLEAN_MIN_TEMP_COLUMN_NAME <- "min_air_temp_clean"
WARM_PRECIPITATION_COLUMN_NAME <- "precip"
