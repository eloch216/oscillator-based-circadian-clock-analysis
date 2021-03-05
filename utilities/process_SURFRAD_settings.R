## This file specifies directories and other settings required to load and
## process the SURFRAD data

## Specify columns and column names to extract from the data files. The data
## files lack headers, so when a file is loaded using read.delim, the column
## names are simply V1, V2, etc. Here we need to choose a subset of the columns,
## and also supply the real column names. We extract the following, based on the
## info in the SURFRAD README file:
##   V1 ->      year -> year
##   V2 ->       day -> day of year (doy)
##   V5 ->      hour -> hour of day
##   V6 ->       min -> minute of hour
##   V8 ->       zen -> solar zenith angle (degrees)
##  V13 ->  direct_n -> direct-normal solar (W m^-2)
##  V14 -> qc_direct_n
##  V15 ->   diffuse -> downwelling diffuse solar (W m^2)
##  V16 -> qc_diffuse
##  V31 ->       par -> photosynthetically active radiation (Watts m^-2)
##  V32 -> qc_par
##  V39 ->      temp -> 10-meter air temperature (deg C)
##  V40 -> qc_temp
##  V41 ->        rh -> relative humidity (%)
##  V42 -> qc_rh
##  V43 -> windspeed -> wind speed (ms^-1)
##  V44 -> qc_windspeed
##  V47 ->  pressure -> station pressure (mb)
##  V48 -> qc_pressure
SUFRAD_COLUMNS <- c(
    "V1",
    "V2",
    "V5",
    "V6",
    "V8",
    "V13",
    "V14",
    "V15",
    "V16",
    "V31",
    "V32",
    "V39",
    "V40",
    "V41",
    "V42",
    "V43",
    "V44",
    "V47",
    "V48"
)

SUFRAD_COLUMN_NAMES <- c(
    "year",
    "doy",
    "hour",
    "min",
    "zen",
    "direct_n",
    "qc_direct_n",
    "diffuse",
    "qc_diffuse",
    "par",
    "qc_par",
    "temp",
    "qc_temp",
    "rh",
    "qc_rh",
    "windspeed",
    "qc_windspeed",
    "pressure",
    "qc_pressure"
)

## Specify variables to extract for hourly data. Here we make a distinction
## between quality-controlled and non-quality-controlled variables. Note that we
## don't need to include year, doy, or hour in the lists (these will be
## automatically included)
NQC_VARIABLE_NAMES <- c(
    "zen"
)

QC_VARIABLE_NAMES <- c(
    "direct_n",
    "diffuse",
    "par",
    "temp",
    "rh",
    "windspeed",
    "pressure"
)

QC_NAMES <- c(
    "qc_direct_n",
    "qc_diffuse",
    "qc_par",
    "qc_temp",
    "qc_rh",
    "qc_windspeed",
    "qc_pressure"
)

## Get lists of all the subdirectories of the raw SURDFRAD directory. We want
## the full paths for changing the working directory in R. We also want just
## the name for naming the output files.
SURFRAD_SUBDIRECTORY_PATHS <- list.dirs(
    path=SURFRAD_RAW_DIR,
    full.names=TRUE,
    recursive=FALSE
)

SURFRAD_SUBDIRECTORY_NAMES <- list.dirs(
    path=SURFRAD_RAW_DIR,
    full.names=FALSE, recursive=FALSE
)
