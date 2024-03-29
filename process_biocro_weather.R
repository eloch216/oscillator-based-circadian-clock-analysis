# This script must be run after `process_WARM.R` and `process_SURFRAD.R`, and
# the processed weather data must be present in the `output` directory; it may
# be necessary to copy the folders from the `output_archive` if you don't want
# to redo the processing.
#
# This script produces an R data file called 'cmi_weather_data.Rdata' that
# includes weather data that is suitable for use with BioCro simulations. In
# fact, this script is used to generate the weather data that is included with
# the public version of the BioCro package.

source('set_defaults.R')
source('utilities/weather_utilities.R')

# Define a function for processing a year of weather data. Here we take most of
# the columns from the SURFRAD data, with the exception of `precip` which comes
# from WARM since the SURFRAD data does not include rainfall.
process_year <- function(
    surfrad_data,
    warm_data,
    year,
    weather_columns = c(
        'year',
        'doy',
        'hour',
        'time_zone_offset',
        'precip',
        'rh',
        'solar',
        'temp',
        'windspeed'
    )
)
{
    # Get the WARM and SURFRAD data
    weather <- convert_to_biocro_surfrad(surfrad_data)
    warm_weather <- convert_to_biocro_warm(warm_data)

    # Make sure the data sets have the same number of observations
    if (nrow(weather) != nrow(warm_weather)) {
        print('length mismatch')
    }

    # Add the precipitation data from warm to the rest of the surfrad data
    weather[['precip']] <- warm_weather[['precip']]

    # Add a column for the time zone offset
    weather[['time_zone_offset']] <- DEF_TIME_ZONE_OFFSET

    return(weather[weather_columns])
}

# Get weather data from each year
weather <- list(
    '1995' = process_year(surfrad1995, warm1995, 1995),
    '1996' = process_year(surfrad1996, warm1996, 1996),
    '1997' = process_year(surfrad1997, warm1997, 1997),
    '1998' = process_year(surfrad1998, warm1998, 1998),
    '1999' = process_year(surfrad1999, warm1999, 1999),
    '2000' = process_year(surfrad2000, warm2000, 2000),
    '2001' = process_year(surfrad2001, warm2001, 2001),
    '2002' = process_year(surfrad2002, warm2002, 2002),
    '2003' = process_year(surfrad2003, warm2003, 2003),
    '2004' = process_year(surfrad2004, warm2004, 2004),
    '2005' = process_year(surfrad2005, warm2005, 2005),
    '2006' = process_year(surfrad2006, warm2006, 2006),
    '2007' = process_year(surfrad2007, warm2007, 2007),
    '2008' = process_year(surfrad2008, warm2008, 2008),
    '2009' = process_year(surfrad2009, warm2009, 2009),
    '2010' = process_year(surfrad2010, warm2010, 2010),
    '2011' = process_year(surfrad2011, warm2011, 2011),
    '2012' = process_year(surfrad2012, warm2012, 2012),
    '2013' = process_year(surfrad2013, warm2013, 2013),
    '2014' = process_year(surfrad2014, warm2014, 2014),
    '2015' = process_year(surfrad2015, warm2015, 2015),
    '2016' = process_year(surfrad2016, warm2016, 2016),
    '2017' = process_year(surfrad2017, warm2017, 2017),
    '2018' = process_year(surfrad2018, warm2018, 2018),
    '2019' = process_year(surfrad2019, warm2019, 2019),
    '2020' = process_year(surfrad2020, warm2020, 2020),
    '2021' = process_year(surfrad2021, warm2021, 2021),
    '2022' = process_year(surfrad2022, warm2022, 2022),
    '2023' = process_year(surfrad2022, warm2022, 2023)
)

# Double check to make sure years are consistent
if (max(as.numeric(names(weather))) != max(FULL_YEARS)) {
    warning('Double check years used for cmi_weather_data')
}

# Save the results
save(weather, file = paste0(DATA_DIR, '/cmi_weather_data.RData'))
