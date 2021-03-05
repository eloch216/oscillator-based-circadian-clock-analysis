## Define some functions related to temperature

source("utilities/celestial_mechanics_utilities.R")

## Define a function that calculates temperature at a particular hour using a
## modification of the "linked sin(14R-1) method" from Chow, D. H. C. &
## Levermore, G. J. New algorithm for generating hourly temperature values using
## daily maximum, minimum and average values from climate models: Building
## Services Engineering Research and Technology (2016)
## doi:10.1177/0143624407078642.
calculate_temperature <- function(temp_next, temp_prev, hour, h_next, h_prev) {
	temp <- 0.5 * (temp_next + temp_prev) - 0.5 * (temp_next - temp_prev) * 
            cos(pi * (hour - h_prev) / (h_next - h_prev))
	return(temp)
}

## Define a function that generates an hourly temperature profile using a
## modification of the "linked sin(14R-1) method" from Chow, D. H. C. &
## Levermore, G. J. New algorithm for generating hourly temperature values using
## daily maximum, minimum and average values from climate models: Building
## Services Engineering Research and Technology (2016)
## doi:10.1177/0143624407078642.
generate_hourly_temperature_profile <- function(
    year,
    doy,
    latitude,
    longitude,
    sunrise_threshold_angle,
    t_yesterday_max,
    t_today_min,
    t_today_max,
    t_tomorrow_min
)
{
    # Make sure things are numeric
    year <- as.numeric(year)
    doy <- as.numeric(doy)
    t_yesterday_max <- as.numeric(t_yesterday_max)
    t_today_min <- as.numeric(t_today_min)
    t_today_max <- as.numeric(t_today_max)
    t_tomorrow_min <- as.numeric(t_tomorrow_min)
    
    # Get the previous day
    if (doy == 1) {
        # This is the first day of the year, so the previous day is the last day
        # of the previous year, and we'll need to check for leap years
        yesterday_year <- year - 1
        if (yesterday_year %% 4 == 0) yesterday_doy = 366
        else yesterday_doy = 365
    }
    else {
        yesterday_year <- year
        yesterday_doy <- doy - 1
    }
    
    # Get the next day
    if ((year %% 4 == 0 && doy == 366) || (year %% 4 != 0 && doy == 365)) {
        # This is the last day of the year, so the next day is the first day
        # of the next year
        tomorrow_year <- year + 1
        tomorrow_doy <- 1
    }
    else {
        tomorrow_year <- year
        tomorrow_doy <- doy + 1
    }
    
    # Get the sunset / sunrise / midday / day length info for today and the
    # surrounding days
    yesterday_param <- calculate_sunrise(
        yesterday_year,
        yesterday_doy,
        latitude,
        longitude,
        sunrise_threshold_angle
    )
    
    today_param <- calculate_sunrise(
        year,
        doy,
        latitude,
        longitude,
        sunrise_threshold_angle
    )
    
    tomorrow_param <- calculate_sunrise(
        tomorrow_year,
        tomorrow_doy,
        latitude,
        longitude,
        sunrise_threshold_angle
    )
    
    # Get the hour of yesterday's maximum temperature, which occurs two hours
    # after solar noon. Note: we also need to subtract 24 h, since this hour is
    # from yesterday
    h_yesterday_max <- yesterday_param$hour_transit + 2.0 - 24.0
    
    # Get the hour of today's minimum temperature, which occurs one hour before
    # dawn
    h_today_min <- today_param$sunrise - 1.0
    
    # Get the hour of today's maximum temperature, which occurs two hours after
    # solar noon
    h_today_max <- today_param$hour_transit + 2.0
    
    # Get the hour of tomorrow's minimum temperature, which occurs one hour
    # before dawn. Note: we also need to add 24 h, since this hour is from
    # tomorrow
    h_tomorrow_min <- tomorrow_param$sunrise - 1.0 + 24.0
    
    # Generate a list of hours for today
    hour <- seq(from=0, to=23, by=1)
    
    # Calculate the hourly temperature values
    temp <- hour
    for (i in 1:length(hour)) {
        current_hour <- hour[i]
        if (0 <= current_hour && current_hour <= h_today_min) {
            # We are still in the early morning where temperature is falling
            # from yesterday's max. So prev is prev_max, next is today_min
            temp[i] <- calculate_temperature(
                temp_next=t_today_min,
                temp_prev=t_yesterday_max,
                hour=current_hour,
                h_next=h_today_min,
                h_prev=h_yesterday_max
            )
        }
        else if (h_today_min < current_hour && current_hour <= h_today_max) {
            # Now we are during the warming part of today. So prev is today_min,
            # next is today_max
            temp[i] <- calculate_temperature(
                temp_next=t_today_max,
                temp_prev=t_today_min,
                hour=current_hour,
                h_next=h_today_max,
                h_prev=h_today_min
            )
        }
        else if (h_today_max < current_hour && current_hour < 24) {
            # Now we are in the cooling part of the day. So prev is today_max,
            # next is tomorrow_min
            temp[i] <- calculate_temperature(
                temp_next=t_tomorrow_min,
                temp_prev=t_today_max,
                hour=current_hour,
                h_next=h_tomorrow_min,
                h_prev=h_today_max
            )
        }
    }
    
    # Convert from Fahrenheit to Celsius
    temp <- (temp - 32) * (5 / 9)
    
    # Return a dataframe with the results
    return(data.frame(temp))
}