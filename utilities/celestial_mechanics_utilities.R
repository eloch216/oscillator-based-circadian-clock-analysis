## This file defines some functions used for celestial mechanics calculations

DTR <- pi / 180.0  # degrees to radians
RTD <- 180.0 / pi  # radians to degrees
DTH <- 1.0 / 15.0  # degrees to hours
HTD <- 15.0        # hours to degrees
HPD <- 24.0        # hours per day

## Define a function that determins an ndate from a time (in UTC) using the
## method from Michalsky, J. J. The Astronomical Almanac’s algorithm for
## approximate solar position (1950–2050). Solar Energy 40, 227–235 (1988).
##
## ndate is a time expressed as the number of days since 1 January 2000 (UTC)
calculate_ndate_from_UTC <- function(year, DOY, hour) {
    # Calculate the Julian date
    delta <- as.numeric(year) - 1949
    leap <- floor(0.25 * delta)
    jd <- 2432916.5 + delta * 365 + leap + as.numeric(DOY) +
            as.numeric(hour) / HPD

    # Find the corresponding number of days since 1 January 2000 (UTC)
    ndate <- jd - 2451545.0

    # Return
    return(ndate)
}

## Define a function that calculates solar coordinates from position and time
## using the method described by Michalsky, J. J. The Astronomical Almanac’s
## algorithm for approximate solar position (1950–2050). Solar Energy 40,
## 227–235 (1988).
##
## ndate is a time expressed as the number of days since 1 January 2000 (UTC)
calculate_solar_coordinates <- function(ndate) {
    # Calculate the ecliptic coordinates of the sun (in degrees):
    #  L (mean longitude)
    #  g (mean anomaly)
    #  ell (ecliptic anomaly)
    # Here we assume that the ecliptic latitude beta is zero
    L <- (280.460 + 0.9856474 * ndate) %% 360
    g <- (357.528 + 0.9856003 * ndate) %% 360
    ell <- (L + 1.915 * sin(g * DTR) + 0.020 * sin(2 * g * DTR)) %% 360

    # Calculate the obliquity of the ecliptic (in degrees):
    ep <- 23.439 - 0.0000004 * ndate

    # Calculate the equatorial coordinates of the sun (in degrees):
    #  ra (right ascension; analgous to longitude)
    #  dec (declination; analogous to latitude)
    ra <- atan2(cos(ep * DTR) * sin(ell * DTR), cos(ell * DTR)) * RTD
    dec <- asin(sin(ep * DTR) * sin(ell * DTR)) * RTD

    # Calculate the equation of time (EOT) in days, which expresses the
    # difference between apparent and mean solar time, which should be shifted
    # to the interval [-180,180) and then normalized by 360 degrees
    eot <- (ra - L) %% 360

    if (eot >= 180) {
        eot <- eot - 360
    }

    eot <- eot / 360

    # Return the results
    return(data.frame(L, g, ell, ep, ra, dec, eot))
}

## Define a function that calculates sunrise and sunset times for a given day
## and location following Keisling, T. C. Calculation of the Length of Day.
## Agronomy Journal 74, 758–759 (1982).
calculate_sunrise <- function(
    year,
    doy,
    latitude,
    longitude,
    sunrise_threshold_angle
)
{
    # Calculate the ndate corresponding to mean local noon on this day. Note
    # that longitude / 360 has units of days, so here we need
    # longitude * 24 / 360 when using hours
    n_noon_local <- calculate_ndate_from_UTC(
        year,
        doy,
        12 - longitude * HPD / 360.0
    )

    # Get the solar coordinates at that time
    solar_coordinates <- calculate_solar_coordinates(n_noon_local)

    # Use the sunrise equation (using a variable angular threshold) to determine
    # omega_0, the hour angle (relative to noon) of sunrise (in degrees)
    temp_numer <- sin(sunrise_threshold_angle * DTR) -
                        sin(latitude * DTR) * sin(solar_coordinates$dec * DTR)

    temp_denom <- cos(latitude * DTR) * cos(solar_coordinates$dec * DTR)

    omega_0 <- acos(temp_numer / temp_denom) * RTD

    # The day length is simply twice omega_0, expressed as a number of hours
    day_length <- 2 * omega_0 * DTH

    # Find the local true solar transit
    n_transit <- n_noon_local + solar_coordinates$eot

    # Convert time to CDT. n is referenced to GMT noon, and CDT is UTC-5, so
    # n = 0 => UTC time is 12 => CDT time is 7
    hour_transit <- (n_transit * HPD + 7) %% HPD

    # Calculate sunrise and sunset
    sunrise <- hour_transit - omega_0 * DTH
    sunset <- hour_transit + omega_0 * DTH

    # Calculate the angular diameter of the sun (in degrees) using the distance
    # and diameter of the sun (in AU)
    solar_distance <- 1.00014 - 0.01671 * cos(solar_coordinates$g * DTR) -
                            0.00014 * cos(2 * solar_coordinates$g * DTR)

    solar_diameter <- 0.00929826069

    solar_angular_diameter <- 2 *
            asin(solar_diameter / (2 * solar_distance)) * RTD

    # Return the results
    return(as.data.frame(list(
        day_length = day_length,
        sunrise = sunrise,
        sunset = sunset,
        hour_transit = hour_transit,
        solar_angular_diameter = solar_angular_diameter,
        solar_coordinates
    )))
}

## Define a function that calculates day length throughout a year based on
## location using the 'calculate_sunrise' function
calculate_day_length <- function(
    year,
    longitude,
    latitude,
    sunrise_threshold_angle
)
{
    # Set up the DOY list
    if (year %% 4 == 0) {
        # This is a leap year
        doy <- seq(from=1, to=366, by=1)
    }
    else {
        doy <- seq(from=1, to=365, by=1)
    }

    # Get the info for the first day
    result <- calculate_sunrise(
        year,
        doy[1],
        latitude,
        longitude,
        sunrise_threshold_angle
    )

    # Get the info for the remaining days
    for (i in 2:length(doy)) {
        result <- rbind(
            result,
            calculate_sunrise(
                year,
                doy[i],
                latitude,
                longitude,
                sunrise_threshold_angle
            )
        )
    }

    # Add some more info
    result$year <- year
    result$doy <- doy
    result$sunrise_threshold_angle <- sunrise_threshold_angle

    # Return the result
    return(result)
}

## Define a function that calculates an hourly sun position profile for a single
## day using the method described by Michalsky, J. J. The Astronomical Almanac’s
## algorithm for approximate solar position (1950–2050). Solar Energy 40,
## 227–235 (1988).
generate_hourly_sun_position_profile <- function(
    longitude,
    latitude,
    year,
    doy
)
{
    # Calculate the ndate value at noon on the first day of the year (in UTC)
    jan1_offset <- calculate_ndate_from_UTC(year, 1, 12)

    # Assume that a day goes from hour = 0 to hour 23 in CDT (central daylight
    # time), which is UTC - 5. So hour 0 of January 1, 2000 in Urbana is hour 5
    # of January 1, 2000 Greenwich time, which is 7 hours before noon (UTC).
    #
    # Use this to determine the number of days since Greenwich noon, Terrestrial
    # Time, on 1 January 2000 at the start of this day (n).
    #
    # n is related to the Julian date (jd) by n = jd - 2451545.0.
    #
    # Note: UTC and Julian days measure mean solar time.
    #
    # Also, we should probably calculate the sun's position at the middle of the
    # hourly interval, i.e., the value at h should be the average over the time
    # period from h-1 to h, which we will approximate as the value at h-0.5
    n_start <- jan1_offset + (doy - 1) - 7.5 / HPD

    # Create a vector of n values for each hour of this day
    n <- seq(from=n_start, by=1.0/HPD, length.out=HPD)

    # Calculate solar coordinates at each n value
    solar_coordinates <- calculate_solar_coordinates(n[1])
    for (i in 2:length(n)) {
        temp <- calculate_solar_coordinates(n[i])
        solar_coordinates <- rbind(solar_coordinates, temp)
    }

    # Calculate the hour in UTC
    hour_utc <- (n * HPD + 12.0) %% HPD

    # Calculate Greenwich mean sidereal time (GMST) in hours
    GMST <- (6.697375 + 0.0657098242 * n + hour_utc) %% HPD

    # Calculate local mean sidereal time (LMST) in hours
    LMST <- (GMST + longitude * DTH) %% HPD

    # Calculate the local hour angle of the sun in degrees
    LHA <- (LMST * HTD - solar_coordinates$ra) %% 360

    # Calculate the zenith angle (in degrees):
    zenith_angle <- acos(
        sin(solar_coordinates$dec * DTR) * sin(latitude * DTR) +
         cos(solar_coordinates$dec * DTR) * cos(latitude * DTR) * cos(LHA * DTR)
    ) * RTD

    # Convert the zenith angle to the elevation angle
    elevation <- 90 - zenith_angle

    # Determine the refraction correction based on the elevation angle
    refrac <- zenith_angle
    for (i in 1:length(elevation)) {
        if (elevation[i] >= 15) {
            # Use the simple formula for large elevation
            refrac[i] <- 0.00452 * 3.51561 / tan(elevation[i] * DTR)
        }
        else if (elevation[i] <= -3) {
            # No need to apply refraction correction when the sun is far below
            # the horizon
            refrac[i] <- 0
        }
        else {
            # Use the complicated version
            refrac[i] <- 3.51561 *
                (0.1594 + 0.0196 * elevation[i] + 0.0002 * elevation[i]^2) /
                (1 + 0.505 * elevation[i] + 0.0845 * elevation[i]^2)
        }
    }
    zenith_angle <- zenith_angle - refrac
    cosine_zenith_angle <- cos(zenith_angle * DTR)

    # Convert time to CDT at the end of the time interval
    hour <- (n * HPD + 7.5) %% HPD

    # Return the results
    return(data.frame(year, doy, hour, zenith_angle, cosine_zenith_angle))
}