# Define some functions related to precipitation

# Convert the daily total rainfall to inches and assume that it falls at an even
# rate throughout the day so the values at each hour are identical
generate_hourly_precipitation_profile <- function(daily_total_in) {
    return(data.frame(
        precip = seq(from=1, to=1, length.out=24) * daily_total_in * 25.4 / 24.0
    ))
}
