## Run some functions to get info about the weather data

library(RColorBrewer)
library(lattice)

source("set_defaults.R")
source("utilities/EdR.R")
source("utilities/weather_utilities.R")

## Choose which operations to perform
ANALYZE_SURFRAD <- TRUE         # produces table s1 and figure s1
ANALYZE_WARM <- TRUE            # produces table s2 and figure s2
COMPARE_WEATHER_SETS <- TRUE    # produces figures s3 and s4
SAVE_TO_FILE <- TRUE

## Define a function that gets stats about the type of solar source during a
## year of surfrad data.
## There are three possible values of solar_source: 1, 2, and 3
count_solar_sources_surfrad <- function(biocro_surfrad_data) {
    # Get info
    source_counts <- numeric(3)
    for (i in 1:length(biocro_surfrad_data$solar_source)) {
        current_solar_source <- biocro_surfrad_data$solar_source[i]
        source_counts[current_solar_source] <-
                source_counts[current_solar_source] + 1
    }
    total_counts <- sum(source_counts)

    # Make list
    source_count_info <- list(
        from_par=source_counts[1],
        from_par_percent=100*source_counts[1]/total_counts,
        from_pyranometer=source_counts[2],
        from_pyranometer_percent=100*source_counts[2]/total_counts,
        from_interpolation=source_counts[3],
        from_interpolation_percent=100*source_counts[3]/total_counts,
        total_counts=total_counts
    )

    # Return list
    return(source_count_info)
}

## Define a function that gets stats about the type of solar source during a
## year of warm data.
## There are two possible values of solar_source: 1 and 2
count_solar_sources_warm <- function(biocro_warm_data) {
    # Get info
    source_counts <- numeric(2)
    for (i in 1:length(biocro_warm_data$solar_source)) {
        current_solar_source <- biocro_warm_data$solar_source[i]
        source_counts[current_solar_source] <-
                source_counts[current_solar_source] + 1
    }
    total_counts <- sum(source_counts)

    # By counting hourly values, we are mis-representing the number of
    # experimental data points. So divide all counts by 24.
    source_counts <- source_counts / 24
    total_counts <- total_counts / 24

    # Make list
    source_count_info <- list(
        from_pyranometer=source_counts[1],
        from_pyranometer_percent=100*source_counts[1]/total_counts,
        from_interpolation=source_counts[2],
        from_interpolation_percent=100*source_counts[2]/total_counts,
        total_counts=total_counts
    )

    # Return list
    return(source_count_info)
}

## Define a function that gets stats for each year of surfrad data
analyze_surfrad_solar_source <- function() {
    # Make the list of years
    years <- 1995:2019

    # Get the weather data from the first year
    command <- paste("weather_data <- surfrad", years[1], sep="")
    eval(parse(text=command))

    # Get the info about the first year
    result <- as.data.frame(
        count_solar_sources_surfrad(convert_to_biocro_surfrad(weather_data)),
        stringsAsFactors=FALSE
    )

    # Get the info from the remaining years
    for (i in 2:length(years)) {
        # Get the weather data for this year
        command <- paste("weather_data <- surfrad", years[i], sep="")
        eval(parse(text=command))

        # Get the info
        temp_result <- as.data.frame(
            count_solar_sources_surfrad(
                convert_to_biocro_surfrad(weather_data)
            ),
            stringsAsFactors=FALSE
        )

        # Add it to the result
        result <- rbind(result, temp_result)
    }

    # Add the year column
    result$year = years

    # Return the result
    return(result)
}

## Define a function that gets stats for each year of warm data
analyze_warm_solar_source <- function() {
    # Make the list of years
    years <- 1989:2019

    # Get the weather data from the first year
    command <- paste("weather_data <- warm", years[1], sep="")
    eval(parse(text=command))

    # Get the info about the first year
    result <- as.data.frame(
        count_solar_sources_warm(convert_to_biocro_warm(weather_data)),
        stringsAsFactors=FALSE
    )

    # Get the info from the remaining years
    for (i in 2:length(years)) {
        # Get the weather data for this year
        command <- paste("weather_data <- warm", years[i], sep="")
        eval(parse(text=command))

        # Get the info
        temp_result <- as.data.frame(
            count_solar_sources_warm(convert_to_biocro_warm(weather_data)),
            stringsAsFactors=FALSE
        )

        # Add it to the result
        result <- rbind(result, temp_result)
    }

    # Add the year column
    result$year = years

    # Return the result
    return(result)
}

if (ANALYZE_SURFRAD) {
    # Analyze the surfrad data
    surfrad_info <- analyze_surfrad_solar_source()

    # Save the result as a csv file
    if (SAVE_TO_FILE) {
        write.csv(
            surfrad_info,
            file=paste0(TABLE_DIR, "/table_s1.csv"),
            row.names=FALSE
        )
    }

    # Extract the percentages
    surfrad_percents <- EdR.extract(
        surfrad_info,
        c("year"),
        c(
            "from_par_percent",
            "from_pyranometer_percent",
            "from_interpolation_percent"
        )
    )

    # Make the plot
    cols <- brewer.pal(11, "RdYlBu")

    pal <- colorRampPalette(cols)

    surfrad_solar_source_plot <- barchart(
        value ~ factor(year),
        group=value_type,
        data=surfrad_percents,
        stack=TRUE,
        grid=TRUE,
        auto.key=list(text=c("Interpolation", "PAR", "Total solar irradiance")),
        scales = list(x = list(rot = 45)),
        par.settings=list(superpose.polygon=list(col=pal(3))),
        ylab="Percentage of SUFRAD solar data from each source"
    )

    # Display or save the plot
    EdR.plot(
        surfrad_solar_source_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s1.pdf"),
        width=6,
        height=6
    )
}

if (ANALYZE_WARM) {
    # Analyze the warm data
    warm_info <- analyze_warm_solar_source()

    # Save the result as a csv file
    if (SAVE_TO_FILE) {
        write.csv(
            warm_info,
            file=paste0(TABLE_DIR, "/table_s2.csv"),
            row.names=FALSE
        )
    }

    # Extract the percentages
    warm_percents <- EdR.extract(
        warm_info,
        c("year"),
        c("from_pyranometer_percent", "from_interpolation_percent")
    )

    # Make the plot
    cols <- brewer.pal(11, "RdYlBu")

    pal <- colorRampPalette(cols)

    warm_solar_source_plot <- barchart(
        value ~ factor(year),
        group=value_type,
        data=warm_percents,
        stack=TRUE,
        grid=TRUE,
        auto.key=list(text=c("Interpolation", "Insolation")),
        scales = list(x = list(rot = 45)),
        par.settings=list(superpose.polygon=list(col=pal(3))),
        ylab="Percentage of WARM solar data from each source"
    )

    # Display or save the plot
    EdR.plot(
        warm_solar_source_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s2.pdf"),
        width=6,
        height=6
    )
}

if (COMPARE_WEATHER_SETS) {

    # Use EdR.extract to extract some columns from the WARM and SURFRAD data for
    # 2019
    time_cols <- c("doy", "hour")
    data_cols <- c("solar", "temp")

    warm_subset_2019 <- EdR.extract(
        convert_to_biocro_warm(warm2019),
        time_cols,
        data_cols
    )

    surfrad_subset_2019 <- EdR.extract(
        convert_to_biocro_surfrad(surfrad2019),
        time_cols,
        data_cols
    )

    # Combine the extracted data into one data frame
    weather_combo <- EdR.merge(
        list(warm_subset_2019, surfrad_subset_2019),
        list("WARM", "SURFRAD"),
        "dataset"
    )

    # Plot the daily solar data
    daily_solar_plot <- xyplot(
        value ~ hour | factor(doy),
        group=factor(dataset),
        data=weather_combo[which(weather_combo$value_type=="solar"),],
        xlab="Hour",
        ylab="PPFD (umol / s / m^2)",
        type='l',
        auto=TRUE,
        xlim=c(0,24)
    )

	EdR.plot(
        daily_solar_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s3.pdf"),
        width=12,
        height=12
    )

    # Plot the daily temperature data
    daily_temp_plot <- xyplot(
        value ~ hour | factor(doy),
        group=factor(dataset),
        data=weather_combo[which(weather_combo$value_type=="temp"),],
        xlab="Hour",
        ylab="Temperature (deg C)",
        type='l',
        auto=TRUE,
        xlim=c(0,24)
    )

	EdR.plot(
        daily_temp_plot,
        SAVE_TO_FILE,
        paste0(FIGURE_DIR, "/figure_s4.pdf"),
        width=12,
        height=12
    )

}
