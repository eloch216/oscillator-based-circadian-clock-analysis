## Define some functions related to solar photon flux density

library(BioCro)

## Define a function that calculates an hourly solar radiation profile based on
## the sun's position and the total daily radiation
generate_hourly_solar_profile <- function(
    hourly_sun_position_profile,
    total_daily_radiation,
    par_percent,
    atmospheric_transmittance,
    atmospheric_pressure
)
{
	# Make a list of inputs to the light macro environment module
	lightME_inputs <- list()
	
	# Specify the atmospheric transmittance and pressure
	lightME_inputs$atmospheric_transmittance <- atmospheric_transmittance
	lightME_inputs$atmospheric_pressure <- atmospheric_pressure
	
	# Initialize transmittance vectors to the correct length
	direct_transmittance <- hourly_sun_position_profile$cosine_zenith_angle
	diffuse_transmittance <- hourly_sun_position_profile$cosine_zenith_angle
    
    # Calculate the transmitted radiation at each hour
	for (i in 1:length(hourly_sun_position_profile$cosine_zenith_angle)) {
		# Update the module inputs
		lightME_inputs$cosine_zenith_angle <- 
            hourly_sun_position_profile$cosine_zenith_angle[i]
		
		# Run the module
		lightME_outputs <- test_module(
            "light_macro_environment",
            lightME_inputs
        )
		
		# Store the transmittance
		direct_transmittance[i] <- 
            lightME_outputs$direct_irradiance_transmittance
        
		diffuse_transmittance[i] <- 
            lightME_outputs$diffuse_irradiance_transmittance
 	}
	
	# Make a "total transmittance" that is zero when direct transmittance is
    # zero, and direct + diffuse otherwise. Here direct should be multiplied by
    # cos zenith angle to represent the flux through a horizontal plane.
	total_transmittance <- direct_transmittance * 
        hourly_sun_position_profile$cosine_zenith_angle + diffuse_transmittance
    
	for (i in 1:length(direct_transmittance)) {
		if (direct_transmittance[i] == 0) total_transmittance[i] = 0
	}
	
	# Make a solar radiation profile representing energy in the PAR band
    # expressed as W/m^2 = J/s/m^2. Assume that par_percent of all solar
    # radiation falls in the PAR band. At each hour, solar should be
    # proportional to total_transmittance. The values should be normalized so
    # that the sum of
    #
	#   solar[i] * (3600 s/hr) * (1 MJ / 1e6 J) * 
    #       (1 J total energy / par_percent J PAR energy) * (1 hour)
    #
	#  adds up to the total daily radiation value, given in MJ/m^2/day
	
	# First normalize so the sum of solar[i] * (1 hour) = 1
	solar <- total_transmittance / sum(total_transmittance)
	
	# Now scale so the sum of solar[i] * (1 hour) =
    #   total_daily_radiation / (3600 * 1e-6 / par_percent),
	#  i.e., so the sum of solar[i] * (3600 * 1e-6 / par_percent) * (1 hour) = 
    # total_daily_radiation
	solar <- solar * as.numeric(total_daily_radiation) / 
                (3600 * 1e-6 / par_percent)
	
	# Now convert to mmol / m^2 / s using 4.6 mmol / W of PAR energy
	solar <- solar * 4.6
	
	# Return the results
	return(data.frame(
        direct_transmittance,
        diffuse_transmittance,
        total_transmittance,
        solar
    ))
}