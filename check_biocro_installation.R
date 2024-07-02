# Some of the scripts in this repository use an old version of BioCro that is
# only available here and must be installed as described in README.md. This
# script just checks to make sure the correct version is installed.

# Load the package
library(BioCro)

# Specify a message to send to the user if an issue is detected
msg <- 'Please install BioCro as described in README.md'

# Check the package version
if (packageVersion('BioCro') != '1.0') {
    stop(msg)
}

# Check to see if the BioCro::test_module function exists
test_module_exists <- tryCatch(
    {
        utils::getFromNamespace('test_module', 'BioCro')
        TRUE
    },
    cond = function(...) {
        FALSE
    }
)

if (!test_module_exists) {
    stop(msg)
}

# Check to make sure the `light_macro_environment` module exists
LME_exists <- tryCatch(
    {
        test_module(
            'light_macro_environment',
            list(
                cosine_zenith_angle = 1.0,
                atmospheric_pressure = 101325,
                atmospheric_transmittance = 0.7
            )
        )
        TRUE
    },
    cond = function(...) {
        FALSE
    }
)

if (!LME_exists) {
    stop(msg)
}
