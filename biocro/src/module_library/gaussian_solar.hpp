#ifndef GAUSSIAN_SOLAR_H
#define GAUSSIAN_SOLAR_H

#include "../modules.h"

/** Class created by EBL for testing and experimentation ... not meant for real simulations! */
class gaussian_solar : public SteadyModule
{
   public:
    gaussian_solar(
        const std::unordered_map<std::string, double>* input_parameters,
        std::unordered_map<std::string, double>* output_parameters)
        : SteadyModule("gaussian_solar"),

          // Get pointers to input parameters
          doy_dbl_ip(get_ip(input_parameters, "doy_dbl")),
          target_doy_dbl_ip(get_ip(input_parameters, "target_doy_dbl")),
          new_dl_ip(get_ip(input_parameters, "new_dl")),

          // Get pointers to output parameters
          solar_op(get_op(output_parameters, "solar"))
    {
    }
    static std::vector<std::string> get_inputs();
    static std::vector<std::string> get_outputs();

   private:
    // Pointers to input parameters
    const double* doy_dbl_ip;
    const double* target_doy_dbl_ip;
    const double* new_dl_ip;

    // Pointers to output parameters
    double* solar_op;

    // Main operation
    void do_operation() const;
};

std::vector<std::string> gaussian_solar::get_inputs()
{
    return {
        "doy_dbl",
        "target_doy_dbl",
        "new_dl"};
}

std::vector<std::string> gaussian_solar::get_outputs()
{
    return {
        "solar"};
}

void gaussian_solar::do_operation() const
{
    //////////////////////////////////////////
    // Collect inputs and make calculations //
    //////////////////////////////////////////

    // Get the current doy_dbl
    double doy_dbl = *doy_dbl_ip;

    // Extract the hour
    double hour = 24.0 * (doy_dbl - floor(doy_dbl));

    // Get the target doy to change the day length from 11 to a new value
    double target_doy_dbl = *target_doy_dbl_ip;
    double new_dl = *new_dl_ip;

    // Calculate the radiation
    double max_solar = 1000.0;

    double day_length;
    if (doy_dbl < target_doy_dbl) {
        day_length = 11.0;
    } else {
        day_length = new_dl;
    }

    double sigma = day_length / 4.0;  // In hours

    double solar = max_solar * exp(-(hour - 12.0) * (hour - 12.0) / (sigma * sigma));

    //////////////////////////////////////
    // Update the output parameter list //
    //////////////////////////////////////

    update(solar_op, solar);
}

#endif
