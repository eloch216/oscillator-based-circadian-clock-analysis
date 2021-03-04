#ifndef OSCILLATOR_CLOCK_CALCULATOR_SK_H
#define OSCILLATOR_CLOCK_CALCULATOR_SK_H

#include <cmath>
#include "../constants.h"
#include "../modules.h"
#include "oscillator_clock_calculator.hpp"  // for range_adjusted_atan2

// This class is identical to the "oscillator_clock_calculator" except that it
// uses a sinusoidal kick for the dawn oscillator rather than calculating the
// kick from the trackers
class oscillator_clock_calculator_sk : public SteadyModule
{
   public:
    oscillator_clock_calculator_sk(
        const std::unordered_map<std::string, double>* input_parameters,
        std::unordered_map<std::string, double>* output_parameters)
        :  // Define basic module properties by passing its name to its parent class
          SteadyModule("oscillator_clock_calculator_sk"),

          // Get pointers to input parameters
          doy_dbl_ip(get_ip(input_parameters, "doy_dbl")),
          kick_strength_ip(get_ip(input_parameters, "kick_strength")),
          kick_start_ip(get_ip(input_parameters, "kick_start")),
          kick_end_ip(get_ip(input_parameters, "kick_end")),
          dawn_b_ip(get_ip(input_parameters, "dawn_b")),
          dawn_a_ip(get_ip(input_parameters, "dawn_a")),
          dusk_b_ip(get_ip(input_parameters, "dusk_b")),
          dusk_a_ip(get_ip(input_parameters, "dusk_a")),
          ref_b_ip(get_ip(input_parameters, "ref_b")),
          ref_a_ip(get_ip(input_parameters, "ref_a")),

          // Get pointers to output parameters
          dawn_kick_op(get_op(output_parameters, "dawn_kick")),
          dusk_kick_op(get_op(output_parameters, "dusk_kick")),
          dawn_phase_op(get_op(output_parameters, "dawn_phase")),
          dusk_phase_op(get_op(output_parameters, "dusk_phase")),
          ref_phase_op(get_op(output_parameters, "ref_phase")),
          dawn_radius_op(get_op(output_parameters, "dawn_radius")),
          dusk_radius_op(get_op(output_parameters, "dusk_radius")),
          ref_radius_op(get_op(output_parameters, "ref_radius")),
          day_length_op(get_op(output_parameters, "day_length")),
          night_length_op(get_op(output_parameters, "night_length")),
          sunrise_op(get_op(output_parameters, "sunrise")),
          sunset_op(get_op(output_parameters, "sunset"))
    {
    }
    static std::vector<std::string> get_inputs();
    static std::vector<std::string> get_outputs();

   private:
    // Pointers to input parameters
    const double* doy_dbl_ip;
    const double* kick_strength_ip;
    const double* kick_start_ip;
    const double* kick_end_ip;
    const double* dawn_b_ip;
    const double* dawn_a_ip;
    const double* dusk_b_ip;
    const double* dusk_a_ip;
    const double* ref_b_ip;
    const double* ref_a_ip;

    // Pointers to output parameters
    double* dawn_kick_op;
    double* dusk_kick_op;
    double* dawn_phase_op;
    double* dusk_phase_op;
    double* ref_phase_op;
    double* dawn_radius_op;
    double* dusk_radius_op;
    double* ref_radius_op;
    double* day_length_op;
    double* night_length_op;
    double* sunrise_op;
    double* sunset_op;

    // Main operation
    void do_operation() const;
};

std::vector<std::string> oscillator_clock_calculator_sk::get_inputs()
{
    return {
        "doy_dbl",
        "kick_strength",
        "kick_start",
        "kick_end",
        "dawn_b",
        "dawn_a",
        "dusk_b",
        "dusk_a",
        "ref_b",
        "ref_a"};
}

std::vector<std::string> oscillator_clock_calculator_sk::get_outputs()
{
    return {
        "dawn_kick",
        "dusk_kick",
        "dawn_phase",
        "dusk_phase",
        "ref_phase",
        "dawn_radius",
        "dusk_radius",
        "ref_radius",
        "day_length",
        "night_length",
        "sunrise",
        "sunset"};
}

void oscillator_clock_calculator_sk::do_operation() const
{
    //////////////////////////////////////////
    // Collect inputs and make calculations //
    //////////////////////////////////////////

    using math_constants::pi;

    // Get the current time value
    const double doy_dbl = *doy_dbl_ip;
    const double hour = 24.0 * (doy_dbl - floor(doy_dbl));

    // Get the kick strength and its starting and ending doy values
    const double kick_strength = *kick_strength_ip;
    const double kick_start = *kick_start_ip;
    const double kick_end = *kick_end_ip;

    // Calculate the kicks using a sinusoidal driving force with a period
    //  of 24 hours that runs for kick_start <= DOY <= kick_end
    double dawn_kick = 0;
    double dusk_kick = 0;
    if (doy_dbl >= kick_start && doy_dbl <= kick_end) {
        dawn_kick = kick_strength * sin(doy_dbl * 2 * pi);
    }

    // Get the current state of the dawn and dusk tracking oscillators
    const double dawn_b = *dawn_b_ip;
    const double dawn_a = *dawn_a_ip;
    const double dusk_b = *dusk_b_ip;
    const double dusk_a = *dusk_a_ip;
    const double ref_b = *ref_b_ip;
    const double ref_a = *ref_a_ip;

    // Calculate the dawn phase angle, which is zero around dawn and increases
    // throughout the day.
    const double dawn_phase = range_adjusted_atan2(dawn_b, dawn_a);

    // Calculate the dusk phase angle, which is zero around dusk and increases
    // throughout the night.
    const double dusk_phase = range_adjusted_atan2(dusk_b, dusk_a);

    // Calculate the reference phase angle, which is not coupled to the light.
    const double ref_phase = range_adjusted_atan2(ref_b, ref_a);

    // Calculate the day and night length indicators (in hours):
    const double day_length = dusk_phase > dawn_phase ? (dawn_phase - dusk_phase + 2 * pi) * 12.0 / pi
                                                      : (dawn_phase - dusk_phase) * 12.0 / pi;
    const double night_length = dawn_phase > dusk_phase ? (dusk_phase - dawn_phase + 2 * pi) * 12.0 / pi
                                                        : (dusk_phase - dawn_phase) * 12.0 / pi;

    // Calculate the sunrise and sunset times
    const double sunrise = dawn_phase * 12 / pi < hour ? (hour - dawn_phase * 12 / pi)
                                                       : (hour - dawn_phase * 12 / pi + 24.0);
    const double sunset = dusk_phase * 12 / pi < hour ? (hour - dusk_phase * 12 / pi)
                                                      : (hour - dusk_phase * 12 / pi + 24.0);

    //////////////////////////////////////
    // Update the output parameter list //
    //////////////////////////////////////

    update(dawn_kick_op, dawn_kick);
    update(dusk_kick_op, dusk_kick);
    update(dawn_phase_op, dawn_phase);
    update(dusk_phase_op, dusk_phase);
    update(ref_phase_op, ref_phase);
    update(dawn_radius_op, sqrt(dawn_a * dawn_a + dawn_b * dawn_b));
    update(dusk_radius_op, sqrt(dusk_a * dusk_a + dusk_b * dusk_b));
    update(ref_radius_op, sqrt(ref_a * ref_a + ref_b * ref_b));
    update(day_length_op, day_length);
    update(night_length_op, night_length);
    update(sunrise_op, sunrise);
    update(sunset_op, sunset);
}

#endif
