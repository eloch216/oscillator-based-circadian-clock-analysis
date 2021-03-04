#include <algorithm>  // for std::transform
#include <cctype>     // for std::tolower
#include "module_wrapper_factory.h"

// Include all the header files that define the modules.
#include "gaussian_solar.hpp"
#include "grimm_soybean_flowering.h"
#include "grimm_soybean_flowering_calculator.h"
#include "harmonic_oscillator.hpp"  // Contains harmonic_oscillator and harmonic_energy
#include "incident_shortwave_from_ground_par.h"
#include "light_macro_environment.hpp"
#include "light_from_solar.hpp"
#include "magic_clock.hpp"
#include "module_graph_test.hpp"  // Includes Module_1, Module_2, and Module_3
#include "night_and_day_trackers.hpp"
#include "oscillator_clock_calculator.hpp"
#include "oscillator_clock_calculator_sk.hpp"
#include "phase_clock.hpp"
#include "poincare_clock.hpp"
#include "pokhilko_circadian_clock.hpp"
#include "shortwave_atmospheric_scattering.h"
#include "thermal_time_beta.h"
#include "thermal_time_bilinear.h"
#include "thermal_time_linear.h"
#include "thermal_time_linear_extended.h"
#include "thermal_time_trilinear.h"

/**
 * @brief A function that returns a unique_ptr to a module_wrapper_base object.
 */
template <typename T>
std::unique_ptr<module_wrapper_base> create_wrapper()
{
    return std::unique_ptr<module_wrapper_base>(new module_wrapper<T>);
}

std::unique_ptr<module_wrapper_base> module_wrapper_factory::create(std::string const& module_name)
{
    try {
        return module_wrapper_factory::module_wrapper_creators.at(module_name)();
    } catch (std::out_of_range) {
        std::string message = std::string("\"") + module_name +
                              std::string("\"") +
                              std::string(" was given as a module name, ") +
                              std::string("but no module with that name could be found.\n");

        throw std::out_of_range(message);
    }
}

module_wrapper_factory::module_wrapper_creator_map module_wrapper_factory::module_wrapper_creators =
{
     {"gaussian_solar",                                  &create_wrapper<gaussian_solar>},
     {"grimm_soybean_flowering",                         &create_wrapper<grimm_soybean_flowering>},
     {"grimm_soybean_flowering_calculator",              &create_wrapper<grimm_soybean_flowering_calculator>},
     {"harmonic_energy",                                 &create_wrapper<harmonic_energy>},
     {"harmonic_oscillator",                             &create_wrapper<harmonic_oscillator>},
     {"incident_shortwave_from_ground_par",              &create_wrapper<incident_shortwave_from_ground_par>},
     {"light_macro_environment",                         &create_wrapper<light_macro_environment>},
     {"light_from_solar",                                &create_wrapper<light_from_solar>},
     {"magic_clock",                                     &create_wrapper<magic_clock>},
     {"Module_1",                                        &create_wrapper<Module_1>},
     {"Module_2",                                        &create_wrapper<Module_2>},
     {"Module_3",                                        &create_wrapper<Module_3>},
     {"night_and_day_trackers",                          &create_wrapper<night_and_day_trackers>},
     {"oscillator_clock_calculator",                     &create_wrapper<oscillator_clock_calculator>},
     {"oscillator_clock_calculator_sk",                  &create_wrapper<oscillator_clock_calculator_sk>},
     {"phase_clock",                                     &create_wrapper<phase_clock>},
     {"poincare_clock",                                  &create_wrapper<poincare_clock>},
     {"pokhilko_circadian_clock",                        &create_wrapper<pokhilko_circadian_clock>},
     {"shortwave_atmospheric_scattering",                &create_wrapper<shortwave_atmospheric_scattering>},
     {"thermal_time_beta",                               &create_wrapper<thermal_time_beta>},
     {"thermal_time_bilinear",                           &create_wrapper<thermal_time_bilinear>},
     {"thermal_time_linear",                             &create_wrapper<thermal_time_linear>},
     {"thermal_time_linear_extended",                    &create_wrapper<thermal_time_linear_extended>},
     {"thermal_time_trilinear",                          &create_wrapper<thermal_time_trilinear>}
};

std::vector<std::string> module_wrapper_factory::get_modules()
{
    std::vector<std::string> module_name_vector;
    for (auto const& x : module_wrapper_creators) {
        module_name_vector.push_back(x.first);
    }

    auto case_insensitive_compare = [](std::string const& a, std::string const& b) {
        // Make a lowercase copy of a
        std::string al = a;
        std::transform(al.begin(), al.end(), al.begin(), [](unsigned char c) { return std::tolower(c); });

        // Make a lowercase copy of b
        std::string bl = b;
        std::transform(bl.begin(), bl.end(), bl.begin(), [](unsigned char c) { return std::tolower(c); });

        int compare = al.compare(bl);
        return (compare > 0) ? false : true;
    };

    std::sort(module_name_vector.begin(), module_name_vector.end(), case_insensitive_compare);

    return module_name_vector;
}

std::unordered_map<std::string, std::vector<std::string>> module_wrapper_factory::get_all_quantities()
{
    // Make the output map
    std::unordered_map<std::string, std::vector<std::string>> quantity_map = {
        {"module_name", std::vector<std::string>{}},
        {"quantity_type", std::vector<std::string>{}},
        {"quantity_name", std::vector<std::string>{}}};

    // Make a lambda function for adding entries to the map
    auto add_quantity_map_entry = [&quantity_map](std::string module_name, std::string quantity_type, std::string quantity_name) {
        quantity_map["module_name"].push_back(module_name);
        quantity_map["quantity_type"].push_back(quantity_type);
        quantity_map["quantity_name"].push_back(quantity_name);
    };

    // Fill the output map with all the quantities
    for (std::string const& module_name : module_wrapper_factory::get_modules()) {
        auto w = module_wrapper_factory::create(module_name);

        // Add the module's inputs to the parameter map
        for (std::string const& input_name : w->get_inputs()) {
            add_quantity_map_entry(module_name, "input", input_name);
        }

        // Add the module's outputs to the parameter map
        for (std::string const& output_name : w->get_outputs()) {
            add_quantity_map_entry(module_name, "output", output_name);
        }
    }

    return quantity_map;
}
