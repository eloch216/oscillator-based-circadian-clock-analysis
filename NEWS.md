## VERSION 1.2.0 (tagged v1.2.0)

- Added 2021 and 2022 weather data
- Changed weather time zone to CST (from CDT)
- Changed the meaning of the weather data value at hour `h` to represent the
  average over an hour-long interval centered at `h`, rather than ending on `h`
- The weather data stored here in `output_archive/RData/cmi_weather_data.RData`
  is included with the `BioCro` R package

## VERSION 1.1.0 (tagged v1.1.0)

- Added 2020 weather data
- Included precipitation in the processed WARM data
- The weather data here was used in the BioCro II paper

## VERSION 1.0.0 (tagged v1.0.0)

- This version contains the scripts and input data files used to generate all
  the figures in the oscillator clock paper, as well as all the raw figures
