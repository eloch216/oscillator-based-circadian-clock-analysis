## oscillator-based-circadian-clock-analysis
This repository includes R scripts and input data that were used to produce
the figures in the manuscript "Integrating oscillator-based circadian clocks
with crop growth simulations," which has been submitted to
[*in silico* Plants](https://academic.oup.com/insilicoplants).

These scripts have been tested using R version 4.0.3 and RTools version 4.0.0
running on Windows 10 Enterprise version 20H2.

All outputs were generated using these scripts and stored in the
```output_archive``` directory, with the exception of any RData files exceeding
100 MB in size:
- ```output_archive/RData/cropgro_astro_comparison_cross.RData```
- ```output_archive/RData/cropgro_astro_comparison.RData```
- ```output_archive/RData/figure_4b.RData```

Figures in the manuscript were produced from the PDFs in
```output_archive/figures``` using Adobe Illustrator to perform the final
combinations, recoloring, annotations, etc.

### Reproducing the outputs

#### Requirements
- The [R environment](https://cran.r-project.org/)
- On Windows, [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
- On MacOS, [Xcode](https://developer.apple.com/xcode/)

#### Steps
- Start a fresh R session and set the working directory to this one
- Install the BioCro package:
  ```install.packages('biocro', repos=NULL, type='SOURCE')```
- Run the main script: ```source('run_all.R')```
- All outputs (figures, tabular data, and R binary data files) will be generated
  in a new ```output``` directory, whose content should be identical to the
  ```output_archive``` directory provided with the repository

#### Alternate method
- The main script (```run_all.R```) simply calls each of the ```analyze_NNN.R```
  scripts, where `NNN` is a phrase that signifies one part of the calculations
- As an alternative to sourcing that script, individual analysis scripts can
  be sourced instead to produce a subset of all outputs
- Within individual analysis scripts, additional control is possible using the
  booleans defined at the start of each one

### Words of caution
- These scripts have not been optimized for efficiency, and some of the
  operations may take a long time to run
- Setting ```SAVE_TO_FILE = FALSE``` on MacOS or Linux requires an X server to
  be installed and running, since doing so will result in calls to the
  ```x11()``` R function
