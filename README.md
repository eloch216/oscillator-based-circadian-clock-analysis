## oscillator-based-circadian-clock-analysis
This repository includes R scripts and input data that were used to produce
the figures in the manuscript "Integrating oscillator-based circadian clocks
with crop growth simulations," which has been submitted to
[*in silico* Plants](https://academic.oup.com/insilicoplants).

These scripts have been tested using R version 4.0.3 and RTools version 4.0.0
running on Windows 10 Enterprise version 20H2.

All outputs were generated on March 4, 2021 and stored in the
```output_archive``` directory.

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

#### A word of caution
- These scripts have not been optimized for efficiency, and some of the
operations may take a long time to run
