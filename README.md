
# PhaseX.2 Quality Control R package

The goal of PhaseX.2QCpackage is to perform Quality Control on data
generated for 4CE phase 1.2 and phase 2.2.

## Installation

``` r
devtools::install_github("https://github.com/covidclinical/PhaseX.2QualityControlRpackage", upgrade=FALSE)
```

## Running QC

``` r
### enter input path, output path and site name
dir.input1.2 = ""   # path that contains phase 1.2 data
dir.input2.2 = ""   # path that contains phase 2.2 data, write NA if 2.2 data is not available
dir.output = ""  # path to save the 1.2 QC report
site.nm = ""     # 4CE site ID


### run QC
PhaseX.2QualityControlRpackage::runQC(dir.input1.2, dir.input2.2, dir.output, site.nm)
```
