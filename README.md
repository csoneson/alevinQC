## alevinQC

[![Codecov.io coverage status](https://codecov.io/github/csoneson/alevinQC/coverage.svg?branch=master)](https://codecov.io/github/csoneson/alevinQC)
[![R build status](https://github.com/csoneson/alevinQC/workflows/R-CMD-check/badge.svg)](https://github.com/csoneson/alevinQC/actions)

The `alevinQC` R package provides functionality for generating QC reports
summarizing the output of [alevin](https://salmon.readthedocs.io/en/latest/alevin.html)
([Srivastava et al., Genome Biology 20:65 (2019)](https://genomebiology.biomedcentral.com/articles/10.1186/s13059-019-1670-y)). The reports can
be generated in html or pdf format, or as R/Shiny applications.

### Installation:

`alevinQC` is available from
[Bioconductor](https://bioconductor.org/packages/alevinQC/), and can be
installed using the `BiocManager` CRAN package:

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("alevinQC")
```

Note that `alevinQC` v 1.1 or newer is required to process output from Salmon version 0.14.0 or newer.

### Example usage:

```
alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14", 
                                     package = "alevinQC"),
               sampleId = "testSample", 
               outputFile = "alevinReport.html", 
               outputFormat = "html_document",
               outputDir = tempdir(), forceOverwrite = TRUE)
```

For more information, we refer to the package vignette.

![](https://raw.githubusercontent.com/csoneson/alevinQC/master/inst/extdata/alevinQC_screenshot1.png)
![](https://raw.githubusercontent.com/csoneson/alevinQC/master/inst/extdata/alevinQC_screenshot2.png)
