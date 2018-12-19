## alevinQC

The `alevinQC` R package provides functionality for generating QC reports
summarizing the output of [Alevin](https://github.com/COMBINE-lab/salmon)
([Srivastava et al., bioRxiv doi:10.1101/335000,
2018](https://www.biorxiv.org/content/early/2018/10/24/335000)). The reports can
be generated in html or pdf format, or as R/Shiny applications.

### Installation:

```
BiocManager::install("csoneson/alevinQC")
```

### Basic usage:

```
alevinQCReport(baseDir = "<path_to_salmon_dir>", 
               sampleId = "<sample_name>", 
               outputFile = "myTestReport.html")
```

For more information, we refer to the package vignette.
