context("test-alevin-v0.14")

test_that("checking for input files works", {
    ## All required files available - check should pass
    expect_equal(checkAlevinInputFiles(system.file("extdata/alevin_example_v0.14",
                                                   package = "alevinQC")),
                 "v0.14")

    ## Remove one file - check should fail
    tmp <- tempdir()
    file.copy(from = system.file("extdata/alevin_example_v0.14",
                                 package = "alevinQC"),
              to = tmp, overwrite = TRUE, recursive = TRUE)
    file.remove(file.path(tmp, "alevin_example_v0.14/cmd_info.json"))
    expect_error(checkAlevinInputFiles(file.path(tmp, "alevin_example_v0.14")))
})

## Read provided example input files for tests of file reading/plotting
alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
                                   package = "alevinQC"))

test_that("reading input files works", {
    expect_length(alevin, 3)
    expect_is(alevin, "list")
    expect_named(alevin, c("cbTable", "versionTable", "summaryTables"))

    expect_equal(nrow(alevin$cbTable), 188613)
    expect_equal(sum(alevin$cbTable$inFirstWhiteList), 100)
    expect_equal(sum(!is.na(alevin$cbTable$mappingRate)), 298)
    expect_equal(sum(alevin$cbTable$inFinalWhiteList), 95)
})

test_that("plots are generated", {
    expect_is(plotAlevinKneeRaw(alevin$cbTable), "ggplot")
    expect_is(plotAlevinBarcodeCollapse(alevin$cbTable), "ggplot")
    expect_is(plotAlevinQuantPairs(alevin$cbTable), "ggmatrix")
    expect_is(plotAlevinKneeNbrGenes(alevin$cbTable), "ggplot")
})

tempDir <- tempdir()
if (file.exists(file.path(tempDir, "tmp.Rmd"))) {
    file.remove(file.path(tempDir, "tmp.Rmd"))
}
if (file.exists(file.path(tempDir, "tmp.html"))) {
    file.remove(file.path(tempDir, "tmp.html"))
}
if (file.exists(file.path(tempDir, "tmp.pdf"))) {
    file.remove(file.path(tempDir, "tmp.pdf"))
}

test_that("input arguments are processed correctly", {
    ## outputFormat
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                outputFormat = "html", outputFile = "tmp.html",
                                outputDir = tempDir, sampleId = "test"))
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                outputFormat = "html_document", outputFile = "tmp.pdf",
                                outputDir = tempDir, sampleId = "test"))
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                outputFormat = "html_document", outputFile = "tmp.html",
                                outputDir = tempDir, sampleId = c("s1", "s2")))
})

test_that("report generation works", {
    rpt <- alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                package = "alevinQC"),
                          sampleId = "test", outputFile = "tmp.html",
                          outputDir = tempDir, outputFormat = NULL,
                          forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmp.html")
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                sampleId = "test", outputFile = "tmp.html",
                                outputDir = tempDir, outputFormat = NULL,
                                forceOverwrite = FALSE))
    file.copy(system.file("extdata/alevin_report_template.Rmd",
                          package = "alevinQC"),
              file.path(tempDir, "tmp.Rmd"))
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                sampleId = "test", outputFile = "tmp.html",
                                outputDir = tempDir, outputFormat = NULL,
                                forceOverwrite = TRUE))
})

test_that("app generation works", {
    app <- alevinQCShiny(baseDir = system.file("extdata/alevin_example_v0.14",
                                               package = "alevinQC"),
                         sampleId = "test")
    expect_s3_class(app, "shiny.appobj")
})
