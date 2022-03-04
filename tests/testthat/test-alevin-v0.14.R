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

    ## Remove whitelist file - should be read differently
    tmp <- tempdir()
    file.copy(from = system.file("extdata/alevin_example_v0.14",
                                 package = "alevinQC"),
              to = tmp, overwrite = TRUE, recursive = TRUE)
    file.remove(file.path(tmp, "alevin_example_v0.14/alevin/whitelist.txt"))
    expect_equal(checkAlevinInputFiles(file.path(tmp, "alevin_example_v0.14")), "v0.14nowl")

    ## Add whitelist entry to json - should be read differently
    a <- rjson::fromJSON(file = file.path(tmp, "alevin_example_v0.14/cmd_info.json"))
    a$whitelist = "whitelist.txt"
    write(rjson::toJSON(a), file = file.path(tmp, "alevin_example_v0.14/cmd_info.json"))
    expect_equal(checkAlevinInputFiles(file.path(tmp, "alevin_example_v0.14")), "v0.14extwl")
})

## Read provided example input files for tests of file reading/plotting
expect_message(alevin <- readAlevinQC(
    system.file("extdata/alevin_example_v0.14",
                package = "alevinQC"),
    customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                 "ATGAGGGAGTAGTGCG"),
                        set2 = c("CGAACATTCTGATACG"))),
    "100% of barcodes in custom barcode set set1 were found in the data set")

test_that("reading input files works", {
    expect_length(alevin, 4)
    expect_type(alevin, "list")
    expect_named(alevin, c("cbTable", "versionTable", "summaryTables", "type"))
    expect_equal(alevin$type, "standard")

    expect_equal(nrow(alevin$cbTable), 188613)
    expect_equal(sum(alevin$cbTable$inFirstWhiteList), 100)
    expect_equal(sum(!is.na(alevin$cbTable$mappingRate)), 298)
    expect_equal(sum(alevin$cbTable$inFinalWhiteList), 95)

    expect_equal(
        alevin$summaryTables$customCB__set2["Number of barcodes (set2)", 1],
        "1", ignore_attr = TRUE
    )
    expect_equal(
        alevin$summaryTables$customCB__set2["Mean number of reads per cell (set2)", 1],
        "106072", ignore_attr = TRUE
    )

    expect_equal(sum(alevin$cbTable$collapsedFreq[alevin$cbTable$customCB__set1]),
                 95259 + 108173)
    expect_equal(alevin$cbTable$collapsedFreq[alevin$cbTable$customCB__set2],
                 106072)
    expect_error(.makeSummaryTable(alevin$cbTable, colName = "collapsedFreq"))
    expect_error(.makeSummaryTable(alevin$cbTable, colName = "missingCol"))
})

test_that("plots are generated", {
    expect_s3_class(plotAlevinKneeRaw(alevin$cbTable), "ggplot")
    cbTableTmp <- alevin$cbTable
    cbTableTmp$inFirstWhiteList[1] <- FALSE
    expect_s3_class(plotAlevinKneeRaw(cbTableTmp), "ggplot")
    expect_s3_class(plotAlevinBarcodeCollapse(alevin$cbTable), "ggplot")
    expect_s3_class(plotAlevinQuantPairs(alevin$cbTable), "ggmatrix")
    expect_s3_class(plotAlevinKneeNbrGenes(alevin$cbTable), "ggplot")
    expect_s3_class(plotAlevinHistogram(alevin$cbTable), "ggplot")

    expect_error(plotAlevinQuantPairs(alevin$cbTable,
                                      colName = "nbrGenesAboveMean"))
    expect_error(plotAlevinQuant(alevin$cbTable,
                                 colName = "nbrGenesAboveMean"))
    expect_error(plotAlevinHistogram(alevin$cbTable,
                                     colName = "missing"))
    expect_error(plotAlevinHistogram(alevin$cbTable,
                                     colName = "CB"))
})

## Read provided example input files for tests of file reading/plotting, after removing whitelist
tmp <- tempdir()
file.copy(from = system.file("extdata/alevin_example_v0.14",
                             package = "alevinQC"),
          to = tmp, overwrite = TRUE, recursive = TRUE)
file.remove(file.path(tmp, "alevin_example_v0.14/alevin/whitelist.txt"))
expect_message(alevin <- readAlevinQC(
    file.path(tmp, "alevin_example_v0.14"),
    customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                 "ATGAGGGAGTAGTGCG"),
                        set2 = c("CGAACATTCTGATACG"))),
    "100% of barcodes in custom barcode set set1 were found in the data set")

test_that("reading input files works", {
    expect_length(alevin, 4)
    expect_type(alevin, "list")
    expect_named(alevin, c("cbTable", "versionTable", "summaryTables", "type"))
    expect_equal(alevin$type, "nowl")

    expect_equal(nrow(alevin$cbTable), 188613)
    expect_equal(sum(alevin$cbTable$inFirstWhiteList), 100)
    expect_equal(sum(!is.na(alevin$cbTable$mappingRate)), 298)
    expect_equal(sum(alevin$cbTable$inFinalWhiteList), 100)

    expect_equal(
        alevin$summaryTables$customCB__set2["Number of barcodes (set2)", 1],
        "1", ignore_attr = TRUE
    )
    expect_equal(
        alevin$summaryTables$customCB__set2["Mean number of reads per cell (set2)", 1],
        "106072", ignore_attr = TRUE
    )

    expect_equal(sum(alevin$cbTable$collapsedFreq[alevin$cbTable$customCB__set1]),
                 95259 + 108173)
    expect_equal(alevin$cbTable$collapsedFreq[alevin$cbTable$customCB__set2],
                 106072)
    expect_error(.makeSummaryTable(alevin$cbTable, colName = "collapsedFreq"))
    expect_error(.makeSummaryTable(alevin$cbTable, colName = "missingCol"))
})


tempDir <- tempdir()
if (file.exists(file.path(tempDir, "tmp.Rmd"))) {
    file.remove(file.path(tempDir, "tmp.Rmd"))
}
if (file.exists(file.path(tempDir, "tmp.html"))) {
    file.remove(file.path(tempDir, "tmp.html"))
}
if (file.exists(file.path(tempDir, "tmp2.html"))) {
    file.remove(file.path(tempDir, "tmp2.html"))
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
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                outputFormat = "html_document", outputFile = "tmp.html",
                                outputDir = tempDir, sampleId = "s1",
                                customCBList = c("A", "B")))
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                outputFormat = "html_document", outputFile = "tmp.html",
                                outputDir = tempDir, sampleId = "s1",
                                customCBList = list(c("A", "B"), c("C", "D"))))
})

test_that("report generation works", {
    rpt <- alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                package = "alevinQC"),
                          sampleId = "test", outputFile = "tmp.html",
                          outputDir = tempDir, outputFormat = NULL,
                          forceOverwrite = FALSE,
                          customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                                       "ATGAGGGAGTAGTGCG"),
                                              set2 = c("CGAACATTCTGATACG")))
    expect_equal(basename(rpt), "tmp.html")
    expect_error(alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                      package = "alevinQC"),
                                sampleId = "test", outputFile = "tmp.html",
                                outputDir = tempDir, outputFormat = NULL,
                                forceOverwrite = FALSE,
                                customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                                             "ATGAGGGAGTAGTGCG"),
                                                    set2 = c("CGAACATTCTGATACG"))))
    file.copy(system.file("extdata/alevin_report_template.Rmd",
                          package = "alevinQC"),
              file.path(tempDir, "tmp.Rmd"))
    expect_warning(
        expect_error(
            alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                 package = "alevinQC"),
                           sampleId = "test", outputFile = "tmp.html",
                           outputDir = tempDir, outputFormat = NULL,
                           forceOverwrite = TRUE,
                           customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                                        "ATGAGGGAGTAGTGCG"),
                                               set2 = c("CGAACATTCTGATACG")))
        )
    )
    rpt <- alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
                                                package = "alevinQC"),
                          sampleId = "test", outputFile = "tmp2.html",
                          outputDir = tempDir, outputFormat = NULL,
                          forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmp2.html")
})

test_that("app generation works", {
    app <- alevinQCShiny(baseDir = system.file("extdata/alevin_example_v0.14",
                                               package = "alevinQC"),
                         sampleId = "test")
    expect_s3_class(app, "shiny.appobj")
    app <- alevinQCShiny(baseDir = system.file("extdata/alevin_example_v0.14",
                                               package = "alevinQC"),
                         sampleId = "test",
                         customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                                      "ATGAGGGAGTAGTGCG"),
                                             set2 = c("CGAACATTCTGATACG")))
    expect_s3_class(app, "shiny.appobj")
})
