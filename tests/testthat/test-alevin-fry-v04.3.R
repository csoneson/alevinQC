test_that("checking for input files works", {
    ## All required files available - check should pass
    expect_equal(checkAlevinFryInputFiles(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC")),
        "v0.4.3")

    ## Remove one file - check should fail
    tmp <- tempdir()
    file.copy(from = system.file("extdata/alevinfry_example_v0.4.3",
                                 package = "alevinQC"),
              to = tmp, overwrite = TRUE, recursive = TRUE)
    file.remove(file.path(tmp, "alevinfry_example_v0.4.3/map/cmd_info.json"))
    expect_error(checkAlevinFryInputFiles(
        mapDir = file.path(tmp, "alevinfry_example_v0.4.3", "map"),
        permitDir = file.path(tmp, "alevinfry_example_v0.4.3", "permit"),
        quantDir = file.path(tmp, "alevinfry_example_v0.4.3", "quant")))
})

## Read provided example input files for tests of file reading/plotting
alevin <- readAlevinFryQC(
    mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                         package = "alevinQC"),
    permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                            package = "alevinQC"),
    quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                           package = "alevinQC"))

test_that("reading input files works", {
    expect_length(alevin, 4)
    expect_type(alevin, "list")
    expect_named(alevin, c("cbTable", "versionTable", "summaryTables", "type"))
    expect_equal(alevin$type, "alevin-fry")

    expect_equal(nrow(alevin$cbTable), 154362)
    expect_equal(sum(alevin$cbTable$inPermitList), 101)
    expect_equal(sum(!is.na(alevin$cbTable$mappingRate)), 101)

    expect_error(.makeSummaryTable(alevin$cbTable, colName = "collapsedFreq"),
                 "is.logical")
    expect_error(.makeSummaryTable(alevin$cbTable, colName = "missingCol"),
                 "colName")
})

test_that("plots are generated", {
    expect_s3_class(plotAlevinKneeRaw(
        alevin$cbTable, firstSelColName = "inPermitList"), "ggplot")
    expect_s3_class(plotAlevinBarcodeCollapse(
        alevin$cbTable,
        firstSelColName = "inPermitList", countCol = "nbrMappedUMI"), "ggplot")
    expect_s3_class(plotAlevinQuantPairs(
        alevin$cbTable,
        firstSelColName = "inPermitList", colName = "inPermitList"), "ggmatrix")
    expect_s3_class(plotAlevinKneeNbrGenes(
        alevin$cbTable, firstSelColName = "inPermitList"), "ggplot")
    expect_s3_class(plotAlevinHistogram(
        alevin$cbTable, colName = "inPermitList", cbName = "permitlist",
        firstSelColName = "inPermitList"), "ggplot")

    expect_error(plotAlevinQuantPairs(alevin$cbTable,
                                      firstSelColName = "inPermitList",
                                      colName = "nbrGenesAboveMean"))
    expect_error(plotAlevinQuant(alevin$cbTable,
                                 firstSelColName = "inPermitList",
                                 colName = "nbrGenesAboveMean"))
    expect_error(plotAlevinHistogram(alevin$cbTable,
                                     firstSelColName = "inPermitList",
                                     colName = "missing"))
    expect_error(plotAlevinHistogram(alevin$cbTable,
                                     firstSelColName = "inPermitList",
                                     colName = "CB"))
})

tempDir <- tempdir()
if (file.exists(file.path(tempDir, "tmpfry.Rmd"))) {
    file.remove(file.path(tempDir, "tmpfry.Rmd"))
}
if (file.exists(file.path(tempDir, "tmpfry.html"))) {
    file.remove(file.path(tempDir, "tmpfry.html"))
}
if (file.exists(file.path(tempDir, "tmp2fry.html"))) {
    file.remove(file.path(tempDir, "tmp2fry.html"))
}
if (file.exists(file.path(tempDir, "tmpfry3.Rmd"))) {
    file.remove(file.path(tempDir, "tmpfry3.Rmd"))
}
if (file.exists(file.path(tempDir, "tmpfry3.html"))) {
    file.remove(file.path(tempDir, "tmpfry3.html"))
}
if (file.exists(file.path(tempDir, "tmpfry.pdf"))) {
    file.remove(file.path(tempDir, "tmpfry.pdf"))
}

test_that("input arguments are processed correctly", {
    ## outputFormat
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        outputFormat = "html", outputFile = "tmpfry.html",
        outputDir = tempDir, sampleId = "test"))
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        outputFormat = "html_document", outputFile = "tmpfry.pdf",
        outputDir = tempDir, sampleId = "test"))
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        outputFormat = "html_document", outputFile = "tmpfry.html",
        outputDir = tempDir, sampleId = c("s1", "s2")))
})

test_that("report generation works", {
    rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmpfry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmpfry.html")
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmpfry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE))
    file.copy(system.file("extdata/alevin_report_template.Rmd",
                          package = "alevinQC"),
              file.path(tempDir, "tmpfry.Rmd"))
    expect_warning(
        expect_error(
            alevinFryQCReport(
                mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                                     package = "alevinQC"),
                permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                        package = "alevinQC"),
                quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                                       package = "alevinQC"),
                sampleId = "test", outputFile = "tmpfry.html",
                outputDir = tempDir, outputFormat = NULL,
                forceOverwrite = TRUE)
        )
    )
    rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmp2fry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmp2fry.html")

    ## Try to specify custom barcode lists - currently not supported
    expect_warning(rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmpfry3.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = TRUE,
        customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                     "ATGAGGGAGTAGTGCG"),
                            set2 = c("CGAACATTCTGATACG"))))
    expect_equal(basename(rpt), "tmpfry3.html")
})

test_that("app generation works", {
    app <- alevinFryQCShiny(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"),
        sampleId = "test")
    expect_s3_class(app, "shiny.appobj")
})
