test_that("checking for input files works", {
    ## All required files available - check should pass
    expect_equal(checkAlevinFryInputFiles(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC")),
        "piscem_v0.6.0")

    ## Remove one file - check should fail
    tmp <- tempdir()
    file.copy(from = system.file("extdata/alevinfry_example_piscem_v0.6.0",
                                 package = "alevinQC"),
              to = tmp, overwrite = TRUE, recursive = TRUE)
    file.remove(file.path(tmp, "alevinfry_example_piscem_v0.6.0/af_map/map_info.json"))
    expect_error(checkAlevinFryInputFiles(
        mapDir = file.path(tmp, "alevinfry_example_piscem_v0.6.0", "af_map"),
        permitDir = file.path(tmp, "alevinfry_example_piscem_v0.6.0", "af_quant"),
        quantDir = file.path(tmp, "alevinfry_example_piscem_v0.6.0", "af_quant")))
})

## Read provided example input files for tests of file reading/plotting
alevin <- readAlevinFryQC(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
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

test_that("reading input files gives same result as for v0.4.3", {
    cb043 <- readAlevinFryQC(
        mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
                               package = "alevinQC"))
    ## Ties can be sorted differently - can't just compare outputs directly
    expect_equal(sort(alevin$cbTable$CB), sort(cb043$cbTable$CB))
    cb043$cbTable <- cb043$cbTable[match(alevin$cbTable$CB, cb043$cbTable$CB), ]
    for (cn in setdiff(colnames(alevin$cbTable), "ranking")) {
        expect_equal(alevin$cbTable[[cn]], cb043$cbTable[[cn]])
    }
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
if (file.exists(file.path(tempDir, "tmppfry.Rmd"))) {
    file.remove(file.path(tempDir, "tmppfry.Rmd"))
}
if (file.exists(file.path(tempDir, "tmppfry.html"))) {
    file.remove(file.path(tempDir, "tmppfry.html"))
}
if (file.exists(file.path(tempDir, "tmpp2fry.html"))) {
    file.remove(file.path(tempDir, "tmpp2fry.html"))
}
if (file.exists(file.path(tempDir, "tmpp2simpleaf.html"))) {
    file.remove(file.path(tempDir, "tmpp2simpleaf.html"))
}
if (file.exists(file.path(tempDir, "tmppfry3.Rmd"))) {
    file.remove(file.path(tempDir, "tmppfry3.Rmd"))
}
if (file.exists(file.path(tempDir, "tmppfry3.html"))) {
    file.remove(file.path(tempDir, "tmppfry3.html"))
}
if (file.exists(file.path(tempDir, "tmppfry.pdf"))) {
    file.remove(file.path(tempDir, "tmppfry.pdf"))
}

test_that("input arguments are processed correctly", {
    ## outputFormat
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        outputFormat = "html", outputFile = "tmppfry.html",
        outputDir = tempDir, sampleId = "test"))
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        outputFormat = "html_document", outputFile = "tmppfry.pdf",
        outputDir = tempDir, sampleId = "test"))
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        outputFormat = "html_document", outputFile = "tmppfry.html",
        outputDir = tempDir, sampleId = c("s1", "s2")))
})

test_that("report generation works", {
    rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmppfry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmppfry.html")
    expect_error(alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmppfry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE))
    file.copy(system.file("extdata/alevin_report_template.Rmd",
                          package = "alevinQC"),
              file.path(tempDir, "tmppfry.Rmd"))
    expect_warning(
        expect_error(
            alevinFryQCReport(
                mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                                     package = "alevinQC"),
                permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                        package = "alevinQC"),
                quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                       package = "alevinQC"),
                sampleId = "test", outputFile = "tmppfry.html",
                outputDir = tempDir, outputFormat = NULL,
                forceOverwrite = TRUE)
        )
    )
    rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmpp2fry.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmpp2fry.html")

    ## simpleaf
    rpt <- simpleafQCReport(
        simpleafQuantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0",
                                       package = "alevinQC"),
        sampleId = "test", outputFile = "tmpp2simpleaf.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = FALSE)
    expect_equal(basename(rpt), "tmpp2simpleaf.html")


    ## Try to specify custom barcode lists - currently not supported
    expect_warning(rpt <- alevinFryQCReport(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        sampleId = "test", outputFile = "tmppfry3.html",
        outputDir = tempDir, outputFormat = NULL,
        forceOverwrite = TRUE,
        customCBList = list(set1 = c("TCGCGAGGTTCAGACT",
                                     "ATGAGGGAGTAGTGCG"),
                            set2 = c("CGAACATTCTGATACG"))))
    expect_equal(basename(rpt), "tmppfry3.html")
})

test_that("app generation works", {
    app <- alevinFryQCShiny(
        mapDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_map",
                             package = "alevinQC"),
        permitDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                                package = "alevinQC"),
        quantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0/af_quant",
                               package = "alevinQC"),
        sampleId = "test")
    expect_s3_class(app, "shiny.appobj")

    app <- simpleafQCShiny(
        simpleafQuantDir = system.file("extdata/alevinfry_example_piscem_v0.6.0",
                                       package = "alevinQC"),
        sampleId = "test")
    expect_s3_class(app, "shiny.appobj")
})
