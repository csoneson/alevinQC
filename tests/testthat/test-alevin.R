context("test-alevin")

test_that("checking for input files works", {
    ## All required files available - check should pass
    expect_null(checkAlevinInputFiles(system.file("extdata/alevin_example",
                                                  package = "alevinQC")))

    ## Remove one file - check should fail
    tmp <- tempdir()
    file.copy(from = system.file("extdata/alevin_example", package = "alevinQC"),
              to = tmp, overwrite = TRUE, recursive = TRUE)
    file.remove(file.path(tmp, "alevin_example/cmd_info.json"))
    expect_error(checkAlevinInputFiles(file.path(tmp, "alevin_example")))
})

## Read provided example input files for tests of file reading/plotting
alevin <- readAlevinQC(system.file("extdata/alevin_example",
                                   package = "alevinQC"))

test_that("reading input files works", {
    expect_length(alevin, 3)
    expect_is(alevin, "list")
    expect_named(alevin, c("cbTable", "versionTable", "summaryTable"))

    expect_equal(nrow(alevin$cbTable), 188613)
    expect_equal(sum(alevin$cbTable$inFirstWhiteList), 299)
    expect_equal(sum(!is.na(alevin$cbTable$mappingRate)), 298)
    expect_equal(sum(alevin$cbTable$inFinalWhiteList), 98)
})

test_that("plots are generated", {
    expect_is(plotAlevinKneeRaw(alevin$cbTable), "ggplot")
    expect_is(plotAlevinBarcodeCollapse(alevin$cbTable), "ggplot")
    expect_is(plotAlevinQuantPairs(alevin$cbTable), "ggmatrix")
    expect_is(plotAlevinKneeNbrGenes(alevin$cbTable), "ggplot")
})
