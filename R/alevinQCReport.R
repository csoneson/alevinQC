#' Check whether pandoc and pandoc-citeproc are available
#'
#' @author Charlotte Soneson
#'
#' @param ignorePandoc logical. If TRUE, just give a warning if pandoc or
#'     pandoc-citeproc is not available. If FALSE, stop.
#'
#' @keywords internal
#' @noRd
#'
#' @return A logical(1), indicating whether pandoc can be run or not.
#'     In addition, raises either a warning or an error (depending on the
#'     value of \code{ignorePandoc}) if pandoc or pandoc-citeproc is not
#'     available.
#'
#' @importFrom rmarkdown pandoc_available pandoc_exec
#'
.checkPandoc <- function(ignorePandoc) {
    # nocov start
    ## Initialize output to TRUE
    doRender <- TRUE

    ## First check whether pandoc is available
    if (!rmarkdown::pandoc_available()) {
        doRender <- FALSE
        ## If pandoc is not available, either give a warning or an error,
        ## depending on the value of ignorePandoc
        if (ignorePandoc) {
            ## If ignorePandoc is TRUE, just give a warning
            warning("pandoc is not available! ",
                    "The final report will not be generated.",
                    immediate. = TRUE)
        } else {
            ## If ignorePandoc is FALSE, stop
            stop("pandoc is not available!")
        }
    } else {
        ## If pandoc is available, check for pandoc-citeproc
        ## Only do this if the pandoc version is <2.11, since
        ## pandoc-citeproc is not included (or needed) in v2.11 and later.
        if (!rmarkdown::pandoc_available(version = "2.11")) {
            ## TRUE if the available pandoc version is not 2.11 or newer
            ## pandoc-citeproc should be found in the path, or in the
            ## same folder as the pandoc executable
            if (Sys.which("pandoc-citeproc") == "" &&
                !file.exists(file.path(dirname(rmarkdown::pandoc_exec()),
                                       "pandoc-citeproc"))) {
                doRender <- FALSE
                ## pandoc-citeproc is required, but not found
                if (ignorePandoc) {
                    ## If ignorePandoc is TRUE, just give a warning
                    warning("pandoc-citeproc is not available! ",
                            "The final report will not be generated.",
                            immediate. = TRUE)
                } else {
                    ## If ignorePandoc is FALSE, stop
                    stop("pandoc-citeproc is not available!")
                }
            }
        }
    }
    return(doRender)
    # nocov end
}

#' Generate alevin/alevin-fry summary report
#'
#' Generate a report summarizing the main aspects of an alevin/alevin-fry
#' quantification run. The report generation assumes that alevin/alevin-fry
#' has been run with the --dumpFeatures flag to generate the necessary
#' output files.
#'
#' @param baseDir (Only used for alevin output) Path to the output directory
#'     from the alevin run (should be the directory containing the
#'     \code{alevin} directory).
#' @param mapDir (Only used for alevin-fry output) Path to the output directory
#'     from the salmon alevin run (should be the directory containing the
#'     \code{map.rad} file).
#' @param permitDir (Only used for alevin-fry output) Path to the output
#'     directory from the permit list generation step (should be
#'     the directory containing the \code{all_freq.tsv} file).
#' @param quantDir (Only used for alevin-fry output) Path to the output
#'     directory from the alevin-fry quantification step (should be
#'     the directory containing the \code{alevin} directory).
#' @param sampleId Sample ID, will be used to set the title for the report.
#' @param outputFile File name of the output report. The file name extension
#'   must be either \code{.html} or \code{.pdf}, and consistent with the value
#'   of \code{outputFormat}.
#' @param outputDir Path to the output directory where the report will be
#'   generated.
#' @param outputFormat The format of the output report. Either
#'   \code{"html_document"} or \code{"pdf_document"}. The file name extension of
#'   \code{outputFile} must be consistent with this choice.
#' @param showCode Logical, whether to display the R code in the report.
#' @param forceOverwrite Logical, whether to force overwrite an existing report
#'   with the same name in the output directory.
#' @param knitrProgress Logical, whether to display the progress of \code{knitr}
#'   when generating the report.
#' @param quiet Logical, whether to show progress messages.
#' @param ignorePandoc Logical, determines what to do if \code{pandoc} or
#'   \code{pandoc-citeproc} is missing (if \code{Sys.which("pandoc")} or
#'   \code{Sys.which("pandoc-citeproc")} returns ""). If \code{ignorePandoc} is
#'   TRUE, only a warning is given. The figures will be generated, but not the
#'   final report. If \code{ignorePandoc} is FALSE (default), the execution
#'   stops immediately.
#' @param customCBList Named list with custom set(s) of barcodes to provide
#'   summary statistics/plots for, in addition to the whitelists generated by
#'   alevin.
#' @param ... Other arguments that will be passed to \code{rmarkdown::render}.
#'
#' @author Charlotte Soneson
#'
#' @details When the function is called, a .Rmd template file will be copied
#'   into the output directory, and \code{rmarkdown::render} will be called to
#'   generate the final report. If there is already a .Rmd file with the same
#'   name in the output directory, the function will raise an error and stop, to
#'   avoid overwriting the existing file. The reason for this behaviour is that
#'   the copied template in the output directory will be deleted once the report
#'   is generated.
#'
#' @name qcReport
#'
#' @importFrom rmarkdown render
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods is
#' @import dplyr
#'
#' @return Generates a summary report in the \code{outputDir} directory, and
#'   returns (invisibly) the name of the generated report.
#'
#' @examples
#' alevinQCReport(baseDir = system.file("extdata/alevin_example_v0.14",
#'                                      package = "alevinQC"),
#'                sampleId = "example", outputFile = "alevinReport.html",
#'                outputDir = tempdir(), forceOverwrite = TRUE)
#'
#' alevinFryQCReport(
#'     mapDir = system.file("extdata/alevinfry_example_v0.5.0/map",
#'                          package = "alevinQC"),
#'     permitDir = system.file("extdata/alevinfry_example_v0.5.0/permit",
#'                             package = "alevinQC"),
#'     quantDir = system.file("extdata/alevinfry_example_v0.5.0/quant",
#'                            package = "alevinQC"),
#'     sampleId = "example", outputFile = "alevinFryReport.html",
#'     outputDir = tempdir(), forceOverwrite = TRUE)
#'
NULL

#' @rdname qcReport
#' @export
alevinQCReport <- function(baseDir, sampleId, outputFile, outputDir = "./",
                           outputFormat = NULL, showCode = FALSE,
                           forceOverwrite = FALSE, knitrProgress = FALSE,
                           quiet = FALSE, ignorePandoc = FALSE,
                           customCBList = list(), ...) {
    .alevinQCReport(baseDir = baseDir, mapDir = NULL, permitDir = NULL,
                    quantDir = NULL, quantMethod = "alevin",
                    sampleId = sampleId, outputFile = outputFile,
                    outputDir = outputDir, outputFormat = outputFormat,
                    showCode = showCode, forceOverwrite = forceOverwrite,
                    knitrProgress = knitrProgress, quiet = quiet,
                    ignorePandoc = ignorePandoc,
                    customCBList = customCBList, ...)
}

#' @rdname qcReport
#' @export
alevinFryQCReport <- function(mapDir, permitDir, quantDir, sampleId,
                              outputFile, outputDir = "./",
                              outputFormat = NULL, showCode = FALSE,
                              forceOverwrite = FALSE, knitrProgress = FALSE,
                              quiet = FALSE, ignorePandoc = FALSE,
                              customCBList = list(), ...) {
    if (length(customCBList) != 0) {
        warning("custom CB lists are currently not implemented for ",
                "alevin-fry QC reports")
    }
    .alevinQCReport(baseDir = NULL, mapDir = mapDir, permitDir = permitDir,
                    quantDir = quantDir, quantMethod = "alevin-fry",
                    sampleId = sampleId, outputFile = outputFile,
                    outputDir = outputDir, outputFormat = outputFormat,
                    showCode = showCode, forceOverwrite = forceOverwrite,
                    knitrProgress = knitrProgress, quiet = quiet,
                    ignorePandoc = ignorePandoc,
                    customCBList = list(), ...)
}

#' @keywords internal
#' @noRd
#' @author Charlotte Soneson
.alevinQCReport <- function(baseDir, mapDir, permitDir, quantDir,
                            quantMethod, sampleId, outputFile, outputDir = "./",
                            outputFormat = NULL, showCode = FALSE,
                            forceOverwrite = FALSE, knitrProgress = FALSE,
                            quiet = FALSE, ignorePandoc = FALSE,
                            customCBList = list(), ...) {
    ## This function was inspired by code from Nicholas Hamilton, provided at
    ## http://stackoverflow.com/questions/37097535/generate-report-in-r

    ## If possible, set output format based on the extension of outputFile, if
    ## the output format is not provided
    if (is.null(outputFormat)) {
        if (tools::file_ext(outputFile) == "pdf") {
            outputFormat <- "pdf_document"
        } else {
            outputFormat <- "html_document"
        }
    }

    ## Check if pandoc and pandoc-citeproc are available
    doRender <- .checkPandoc(ignorePandoc)

    ## ---------------------------------------------------------------------- ##
    ## --------------------- Check input arguments -------------------------- ##
    ## ---------------------------------------------------------------------- ##

    ## ------------------------ outputFormat -------------------------------- ##
    ## Raise an error if outputFormat is not one of the allowed
    if (!(outputFormat %in% c("pdf_document", "html_document",
                              "BiocStyle::html_document",
                              "BiocStyle::pdf_document"))) {
        stop("The provided outputFormat is currently not supported. Please ",
             "use either 'html_document' or 'pdf_document'.", call. = FALSE)
    }

    ## Raise an error if the output format and file name extension don't match
    if (gsub("BiocStyle::", "", outputFormat) !=
        paste0(tools::file_ext(outputFile), "_document")) {
        stop(paste0("File name extension of outputFile (.",
                    tools::file_ext(outputFile),
                    ") doesn't agree with the ",
                    "outputFormat, should be .",
                    gsub("_document$", "", outputFormat)), call. = FALSE)
    }

    ## ----------------------- input directory ------------------------------ ##
    ## Normalize directory paths
    if (!is.null(baseDir)) {
        baseDir <- normalizePath(baseDir)
    }
    if (!is.null(mapDir)) {
        mapDir <- normalizePath(mapDir)
    }
    if (!is.null(permitDir)) {
        permitDir <- normalizePath(permitDir)
    }
    if (!is.null(quantDir)) {
        quantDir <- normalizePath(quantDir)
    }


    ## Check that all required input files are available
    if (quantMethod == "alevin") {
        checkAlevinInputFiles(baseDir)
    } else if (quantMethod == "alevin-fry") {
        checkAlevinFryInputFiles(mapDir = mapDir, permitDir = permitDir,
                                 quantDir = quantDir)
    }

    ## sampleId must be a character string of length 1
    if (!is(sampleId, "character") || length(sampleId) != 1) {
        stop("sampleId must be a character string")
    }

    ## --------------------- custom barcode list ---------------------------- ##
    if (length(customCBList) > 0) {
        if (!is(customCBList, "list")) {
            stop("'customCBList' must be a list")
        }
        if (any(is.null(names(customCBList))) ||
            any(names(customCBList) == "")) {
            stop("'customCBList' must be a named list")
        }
        if (!all(vapply(customCBList, function(cbl) is(cbl, "character"), FALSE))) {
            stop("'customCBList' must be a named list of character vectors")
        }
    }

    ## ------------------------- output files ------------------------------- ##
    outputReport <- file.path(outputDir, basename(outputFile))
    outputRmd <- file.path(
        outputDir,
        paste0(tools::file_path_sans_ext(basename(outputFile)), ".Rmd")
    )

    ## Report
    if (file.exists(outputReport)) {
        if (!forceOverwrite) {
            stop("The file ", outputReport,
                 " already exists. Please remove or rename the file, provide ",
                 "another value of outputFile, or set forceOverwrite = TRUE.",
                 call. = FALSE)
        } else {
            if (!quiet) {
                warning("The file ", outputReport,
                        " already exists and will be overwritten, since ",
                        "forceOverwrite = TRUE.", immediate. = TRUE,
                        call. = FALSE)
            }
        }
    }

    ## ------------------------- Rmd template ------------------------------- ##
    ## Path to the template file
    templateFile <- system.file("extdata",
                                "alevin_report_template.Rmd",
                                package = "alevinQC")
    if (file.exists(templateFile)) {
        if (file.exists(outputRmd)) {
            stop("There is already an .Rmd file ", outputRmd,
                 ". Please remove or rename this file, or choose another ",
                 "outputFile name.", call. = FALSE)
        } else {
            file.copy(from = templateFile, to = outputRmd, overwrite = FALSE)
        }
    } else {
        stop("The Rmd template file ", templateFile, " does not exist.",
             call. = FALSE)
    }

    ## ---------------------------------------------------------------------- ##
    ## ----------------------- Process the arguments ------------------------ ##
    ## ---------------------------------------------------------------------- ##

    args <- list(...)
    args$input <- outputRmd
    args$output_format <- outputFormat
    args$output_file <- outputFile
    args$quiet <- !knitrProgress
    args$run_pandoc <- doRender

    ## ---------------------------------------------------------------------- ##
    ## ------------------------ Render the report --------------------------- ##
    ## ---------------------------------------------------------------------- ##

    outputFile <- do.call("render", args = args)

    ## ---------------------------------------------------------------------- ##
    ## --------------------- Remove temporary file -------------------------- ##
    ## ---------------------------------------------------------------------- ##

    file.remove(outputRmd)

    invisible(outputFile)
}
