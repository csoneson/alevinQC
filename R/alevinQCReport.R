#' Generate Alevin summary report
#'
#' Generate a report summarizing the main aspects of an Alevin quantification
#' run. The report generation assumes that Alevin has been run with
#' --dumpFeatures flag to generate the necessary output files.
#'
#' @param basedir Base directory (should be the directory containing the
#'   \code{alevin} directory).
#' @param sampleId Sample ID
#'
#' @author Charlotte Soneson
#'
#' @details When the function is called, an .Rmd template file will be copied
#'   into the output folder, and \code{rmarkdown::render} will be called to
#'   generate the final report. If there is already a .Rmd file with the same
#'   name in the output folder, the function will raise an error and stop, to
#'   avoid overwriting the existing file. The reason for this behaviour is that
#'   the copied template in the output folder will be deleted once the report is
#'   generated.
#'
#' @export
#' @importFrom rmarkdown render
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom methods is
#'
#' @return No value is returned, but a report is generated in the
#'   \code{outputDir} directory.
#'
#' @examples
#'
alevinQCReport <- function(baseDir, outputFile, outputDir = "./",
                           outputFormat = NULL,
                           showCode = FALSE, forceOverwrite = FALSE,
                           sampleId, knitrProgress = FALSE, quiet = FALSE,
                           ignorePandoc = FALSE, ...) {
    ## This function was inspired by code from Nicholas Hamilton, provided at
    ## http://stackoverflow.com/questions/37097535/generate-report-in-r

    if (is.null(outputFormat)) outputFormat <- "html_document"

    ## Check if pandoc and pandoc-citeproc is available
    if (Sys.which("pandoc") == "") {
        if (ignorePandoc) {
            ## If ignorePandoc is TRUE, just give a warning
            warning("pandoc is not available! ",
                    "The final report will not be generated.")
        } else {
            ## If ignorePandoc is FALSE, stop
            stop("pandoc is not available!")
        }
    }
    if (Sys.which("pandoc-citeproc") == "") {
        if (ignorePandoc) {
            ## If ignorePandoc is TRUE, just give a warning
            warning("pandoc-citeproc is not available! ",
                    "The final report will not be generated.")
        } else {
            ## If ignorePandoc is FALSE, stop
            stop("pandoc-citeproc is not available!")
        }
    }

    ## ------------------------------------------------------------------------ ##
    ## --------------------- Check input arguments ---------------------------- ##
    ## ------------------------------------------------------------------------ ##

    ## ------------------------ outputFormat --------------------------------- ##
    ## Raise an error if outputFormat is not one of the allowed
    if (!(outputFormat %in% c("pdf_document", "html_document"))) {
        stop("The provided outputFormat is currently not supported. Please use ",
             "either 'html_document' (or NULL) or 'pdf_document'.", call. = FALSE)
    }

    ## Raise an error if the output format and file name extension don't match
    if (outputFormat != paste0(tools::file_ext(outputFile), "_document")) {
        stop(paste0("File name extension of outputFile doesn't agree with the ",
                    "outputFormat, should be .",
                    gsub("_document$", "", outputFormat)), call. = FALSE)
    }

    ## ----------------------- input directory ------------------------------- ##
    ## Raise an error if any of the required files is missing
    reqFiles <- c(file.path(baseDir, "alevin/raw_cb_frequency.txt"),
                  file.path(baseDir, "alevin/filtered_cb_frequency.txt"),
                  file.path(baseDir, "alevin/featureDump.txt"),
                  file.path(baseDir, "alevin/MappedUmi.txt"),
                  file.path(baseDir, "alevin/whitelist.txt"),
                  file.path(baseDir, "alevin/quants_mat_rows.txt"),
                  file.path(baseDir, "alevin/quants_mat_cols.txt"),
                  file.path(baseDir, "alevin/quants_mat.gz"),
                  file.path(baseDir, "aux_info/meta_info.json"),
                  file.path(baseDir, "cmd_info.json"))
    lapply(reqFiles, function(f) {
        if (!file.exists(f)) {
            stop("the required file ", f, " doesn't exist.")
        }
    })

    ## sampleId must be a character string of length 1
    if (!is(sampleId, "character") || length(sampleId) != 1) {
        stop("sampleId must be a character string")
    }

    ## ------------------------- output files --------------------------------- ##
    outputReport <- file.path(outputDir, basename(outputFile))
    outputRmd <- file.path(
        outputDir,
        paste0(tools::file_path_sans_ext(basename(outputFile)), ".Rmd"))

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
                        "forceOverwrite = TRUE.", immediate. = TRUE, call. = FALSE)
            }
        }
    }

    ## ------------------------- Rmd template --------------------------------- ##
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
        stop("The intended Rmd template file ", templateFile, " does not exist.",
             call. = FALSE)
    }

    ## ------------------------------------------------------------------------ ##
    ## ----------------------- Process the arguments -------------------------- ##
    ## ------------------------------------------------------------------------ ##

    args <- list(...)
    args$input <- outputRmd
    args$output_format <- outputFormat
    args$output_file <- outputFile
    args$quiet <- !knitrProgress

    ## ------------------------------------------------------------------------ ##
    ## ------------------------ Render the report ----------------------------- ##
    ## ------------------------------------------------------------------------ ##

    outputFile <- do.call("render", args = args)

    ## ------------------------------------------------------------------------ ##
    ## --------------------- Remove temporary file ---------------------------- ##
    ## ------------------------------------------------------------------------ ##

    file.remove(outputRmd)

    invisible(outputFile)
}
