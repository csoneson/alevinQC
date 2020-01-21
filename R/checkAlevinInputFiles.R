#' Check that all required input files are available
#'
#' @param baseDir Path to the output directory from the alevin run (should be
#'   the directory containing the \code{alevin} directory).
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return Returns nothing, raises an error if any of the required files are
#'   missing.
#'
#' @examples
#' checkAlevinInputFiles(system.file("extdata/alevin_example_v0.14",
#'                                   package = "alevinQC"))
#'
checkAlevinInputFiles <- function(baseDir) {
    msg <- NULL

    v0.14 <- .checkAlevinInputFiles_v0.14(baseDir)
    if (is.null(v0.14)) {
        return("v0.14")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with Salmon v0.14 or newer, ",
                 "the following required file(s) are missing or malformed:\n",
                 paste(v0.14, collapse = "\n"), "\n\n")
    }

    pre0.14 <- .checkAlevinInputFiles_pre0.14(baseDir)
    if (is.null(pre0.14)) {
        return("pre0.14")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with Salmon pre-v0.14, ",
                 "the following required file(s) are missing or malformed:\n",
                 paste(pre0.14, collapse = "\n"), "\n\n")
    }

    ## If we are here, the baseDir doesn't match any of the known
    ## output structures.
    stop(msg)
}


.checkAlevinInputFiles_pre0.14 <- function(baseDir) {
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
    missingFiles <- reqFiles[vapply(reqFiles,
                                    function(f) {
                                        !file.exists(f)
                                    }, TRUE)]
    if (length(missingFiles) > 0) {
        return(missingFiles)
    } else {
        return(NULL)
    }
}

.checkAlevinInputFiles_v0.14 <- function(baseDir) {
    ## Raise an error if any of the required files is missing
    reqFiles <- c(file.path(baseDir, "alevin/raw_cb_frequency.txt"),
                  file.path(baseDir, "alevin/featureDump.txt"),
                  file.path(baseDir, "alevin/whitelist.txt"),
                  file.path(baseDir, "aux_info/meta_info.json"),
                  file.path(baseDir, "aux_info/alevin_meta_info.json"),
                  file.path(baseDir, "cmd_info.json"))
    missingFiles <- reqFiles[vapply(reqFiles,
                                    function(f) {
                                        !file.exists(f)
                                    }, TRUE)]
    if (length(missingFiles) > 0) {
        return(missingFiles)
    }

    ## Check that all required columns in featureDump.txt are present
    coln <- unlist(read.delim(
        file.path(baseDir, "alevin/featureDump.txt"),
        header = FALSE, as.is = TRUE, sep = "\t", nrows = 1))
    if (!(all(c("CorrectedReads", "MappingRate", "DedupRate",
                "NumGenesOverMean", "MappedReads", "DeduplicatedReads",
                "NumGenesExpressed") %in% coln))) {
        missingFiles <- c(file.path(baseDir, "alevin/featureDump.txt"))
    }

    if (length(missingFiles) > 0) {
        return(missingFiles)
    } else {
        return(NULL)
    }
}
