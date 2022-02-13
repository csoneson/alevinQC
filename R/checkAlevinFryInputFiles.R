#' Check that all required input files are available for alevin-fry
#'
#' @param mapDir Path to the directory containing the map.rad file
#' @param permitDir Path to the output directory of the alevin-fry
#'     generate-permit-list command.
#' @param quantDir Path to the output of the alevin-fry quant command.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @importFrom utils read.delim
#'
#' @return Returns nothing, raises an error if any of the required files are
#'   missing.
#'
#' @examples
#' checkAlevinFryInputFiles(
#'     mapDir = system.file("extdata/alevinfry_example_v0.4.3/map",
#'                          package = "alevinQC"),
#'     permitDir = system.file("extdata/alevinfry_example_v0.4.3/permit",
#'                             package = "alevinQC"),
#'     quantDir = system.file("extdata/alevinfry_example_v0.4.3/quant",
#'                            package = "alevinQC"))
#'
checkAlevinFryInputFiles <- function(mapDir, permitDir, quantDir) {
    msg <- NULL

    v0.4.3 <- .checkAlevinFryInputFiles_0.4.3(mapDir = mapDir,
                                              permitDir = permitDir,
                                              quantDir = quantDir)
    if (is.null(v0.4.3)) {
        return("v0.4.3")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with alevin-fry v0.4.3 ",
                 "or newer, the following required file(s) are missing ",
                 "or malformed:\n",
                 paste(v0.4.3, collapse = "\n"), "\n\n")
    }

    ## If we are here, the baseDir doesn't match any of the known
    ## output structures.
    stop(msg)
}

#' @keywords internal
#' @noRd
.checkAlevinFryInputFiles_0.4.3 <- function(mapDir, permitDir, quantDir) {
    ## Raise an error if any of the required files is missing
    reqFiles <- c(file.path(permitDir, "all_freq.tsv"),
                  file.path(permitDir, "permit_freq.tsv"),
                  file.path(quantDir, "featureDump.txt"),
                  file.path(mapDir, "aux_info/meta_info.json"),
                  file.path(quantDir, "quant.json"),
                  file.path(mapDir, "cmd_info.json"),
                  file.path(quantDir, "alevin", "quants_mat_rows.txt"))
    missingFiles <- reqFiles[vapply(reqFiles,
                                    function(f) {
                                        !file.exists(f)
                                    }, TRUE)]
    if (length(missingFiles) > 0) {
        return(missingFiles)
    }

    ## Check that all required columns in featureDump.txt are present
    coln <- unlist(utils::read.delim(
        file.path(quantDir, "featureDump.txt"),
        header = FALSE, as.is = TRUE, sep = "\t", nrows = 1))
    if (!(all(c("CorrectedReads", "MappingRate", "DedupRate",
                "NumGenesOverMean", "MappedReads", "DeduplicatedReads",
                "NumGenesExpressed") %in% coln))) {
        missingFiles <- c(file.path(quantDir, "featureDump.txt"))
    }

    if (length(missingFiles) > 0) {
        return(missingFiles)
    }

    ## If we are here, the input is consistent with the expectation
    return(NULL)
}
