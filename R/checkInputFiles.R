#' Check that all required input files are available
#'
#' @param baseDir Path to the output directory from the Alevin run (should be
#'   the directory containing the \code{alevin} directory).
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @return Returns nothing, raises an error if any of the required files are
#'   missing.
#'
checkInputFiles <- function(baseDir) {
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
        stop("The following required file(s) are missing:\n",
             paste(missingFiles, collapse = "\n"))
    }
}
