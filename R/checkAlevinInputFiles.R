#' Check that all required input files are available
#'
#' @param baseDir Path to the output directory from the alevin run (should be
#'   the directory containing the \code{alevin} directory).
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
#' checkAlevinInputFiles(system.file("extdata/alevin_example_v0.14",
#'                                   package = "alevinQC"))
#'
checkAlevinInputFiles <- function(baseDir) {
    msg <- NULL

    ## First check whether the baseDir files are compatible with alevin
    ## v0.14 or later, including a whitelist.txt file.
    ## This is the "standard" v0.14 output.
    v0.14 <- .checkAlevinInputFiles_v0.14(baseDir, type = "standard")
    if (is.null(v0.14)) {
        return("v0.14")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with Salmon v0.14 or newer ",
                 "(without external whitelist), ",
                 "the following required file(s) are missing or malformed:\n",
                 paste(v0.14, collapse = "\n"), "\n\n")
    }

    ## If the above check failed, check whether the baseDir files are
    ## compatible with alevin v0.14 or later, with an external whitelist
    ## provided. In this case, there would not have to be a whitelist.txt
    ## file, and there would be a "whitelist" entry in the meta_info.json file.
    ## This is the "extwl" v0.14 output.
    v0.14extwl <- .checkAlevinInputFiles_v0.14(baseDir, type = "extwl")
    if (is.null(v0.14extwl)) {
        return("v0.14extwl")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with Salmon v0.14 or newer ",
                 "(with external whitelist), ",
                 "the following required file(s) are missing or malformed:\n",
                 paste(v0.14extwl, collapse = "\n"), "\n\n")
    }

    ## If the above checks failed, check whether the baseDir files are
    ## compatible with alevin v0.14 or later, but without the whitelist.txt
    ## file. This can happen if the whitelisting could not be performed
    ## for some reason. This is the "nowl" v0.14 output.
    v0.14nowl <- .checkAlevinInputFiles_v0.14(baseDir, type = "nowl")
    if (is.null(v0.14nowl)) {
        return("v0.14nowl")
    } else {
        msg <- c(msg,
                 "Input directory not compatible with Salmon v0.14 or newer ",
                 "(without final whitelist), ",
                 "the following required file(s) are missing or malformed:\n",
                 paste(v0.14nowl, collapse = "\n"), "\n\n")
    }

    ## If all the checks above failed, check whether the baseDir files are
    ## compatible with alevin pre-v0.14.
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

# type is either 'standard', 'extwl', or 'nowl'
.checkAlevinInputFiles_v0.14 <- function(baseDir, type = "standard") {
    ## Raise an error if any of the required files is missing
    reqFiles <- c(file.path(baseDir, "alevin/raw_cb_frequency.txt"),
                  file.path(baseDir, "alevin/featureDump.txt"),
                  file.path(baseDir, "aux_info/meta_info.json"),
                  file.path(baseDir, "aux_info/alevin_meta_info.json"),
                  file.path(baseDir, "cmd_info.json"))
    if (type == "standard") {
        ## 'standard' checks should include also the whitelist.txt file
        reqFiles <- c(reqFiles, file.path(baseDir, "alevin/whitelist.txt"))
    }
    ## If any of the files are missing, stop and return these
    missingFiles <- reqFiles[vapply(reqFiles,
                                    function(f) {
                                        !file.exists(f)
                                    }, TRUE)]
    if (length(missingFiles) > 0) {
        return(missingFiles)
    }

    ## All required files exist. Check that they contain the required info.
    if (type == "extwl") {
        ## 'extwl' checks should have a 'whitelist' entry in meta_info.json
        meta <- rjson::fromJSON(file = file.path(baseDir,
                                                 "cmd_info.json"))
        if (is.null(meta$whitelist)) {
            missingFiles <- paste0(
                file.path(baseDir, "aux_info/meta_info.json"),
                " (whitelist entry)"
            )
        }
        if (length(missingFiles) > 0) {
            return(missingFiles)
        }
    }

    ## Check that all required columns in featureDump.txt are present
    coln <- unlist(utils::read.delim(
        file.path(baseDir, "alevin/featureDump.txt"),
        header = FALSE, as.is = TRUE, sep = "\t", nrows = 1))
    if (!(all(c("CorrectedReads", "MappingRate", "DedupRate",
                "NumGenesOverMean", "MappedReads", "DeduplicatedReads",
                "NumGenesExpressed") %in% coln))) {
        missingFiles <- c(file.path(baseDir, "alevin/featureDump.txt"))
    }

    if (length(missingFiles) > 0) {
        return(missingFiles)
    }

    ## If we are here, the input is consistent with the provided
    ## 'type' argument.
    return(NULL)
}
