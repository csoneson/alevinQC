#' Read alevin-fry data required to generate summary report
#'
#' Read all alevin-fry output files required to generate the
#' summary report or shiny app.
#'
#' @param mapDir Path to the output directory from the \code{salmon alevin}
#'     run (should be the directory containing the \code{alevin} folder).
#' @param permitDir Path to the output directory from the
#'     \code{generate-permit-list} and \code{collate} runs.
#' @param quantDir Path to the output directory from the
#'     \code{alevin-fry quant} run (should be the directory containing the
#'     \code{alevin} folder).
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @importFrom utils read.delim
#' @import dplyr
#' @importFrom rjson fromJSON
#'
#' @return A list collecting all necessary information for generating the
#'     summary report/shiny app.
#'
#' @examples
#' alevinfry <- readAlevinFryQC(
#'     mapDir = system.file("extdata/alevinfry_example_v0.5.0/map",
#'                          package = "alevinQC"),
#'     permitDir = system.file("extdata/alevinfry_example_v0.5.0/permit",
#'                             package = "alevinQC"),
#'     quantDir = system.file("extdata/alevinfry_example_v0.5.0/quant",
#'                            package = "alevinQC"))
#'
readAlevinFryQC <- function(mapDir, permitDir, quantDir) {
    ## Check that all required files are available, stop if not
    infversion <- checkAlevinFryInputFiles(mapDir = mapDir,
                                           permitDir = permitDir,
                                           quantDir = quantDir)

    ## Depending on the inferred version, read alevin output files
    if (infversion == "v0.4.3") {
        ## v0.4.3 or newer
        .readAlevinFryQC_v0.4.3(mapDir = mapDir, permitDir = permitDir,
                                quantDir = quantDir)
    } else if (infversion == "v0.5.0") {
        ## v0.5.0 or newer
        .readAlevinFryQC_v0.5.0(mapDir = mapDir, permitDir = permitDir,
                                quantDir = quantDir)
    } else {
        stop("Unidentifiable alevin-fry output")
    }
}

.readAlevinFryQC_v0.4.3 <- function(mapDir, permitDir, quantDir) {

    ## Raw CB frequencies (in descending order)
    rawcbfreq <- utils::read.delim(file.path(permitDir, "all_freq.tsv"),
                                   header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, originalFreq = V2) %>%
        dplyr::arrange(dplyr::desc(originalFreq)) %>%
        dplyr::mutate(ranking = seq_len(length(CB)))
    if (!all(diff(rawcbfreq$originalFreq) <= 0)) {
        warning("The raw CB frequencies are not sorted in decreasing order")
    }

    ## FeatureDump
    featuredump <- utils::read.delim(file.path(quantDir, "featureDump.txt"),
                          header = TRUE, as.is = TRUE, sep = "\t")
    featuredump <- featuredump %>%
        dplyr::rename(mappingRate = MappingRate,
                      collapsedFreq = CorrectedReads,
                      dedupRate = DedupRate,
                      nbrGenesAboveMean = NumGenesOverMean,
                      nbrMappedUMI = MappedReads,
                      totalUMICount = DeduplicatedReads,
                      nbrGenesAboveZero = NumGenesExpressed)

    permitlist <- utils::read.delim(file.path(quantDir, "alevin",
                                              "quants_mat_rows.txt"),
                                    header = FALSE, as.is = TRUE) %>%
        dplyr::pull(V1)

    ## Meta information and command information
    metainfo <- rjson::fromJSON(file = file.path(mapDir,
                                                 "aux_info/meta_info.json"))
    cmdinfo <- rjson::fromJSON(file = file.path(mapDir, "cmd_info.json"))
    quantinfo <- rjson::fromJSON(file = file.path(quantDir, "quant.json"))

    ## Merge information about quantified CBs
    cbtable <- dplyr::full_join(
        rawcbfreq,
        featuredump,
        by = "CB"
    )

    cbtable <- cbtable %>%
        dplyr::mutate(inPermitList = CB %in% permitlist)

    ## Check if there is any barcode that is not in the permitlist,
    ## but which has an original ranking lower than any barcode that is
    ## in the permitlist, and remove it.
    # toremove <-
    #     !cbtable$inPermitList &
    #     cbtable$ranking <= max(cbtable$ranking[cbtable$inPermitList])
    # if (any(toremove)) {
    #     warning("Excluding ", sum(toremove), " unquantified barcode",
    #             ifelse(sum(toremove) > 1, "s", ""),
    #             " with higher original frequency than barcodes ",
    #             "included in the permitlist: ",
    #             paste0(cbtable$CB[toremove], collapse = ", "))
    #     cbtable <- cbtable[!toremove, ]
    # }

    ## Add information from custom barcode sets (not implemented)
    customCBsummary <- list()

    ## Create "version info" table
    versiontable <- t(data.frame(
        `Start time` = metainfo$start_time,
        `Salmon version` = metainfo$salmon_version,
        `alevin-fry version (quant)` = quantinfo$version_str,
        `Index` = cmdinfo$index,
        `R1file` = paste(cmdinfo$mates1,
                         collapse = ", "),
        `R2file` = paste(cmdinfo$mates2,
                         collapse = ", "),
        `tgMap` = ifelse(is.null(cmdinfo$tgMap), "NA",
                         cmdinfo$tgMap),
        `Library type` = metainfo$library_types,
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    ## Create summary tables
    summarytable_full <- t(data.frame(
        `Total number of processed reads` =
            as.character(metainfo$num_processed),
        `Number of mapped reads` = metainfo$num_mapped,
        `Total number of observed cell barcodes` =
            as.character(length(unique(cbtable$CB))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    summarytable_permitlist <- .makeSummaryTable(
        cbtable = cbtable,
        colName = "inPermitList",
        cbName = " (permitlist)",
        countCol = "nbrMappedUMI",
        quantmat = NULL
    )


    ## Return
    list(cbTable = cbtable, versionTable = versiontable,
         summaryTables = c(list(fullDataset = summarytable_full,
                                permitlist = summarytable_permitlist),
                           customCBsummary),
         type = "alevin-fry"
    )
}

.readAlevinFryQC_v0.5.0 <- function(mapDir, permitDir, quantDir) {

    ## Raw CB frequencies (in descending order)
    rawcbfreq <- cpp_get_permit_freq_info(file.path(permitDir, "all_freq.bin"))
    rawcbfreq <- data.frame(CB = rawcbfreq[[1]],
                            originalFreq = rawcbfreq[[2]]) %>%
        dplyr::arrange(dplyr::desc(originalFreq)) %>%
        dplyr::mutate(ranking = seq_len(length(CB)))
    if (!all(diff(rawcbfreq$originalFreq) <= 0)) {
        warning("The raw CB frequencies are not sorted in decreasing order")
    }

    ## FeatureDump
    featuredump <- utils::read.delim(file.path(quantDir, "featureDump.txt"),
                                     header = TRUE, as.is = TRUE, sep = "\t")
    featuredump <- featuredump %>%
        dplyr::rename(mappingRate = MappingRate,
                      collapsedFreq = CorrectedReads,
                      dedupRate = DedupRate,
                      nbrGenesAboveMean = NumGenesOverMean,
                      nbrMappedUMI = MappedReads,
                      totalUMICount = DeduplicatedReads,
                      nbrGenesAboveZero = NumGenesExpressed)

    permitlist <- cpp_get_permit_freq_info(file.path(permitDir,
                                                     "permit_freq.bin"))[[1]]

    ## Meta information and command information
    metainfo <- rjson::fromJSON(file = file.path(mapDir,
                                                 "aux_info/meta_info.json"))
    cmdinfo <- rjson::fromJSON(file = file.path(mapDir, "cmd_info.json"))
    quantinfo <- rjson::fromJSON(file = file.path(quantDir, "quant.json"))

    ## Merge information about quantified CBs
    cbtable <- dplyr::full_join(
        rawcbfreq,
        featuredump,
        by = "CB"
    )

    cbtable <- cbtable %>%
        dplyr::mutate(inPermitList = CB %in% permitlist)

    ## Check if there is any barcode that is not in the permitlist,
    ## but which has an original ranking lower than any barcode that is
    ## in the permitlist, and remove it.
    # toremove <-
    #     !cbtable$inPermitList &
    #     cbtable$ranking <= max(cbtable$ranking[cbtable$inPermitList])
    # if (any(toremove)) {
    #     warning("Excluding ", sum(toremove), " unquantified barcode",
    #             ifelse(sum(toremove) > 1, "s", ""),
    #             " with higher original frequency than barcodes ",
    #             "included in the permitlist: ",
    #             paste0(cbtable$CB[toremove], collapse = ", "))
    #     cbtable <- cbtable[!toremove, ]
    # }

    ## Add information from custom barcode sets (not implemented)
    customCBsummary <- list()

    ## Create "version info" table
    versiontable <- t(data.frame(
        `Start time` = metainfo$start_time,
        `Salmon version` = metainfo$salmon_version,
        `alevin-fry version (quant)` = quantinfo$version_str,
        `Index` = cmdinfo$index,
        `R1file` = paste(cmdinfo$mates1,
                         collapse = ", "),
        `R2file` = paste(cmdinfo$mates2,
                         collapse = ", "),
        `tgMap` = ifelse(is.null(cmdinfo$tgMap), "NA",
                         cmdinfo$tgMap),
        `Library type` = metainfo$library_types,
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    ## Create summary tables
    summarytable_full <- t(data.frame(
        `Total number of processed reads` =
            as.character(metainfo$num_processed),
        `Number of mapped reads` = metainfo$num_mapped,
        `Total number of observed cell barcodes` =
            as.character(length(unique(cbtable$CB))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    summarytable_permitlist <- .makeSummaryTable(
        cbtable = cbtable,
        colName = "inPermitList",
        cbName = " (permitlist)",
        countCol = "nbrMappedUMI",
        quantmat = NULL
    )


    ## Return
    list(cbTable = cbtable, versionTable = versiontable,
         summaryTables = c(list(fullDataset = summarytable_full,
                                permitlist = summarytable_permitlist),
                           customCBsummary),
         type = "alevin-fry"
    )
}
