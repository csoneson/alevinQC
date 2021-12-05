#' Read alevin-fry data required to generate summary report
#'
#' Read all alevin-fry output files required to generate the
#' summary report or shiny app.
#'
#' @param mapDir Map directory
#' @param permitDir Permit list directory
#' @param quantDir Quant directory
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
#' #alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#' #                       package = "alevinQC"))
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
    } else {
        stop("Unidentifiable alevin-fry output")
    }
}

.readAlevinFryQC_v0.4.3 <- function(mapDir, permitDir, quantDir) {

    ## Raw CB frequencies (assumed to be in descending order)
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
    # alevinmetainfo <- rjson::fromJSON(
    #     file = file.path(baseDir, "aux_info/alevin_meta_info.json"))

    ## Merge information about quantified CBs
    cbtable <- dplyr::full_join(
        rawcbfreq,
        featuredump,
        by = "CB"
    )

    ## we have a whitelist.txt file representing the final whitelist
    cbtable <- cbtable %>%
        dplyr::mutate(inPermitList = CB %in% permitlist)
    ## Check if there is any barcode that is not in the first whitelist,
    ## but which has an original ranking lower than any barcode that is
    ## in the first whitelist, and remove it.
    toremove <-
        !cbtable$inPermitList &
        cbtable$ranking <= max(cbtable$ranking[cbtable$inPermitList])
    if (any(toremove)) {
        warning("Excluding ", sum(toremove), " unquantified barcode",
                ifelse(sum(toremove) > 1, "s", ""),
                " with higher original frequency than barcodes ",
                "included in the permitlist: ",
                paste0(cbtable$CB[toremove], collapse = ", "))
        cbtable <- cbtable[!toremove, ]
    }

    ## Add information from custom barcode sets
    customCBsummary <- list()

    ## Create "version info" table
    versiontable <- t(data.frame(
        `Start time` = metainfo$start_time,
        `Salmon version` = metainfo$salmon_version,
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
        # `Number of reads with Ns` =
        #     as.character(alevinmetainfo$reads_with_N),
        # `Number of reads with valid cell barcode (no Ns)` =
        #     as.character(round(sum(rawcbfreq$originalFreq, na.rm = TRUE))),
        `Number of mapped reads` = metainfo$num_mapped,
        # `Percent mapped (of all reads)` =
        #     paste0(signif(as.numeric(alevinmetainfo$mapping_rate), 4), "%"),
        # `Number of noisy CB reads` =
        #     as.character(alevinmetainfo$noisy_cb_reads),
        # `Number of noisy UMI reads` =
        #     as.character(alevinmetainfo$noisy_umi_reads),
        `Total number of observed cell barcodes` =
            as.character(length(unique(cbtable$CB))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    ## If used_reads is reported, add actual mapping rate
    # if (!is.null(alevinmetainfo$used_reads)) {
    #     summarytable_full <- rbind(
    #         summarytable_full,
    #         t(data.frame(
    #             `Number of used reads` = alevinmetainfo$used_reads,
    #             `Percent mapped (of used reads)` =
    #                 paste0(signif(100 * alevinmetainfo$reads_in_eqclasses /
    #                                   alevinmetainfo$used_reads, 4), "%"),
    #             stringsAsFactors = FALSE,
    #             check.names = FALSE
    #         ))
    #     )
    # }

    summarytable_permitlist <- .makeSummaryTable(
        cbtable = cbtable,
        colName = "inPermitList",
        cbName = " (permitlist)",
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
