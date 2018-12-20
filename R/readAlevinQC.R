#' Read alevin data required to generate summary report
#'
#' Read all alevin output files required to generate the summary report or shiny
#' app.
#'
#' @param baseDir Path to the output directory from the alevin run (should be
#'   the directory containing the \code{alevin} directory).
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @importFrom utils read.delim
#' @import dplyr
#' @importFrom rjson fromJSON
#' @importFrom tximport tximport
#'
#' @return A list collecting all necessary information for generating the
#'   summary report/shiny app.
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example",
#'                        package = "alevinQC"))
#'
readAlevinQC <- function(baseDir) {
    ## Check that all required files are available, stop if not
    checkAlevinInputFiles(baseDir)

    alevinDir <- file.path(baseDir, "alevin")

    ## Raw CB frequencies (assumed to be in descending order)
    rawcbfreq <- utils::read.delim(file.path(alevinDir, "raw_cb_frequency.txt"),
                                   header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, originalFreq = V2) %>%
        dplyr::mutate(ranking = seq_len(length(CB)))

    ## First set of whitelisted CBs (quantified)
    filtcbfreq <- read.delim(file.path(alevinDir, "filtered_cb_frequency.txt"),
                             header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, collapsedFreq = V2)

    ## FeatureDump
    ## dedupRate = nbr deduplicated UMI counts/nbr mapped reads
    ## nbrGenesAboveMean = nbr genes with count > mean gene count
    featuredump <- read.delim(file.path(alevinDir, "featureDump.txt"),
                              header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, mappingRate = V2, duplicationRate = V3,
                      dedupRate = V4, nbrGenesAboveMean = V5)

    ## Mapped UMI
    mappedumi <- read.delim(file.path(alevinDir, "MappedUmi.txt"),
                            header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, nbrMappedUMI = V2)

    ## Final set of whitelisted CBs
    finalwhitelist <- read.delim(file.path(alevinDir, "whitelist.txt"),
                                 header = FALSE, as.is = TRUE)$V1

    ## Quantification
    quantmat <- tximport::tximport(file.path(baseDir, "alevin/quants_mat.gz"),
                                   type = "alevin")$counts
    quants <- data.frame(CB = colnames(quantmat),
                         totalUMICount = colSums(quantmat),
                         nbrGenesAboveZero = colSums(quantmat > 0),
                         stringsAsFactors = FALSE)

    ## Merge information about quantified CBs
    quantbcs <- filtcbfreq %>%
        dplyr::full_join(featuredump, by = "CB") %>%
        dplyr::full_join(mappedumi, by = "CB") %>%
        dplyr::full_join(quants, by = "CB") %>%
        dplyr::mutate(inFinalWhiteList = CB %in% finalwhitelist) %>%
        dplyr::mutate(inFirstWhiteList = TRUE)

    cbtable <- dplyr::full_join(
        rawcbfreq,
        quantbcs
    ) %>% dplyr::mutate(inFirstWhiteList = replace(inFirstWhiteList,
                                                   is.na(inFirstWhiteList),
                                                   FALSE),
                        inFinalWhiteList = replace(inFinalWhiteList,
                                                   is.na(inFinalWhiteList),
                                                   FALSE))

    ## Meta information and command information
    metainfo <- rjson::fromJSON(file = file.path(baseDir,
                                                 "aux_info/meta_info.json"))
    cmdinfo <- rjson::fromJSON(file = file.path(baseDir, "cmd_info.json"))

    ## Create "version info" table
    versiontable <- t(data.frame(`Start time` = metainfo$start_time,
                                 `Salmon version` = metainfo$salmon_version,
                                 `Index` = cmdinfo$index,
                                 `R1file` = paste(cmdinfo$mates1,
                                                  collapse = ", "),
                                 `R2file` = paste(cmdinfo$mates2,
                                                  collapse = ", "),
                                 `tgMap` = cmdinfo$tgMap,
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE))

    ## Create summary tables
    summarytable_full <- t(data.frame(
        `Total number of processed reads` =
            as.character(metainfo$num_processed),
        `Number of reads with valid cell barcode (no Ns)` =
            as.character(round(sum(rawcbfreq$originalFreq, na.rm = TRUE))),
        `Total number of observed cell barcodes` =
            as.character(length(unique(cbtable$CB))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    summarytable_initialwl <- t(data.frame(
        `Number of barcodes in initial whitelist` =
            as.character(sum(cbtable$inFirstWhiteList, na.rm = TRUE)),
        `Fraction reads in initial whitelist barcodes` =
            as.character(paste0(signif(
                100 * sum(cbtable$collapsedFreq[cbtable$inFirstWhiteList],
                          na.rm = TRUE)/
                    sum(cbtable$originalFreq, na.rm = TRUE), 4), "%")),
        `Mean number of reads per cell (initial whitelist)` =
            as.character(round(mean(
                cbtable$collapsedFreq[cbtable$inFirstWhiteList],
                na.rm = TRUE))),
        `Median number of reads per cell (initial whitelist)` =
            as.character(round(stats::median(
                cbtable$collapsedFreq[cbtable$inFirstWhiteList],
                na.rm = TRUE))),
        `Median number of detected genes per cell (initial whitelist)` =
            as.character(round(stats::median(
                cbtable$nbrGenesAboveZero[cbtable$inFirstWhiteList],
                na.rm = TRUE))),
        `Total number of detected genes (initial whitelist)` =
            as.character(sum(rowSums(quantmat) > 0)),
        `Median UMI count per cell (initial whitelist)` =
            as.character(round(stats::median(
                cbtable$totalUMICount[cbtable$inFirstWhiteList],
                na.rm = TRUE))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    summarytable_finalwl <- t(data.frame(
        `Number of barcodes in final whitelist` =
            as.character(sum(cbtable$inFinalWhiteList, na.rm = TRUE)),
        `Fraction reads in final whitelist barcodes` =
            as.character(paste0(signif(
                100 * sum(cbtable$collapsedFreq[cbtable$inFinalWhiteList],
                          na.rm = TRUE)/
                    sum(cbtable$originalFreq, na.rm = TRUE), 4), "%")),
        `Mean number of reads per cell (final whitelist)` =
            as.character(round(mean(
                cbtable$collapsedFreq[cbtable$inFinalWhiteList],
                na.rm = TRUE))),
        `Median number of reads per cell (final whitelist)` =
            as.character(round(stats::median(
                cbtable$collapsedFreq[cbtable$inFinalWhiteList],
                na.rm = TRUE))),
        `Median number of detected genes per cell (final whitelist)` =
            as.character(round(stats::median(
                cbtable$nbrGenesAboveZero[cbtable$inFinalWhiteList],
                na.rm = TRUE))),
        `Total number of detected genes (final whitelist)` = as.character(sum(
            rowSums(quantmat[, colnames(quantmat) %in%
                                 cbtable$CB[cbtable$inFinalWhiteList]]) > 0)),
        `Median UMI count per cell (final whitelist)` =
            as.character(round(stats::median(
                cbtable$totalUMICount[cbtable$inFinalWhiteList],
                na.rm = TRUE))),
        stringsAsFactors = FALSE,
        check.names = FALSE
    ))

    ## Return
    list(cbTable = cbtable, versionTable = versiontable,
         summaryTables = list(fullDataset = summarytable_full,
                              initialWhitelist = summarytable_initialwl,
                              finalWhitelist = summarytable_finalwl))
}
