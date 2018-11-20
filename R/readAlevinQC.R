#' Read all input data
#'
#' @param baseDir Path to the output directory from the Alevin run (should be
#'   the directory containing the \code{alevin} directory).
#' @param quiet Logical, whether to show progress messages.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @importFrom utils read.delim
#' @import dplyr
#' @importFrom rjson fromJSON
#'
#' @return A list with all information necessary to create the alevinQC
#'   report/app.
#'
readAlevinQC <- function(baseDir, quiet = TRUE) {
    ## Check that all required files are available
    checkInputFiles(baseDir)

    alevinDir <- file.path(baseDir, "alevin")

    if (!quiet) message("Reading Alevin output files...")
    ## Raw CB frequencies
    rawcbfreq <- utils::read.delim(file.path(alevinDir, "raw_cb_frequency.txt"),
                                   header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, originalFreq = V2) %>%
        dplyr::mutate(ranking = seq_len(length(CB)))

    ## First set of whitelisted CBs (quantified)
    filtcbfreq <- read.delim(file.path(alevinDir, "filtered_cb_frequency.txt"),
                             header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, collapsedFreq = V2) %>%
        dplyr::left_join(rawcbfreq, by = "CB")

    ## FeatureDump
    featuredump <- read.delim(file.path(alevinDir, "featureDump.txt"),
                              header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, mappingRate = V2, duplicationRate = V3,
                      unknown1 = V4, nbrGenes = V5)

    ## Mapped UMI
    mappedumi <- read.delim(file.path(alevinDir, "MappedUmi.txt"),
                            header = FALSE, as.is = TRUE) %>%
        dplyr::rename(CB = V1, nbrMappedUMI = V2)

    ## Final set of whitelisted CBs
    finalwhitelist <- read.delim(file.path(alevinDir, "whitelist.txt"),
                                 header = FALSE, as.is = TRUE)$V1

    ## Quantification
    quantmat <- readAlevin(baseDir)
    quants <- data.frame(CB = colnames(quantmat),
                         totalUMICount = colSums(quantmat),
                         nbrGenes2 = colSums(quantmat >= 0.05),
                         stringsAsFactors = FALSE)

    ## Merge information about quantified CBs
    quantbcs <- filtcbfreq %>%
        dplyr::full_join(featuredump, by = "CB") %>%
        dplyr::full_join(mappedumi, by = "CB") %>%
        dplyr::full_join(quants, by = "CB") %>%
        dplyr::mutate(inFinalWhiteList = CB %in% finalwhitelist)

    ## Meta information and command information
    metainfo <- rjson::fromJSON(file = file.path(baseDir, "aux_info/meta_info.json"))
    cmdinfo <- rjson::fromJSON(file = file.path(baseDir, "cmd_info.json"))

    ## Create "version info" table
    versiontable <- t(data.frame(`Start time` = alevin$metainfo$start_time,
                                 `Salmon version` = alevin$metainfo$salmon_version,
                                 `Index` = alevin$cmdinfo$index,
                                 `R1file` = alevin$cmdinfo$mates1,
                                 `R2file` = alevin$cmdinfo$mates2,
                                 `tgMap` = alevin$cmdinfo$tgMap,
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE))

    ## Create summary table
    summarytable <- t(data.frame(`Total number of processed reads` = as.character(metainfo$num_processed),
                                 `Number of reads with valid barcode (no Ns)` = as.character(round(sum(rawcbfreq$originalFreq))),
                                 `Nbr whitelisted barcodes (first round)` = as.character(nrow(quantbcs)),
                                 `Fraction reads in whitelisted barcodes` = paste0(signif(100 * sum(quantbcs$collapsedFreq)/sum(rawcbfreq$originalFreq), 4), "%"),
                                 `Mean reads per cell` = round(mean(quantbcs$collapsedFreq)),
                                 `Median reads per cell` = round(stats::median(quantbcs$collapsedFreq)),
                                 `Median nbr detected genes` = stats::median(quantbcs$nbrGenes2),
                                 `Total nbr detected genes` = sum(rowSums(quantmat) > 0),
                                 `Median UMI count` = stats::median(quantbcs$totalUMICount),
                                 `Final nbr whitelisted barcodes` = sum(quantbcs$inFinalWhiteList),
                                 `Fraction reads in final whitelisted barcodes` = paste0(signif(100 *  sum(quantbcs$collapsedFreq[quantbcs$inFinalWhiteList])/sum(rawcbfreq$originalFreq), 4), "%"),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE))

    list(rawcbfreq = rawcbfreq, quantbcs = quantbcs,
         versiontable, summarytable)
}
