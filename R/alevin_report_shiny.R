#' Generate Alevin summary shiny app
#'
#' Generate a shiny app summarizing the main aspects of an Alevin quantification
#' run. The app generation assumes that Alevin has been run with the
#' --dumpFeatures flag to generate the necessary output files.
#'
#' @param baseDir Path to the output directory from the Alevin run (should be
#'   the directory containing the \code{alevin} directory).
#' @param sampleId Sample ID, will be used set the title for the app.
#'
#' @author Charlotte Soneson
#'
#' @export
#'
#' @importFrom ggplot2 aes xlab ylab theme_bw theme element_text geom_line ggplot scale_colour_manual geom_point geom_abline geom_label scale_x_log10 scale_y_log10
#' @importFrom GGally ggally_densityDiag ggally_points ggally_cor ggpairs
#' @import dplyr
#' @importFrom rjson fromJSON
#' @importFrom shiny fluidRow plotOutput renderPlot shinyApp
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar box
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom utils read.delim
#' @importFrom stats median
#'
#' @return A shiny app.
#'
alevinQCShiny <- function(baseDir, sampleId) {
    alevinDir <- file.path(baseDir, "alevin")

    ## -------------------------------------------------------------------------- ##
    ## Read files
    ## -------------------------------------------------------------------------- ##
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

    pLayout <- function() {
        shinydashboard::dashboardPage(
            skin = "red",

            shinydashboard::dashboardHeader(title = paste0("alevinQC, ", sampleId),
                                            titleWidth = (10 + nchar(sampleId)) * 20),

            shinydashboard::dashboardSidebar(disable = TRUE),

            shinydashboard::dashboardBody(
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Version info",
                        DT::dataTableOutput("first.table")
                    ),
                    shinydashboard::box(
                        title = "Summary table",
                        DT::dataTableOutput("second.table")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot I",
                        shiny::plotOutput("first.plot")
                    ),
                    shinydashboard::box(
                        title = "Barcode collapsing",
                        shiny::plotOutput("second.plot")
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::box(
                        title = "Knee plot II",
                        shiny::plotOutput("fourth.plot")
                    ),
                    shinydashboard::box(
                        title = "Quantification summary",
                        shiny::plotOutput("third.plot")
                    )
                )

            )
        )
    }

    server_function <- function(input, output, session) {
        output$first.table <- DT::renderDataTable(
            DT::datatable(
                t(data.frame(`Start time` = metainfo$start_time,
                             `Salmon version` = metainfo$salmon_version,
                             `Index` = cmdinfo$index,
                             `R1file` = cmdinfo$mates1,
                             `R2file` = cmdinfo$mates2,
                             `tgMap` = cmdinfo$tgMap,
                             stringsAsFactors = FALSE,
                             check.names = FALSE)),
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$second.table <- DT::renderDataTable(
            DT::datatable(
                t(data.frame(`Total number of processed reads` = as.character(metainfo$num_processed),
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
                             check.names = FALSE)),
                colnames = "",
                options = list(scrollX = TRUE)
            )
        )

        output$first.plot <- shiny::renderPlot(
            ggplot2::ggplot(rawcbfreq %>%
                                dplyr::mutate(origwl = (ranking <= nrow(quantbcs))),
                   ggplot2::aes(x = ranking, y = originalFreq)) +
                ggplot2::geom_line(size = 2, ggplot2::aes(color = origwl)) +
                ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
                ggplot2::xlab("Cell barcode rank") +
                ggplot2::ylab("Cell barcode frequency") +
                ggplot2::theme_bw() +
                ggplot2::theme(legend.position = "none",
                               axis.title = ggplot2::element_text(size = 12)) +
                ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                                       `FALSE` = "black")) +
                ggplot2::geom_label(data = rawcbfreq[nrow(quantbcs), ],
                                    ggplot2::aes(label = paste0("(", ranking, ", ",
                                                                originalFreq, ")")),
                                    hjust = 0, nudge_x = 0.1)
        )

        output$second.plot <- shiny::renderPlot(
            ggplot2::ggplot(quantbcs,
                            ggplot2::aes(x = originalFreq,
                                         y = collapsedFreq, color = inFinalWhiteList)) +
                ggplot2::geom_abline(slope = 1, intercept = 0) +
                ggplot2::geom_point() + ggplot2::theme_bw() +
                ggplot2::theme(axis.title = ggplot2::element_text(size = 12)) +
                ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                                       `FALSE` = "darkgreen")) +
                ggplot2::xlab("Cell barcode frequency, original whitelist") +
                ggplot2::ylab("Cell barcode frequency, following reassignment")
        )

        output$third.plot <- shiny::renderPlot(
            GGally::ggpairs(
                quantbcs %>%
                    dplyr::rename(`Barcode frequency` = "collapsedFreq",
                                  `Total UMI count` = "totalUMICount",
                                  `Nbr detected genes` = "nbrGenes2"),
                mapping = ggplot2::aes(colour = inFinalWhiteList),
                columns = c("Barcode frequency", "Total UMI count",
                            "Nbr detected genes"),
                upper = list(continuous = function(data, mapping, ...) {
                    GGally::ggally_cor(data = data, mapping = mapping) +
                        ggplot2::scale_colour_manual(values = c("darkgreen", "red"))}),
                lower = list(continuous = function(data, mapping, ...) {
                    GGally::ggally_points(data = data, mapping = mapping, alpha = 0.5) +
                        ggplot2::scale_colour_manual(values = c("darkgreen", "red"))}),
                diag = list(continuous = function(data, mapping, ...) {
                    GGally::ggally_densityDiag(data = data,
                                               mapping = mapping, alpha = 0.5) +
                        ggplot2::scale_fill_manual(values = c("darkgreen", "red"))})) +
                ggplot2::theme_bw()
        )

        output$fourth.plot <- shiny::renderPlot(
            ggplot2::ggplot(quantbcs %>% dplyr::arrange(desc(nbrGenes2)) %>%
                                dplyr::mutate(x = seq_along(nbrGenes2)),
                   ggplot2::aes(x = x, y = nbrGenes2)) +
                ggplot2::geom_line(size = 2) +
                ggplot2::xlab("Cell barcode rank") +
                ggplot2::ylab("Number of detected genes") +
                ggplot2::theme_bw() +
                ggplot2::theme(legend.position = "none",
                               axis.title = ggplot2::element_text(size = 12))
        )
    }

    shiny::shinyApp(ui = pLayout, server = server_function)
}
