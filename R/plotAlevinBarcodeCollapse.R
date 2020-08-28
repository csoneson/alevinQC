#' Summary plot of cell barcode collapsing
#'
#' Plot the original frequency of each cell barcode in the original whitelist
#' against the frequency after collapsing similar cell barcodes.
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with barcode frequencies before and after collapsing.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_abline geom_point theme_bw theme
#'   element_text xlab ylab geom_label
#' @import dplyr
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinBarcodeCollapse(alevin$cbTable)
#'
plotAlevinBarcodeCollapse <- function(cbTable) {
    mrg <- cbTable %>% dplyr::filter(inFirstWhiteList) %>%
        dplyr::summarize(
            mrg = signif(100 * mean(collapsedFreq/originalFreq - 1,
                                    na.rm = TRUE), 4)) %>%
        dplyr::pull(mrg)
    ggplot2::ggplot(cbTable %>% dplyr::filter(inFirstWhiteList),
                    ggplot2::aes(x = originalFreq, y = collapsedFreq)) +
        ggplot2::geom_abline(slope = 1, intercept = 0) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title = ggplot2::element_text(size = 12)) +
        ggplot2::xlab("Cell barcode frequency, original whitelist") +
        ggplot2::ylab("Cell barcode frequency, following reassignment") +
        ggplot2::geom_label(x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
                            aes(label = paste0("Mean read gain per CB: ",
                                               mrg, "%")))
}
