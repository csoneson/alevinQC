#' Summary plot of cell barcode collapsing
#'
#' Plot the original frequency of each cell barcode in the original whitelist
#' against the frequency after collapsing similar cell barcodes. Points are
#' colored based on whether the corresponding cell barcodes are retained in the
#' final whitelist or not.
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with barcode frequencies before and after collapsing,
#'   as well as a column indicating whether the barcode is retained in the final
#'   whitelist.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_abline geom_point theme_bw theme
#'   scale_color_manual element_text xlab ylab
#' @import dplyr
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example",
#'                                    package = "alevinQC"))
#' plotAlevinBarcodeCollapse(alevin$cbTable)
#'
plotAlevinBarcodeCollapse <- function(cbTable) {
    ggplot2::ggplot(cbTable %>% dplyr::filter(inFirstWhiteList),
                    ggplot2::aes(x = originalFreq, y = collapsedFreq,
                                 color = inFinalWhiteList)) +
        ggplot2::geom_abline(slope = 1, intercept = 0) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title = ggplot2::element_text(size = 12)) +
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "darkgreen")) +
        ggplot2::xlab("Cell barcode frequency, original whitelist") +
        ggplot2::ylab("Cell barcode frequency, following reassignment")
}
