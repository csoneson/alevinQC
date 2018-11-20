#' Plot cell barcode collapsing
#'
#' @author Charlotte Soneson
#'
#' @param quantbcs data.frame with barcode frequencies (as returned by
#'   \code{readAlevinQC}).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_abline geom_point theme_bw theme
#'   scale_color_manual element_text xlab ylab
#'
#' @return A ggplot object
#'
plotAlevinBarcodeCollapse <- function(quantbcs) {
    ggplot2::ggplot(quantbcs, ggplot2::aes(x = originalFreq, y = collapsedFreq,
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
