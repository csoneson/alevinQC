#' Knee plot of raw cell barcode frequencies
#'
#' Plot the raw cell barcode frequencies in decreasing order, and indicate a
#' predetermined breakpoint using color as well as a label.
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} with raw barcode frequencies (such as the
#'   \code{cbTable} returned by \code{readAlevinQC}).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line aes scale_x_log10 xlab ylab theme_bw
#'   theme scale_color_manual geom_label element_text
#' @import dplyr
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_pre0.14",
#'                                    package = "alevinQC"))
#' plotAlevinKneeRaw(alevin$cbTable)
#'
plotAlevinKneeRaw <- function(cbTable) {
    ggplot2::ggplot(cbTable,
                    ggplot2::aes(x = ranking, y = originalFreq)) +
        ggplot2::geom_line(size = 2, ggplot2::aes(color = inFirstWhiteList)) +
        ggplot2::scale_x_log10() +
        ggplot2::scale_y_log10() +
        ggplot2::xlab("Cell barcode rank") +
        ggplot2::ylab("Cell barcode frequency") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.title = ggplot2::element_text(size = 12)) +
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "black")) +
        ggplot2::geom_label(data = cbTable %>%
                                dplyr::filter(inFirstWhiteList) %>%
                                dplyr::filter(ranking == max(ranking)),
                            ggplot2::aes(label = paste0("(", ranking, ", ",
                                                        originalFreq, ")")),
                            hjust = 0, nudge_x = 0.1)
}
