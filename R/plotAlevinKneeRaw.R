#' Plot raw knee plot
#'
#' @author Charlotte Soneson
#'
#' @param rawcbfreq data.frame with raw barcode frequencies (as returned by
#'   \code{readAlevinQC}).
#' @param threshold The number of initially selected barcodes (will be indicated
#'   in the plot).
#'
#' @export
#'
#' @importFrom ggplot2 ggplot geom_line aes scale_x_log10 xlab ylab theme_bw
#'   theme scale_color_manual geom_label element_text
#' @import dplyr
#'
#' @return A ggplot object
#'
plotAlevinKneeRaw <- function(rawcbfreq, threshold) {
    ggplot2::ggplot(rawcbfreq %>%
                        dplyr::mutate(origwl = (ranking <= threshold)),
                    ggplot2::aes(x = ranking, y = originalFreq)) +
        ggplot2::geom_line(size = 2, ggplot2::aes(color = origwl)) +
        ggplot2::scale_x_log10() +
        ggplot2::scale_y_log10() +
        ggplot2::xlab("Cell barcode rank") +
        ggplot2::ylab("Cell barcode frequency") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none",
                       axis.title = ggplot2::element_text(size = 12)) +
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "black")) +
        ggplot2::geom_label(data = rawcbfreq[threshold, ],
                            ggplot2::aes(label = paste0("(", ranking, ", ",
                                                        originalFreq, ")")),
                            hjust = 0, nudge_x = 0.1)
}
