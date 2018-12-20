#' Panel of plots with quantification summary statistics
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with collapsed barcode frequencies, the total UMI
#'   count and the number of detected genes for each cell.
#'
#' @export
#'
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw theme
#'   labs geom_point ggplot xlab ylab
#' @import dplyr
#' @importFrom cowplot plot_grid get_legend
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example",
#'                                    package = "alevinQC"))
#' plotAlevinQuant(alevin$cbTable)
#'
plotAlevinQuant <- function(cbTable) {
    cbTable <- cbTable %>% dplyr::filter(inFirstWhiteList)
    gglayers <- list(
        ggplot2::geom_point(alpha = 0.5),
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "darkgreen")),
        ggplot2::theme_bw()
    )
    g1 <- ggplot2::ggplot(cbTable,
                          ggplot2::aes(x = collapsedFreq, y = totalUMICount,
                                       color = inFinalWhiteList)) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Total UMI count")
    g2 <- ggplot2::ggplot(cbTable,
                          ggplot2::aes(x = collapsedFreq, y = nbrGenesAboveZero,
                                       color = inFinalWhiteList)) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Number of detected genes")
    g3 <- ggplot2::ggplot(cbTable,
                          ggplot2::aes(x = totalUMICount, y = nbrGenesAboveZero,
                                       color = inFinalWhiteList)) + gglayers +
        ggplot2::xlab("Total UMI count") +
        ggplot2::ylab("Number of detected genes")

    cowplot::plot_grid(
        cowplot::plot_grid(
            g1 + ggplot2::theme(legend.position = "none"),
            g2 + ggplot2::theme(legend.position = "none"),
            g3 + ggplot2::theme(legend.position = "none"),
            nrow = 1, rel_widths = c(1, 1, 1)
        ),
        cowplot::get_legend(
            g1 + ggplot2::theme(legend.position = "bottom") +
                ggplot2::labs(color = "Barcode included in final whitelist")
        ),
        ncol = 1, rel_heights = c(1, 0.1)
    )
}
