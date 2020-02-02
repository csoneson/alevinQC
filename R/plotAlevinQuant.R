#' Panel of plots with quantification summary statistics
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'   \code{readAlevinQC}) with collapsed barcode frequencies, the total UMI
#'   count and the number of detected genes for each cell.
#' @param colName Character scalar giving the name of a logical column of
#'   \code{cbTable} to use for coloring the points.
#' @param cbName Character scalar giving the name of the set of barcodes
#'   defined by \code{colName}, used for labelling the plot legend.
#'
#' @export
#'
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw theme
#'   labs geom_point ggplot xlab ylab
#' @import dplyr
#' @importFrom cowplot plot_grid get_legend
#' @import rlang
#'
#' @return A ggplot object
#'
#' @examples
#' alevin <- readAlevinQC(system.file("extdata/alevin_example_v0.14",
#'                                    package = "alevinQC"))
#' plotAlevinQuant(alevin$cbTable, colName = "inFinalWhiteList",
#'                 cbName = "final whitelist")
#'
plotAlevinQuant <- function(cbTable, colName = "inFinalWhiteList",
                            cbName = "final whitelist") {
    stopifnot(is.logical(cbTable[[colName]]))

    cbTable <- cbTable %>% dplyr::filter(inFirstWhiteList)
    gglayers <- list(
        ggplot2::geom_point(alpha = 0.5),
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "darkgreen")),
        ggplot2::theme_bw()
    )
    g1 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = collapsedFreq, y = totalUMICount,
                     color = !!rlang::sym(colName))
    ) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Total UMI count")
    g2 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = collapsedFreq, y = nbrGenesAboveZero,
                     color = !!rlang::sym(colName))
    ) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Number of detected genes")
    g3 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = totalUMICount, y = nbrGenesAboveZero,
                     color = !!rlang::sym(colName))
    ) + gglayers +
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
                ggplot2::labs(color = paste0("Barcode included in ", cbName))
        ),
        ncol = 1, rel_heights = c(1, 0.1)
    )
}
