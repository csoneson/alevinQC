#' Panel of plots with quantification summary statistics
#'
#' @author Charlotte Soneson
#'
#' @param cbTable \code{data.frame} (such as the \code{cbTable} returned by
#'     \code{readAlevinQC} or \code{readAlevinFryQC}) with collapsed barcode
#'     frequencies, the total UMI count and the number of detected genes for
#'     each cell.
#' @param colName Character scalar giving the name of a logical column of
#'     \code{cbTable} to use for coloring the points.
#' @param cbName Character scalar giving the name of the set of barcodes
#'     defined by \code{colName}, used for labelling the plot legend.
#' @param firstSelColName Character scalar indicating the name of the logical
#'     column in \code{cbTable} that indicates the original selection of
#'     barcodes for quantification.
#'
#' @export
#'
#' @importFrom ggplot2 aes scale_color_manual scale_fill_manual theme_bw theme
#'     labs geom_point ggplot xlab ylab
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
                            cbName = "final whitelist",
                            firstSelColName = "inFirstWhiteList") {
    ## Check input arguments
    .assertVector(x = cbTable, type = "data.frame")
    .assertScalar(x = colName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[colName]], type = "logical")
    .assertScalar(x = cbName, type = "character")
    .assertScalar(x = firstSelColName, type = "character",
                  validValues = colnames(cbTable))
    .assertVector(x = cbTable[[firstSelColName]], type = "logical")
    stopifnot(all(c("collapsedFreq", "totalUMICount",
                    "nbrGenesAboveZero") %in%
                      colnames(cbTable)))

    ## Keep only CBs in first selection
    cbTable <- cbTable %>% dplyr::filter(.data[[firstSelColName]])

    ## Define shared ggplot layers
    gglayers <- list(
        ggplot2::geom_point(alpha = 0.5),
        ggplot2::scale_color_manual(values = c(`TRUE` = "red",
                                               `FALSE` = "darkgreen")),
        ggplot2::theme_bw()
    )

    ## Generate plots
    g1 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = .data$collapsedFreq, y = .data$totalUMICount,
                     color = .data[[colName]])
    ) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Total UMI count")
    g2 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = .data$collapsedFreq, y = .data$nbrGenesAboveZero,
                     color = .data[[colName]])
    ) + gglayers +
        ggplot2::xlab("Barcode frequency") +
        ggplot2::ylab("Number of detected genes")
    g3 <- ggplot2::ggplot(
        cbTable,
        ggplot2::aes(x = .data$totalUMICount, y = .data$nbrGenesAboveZero,
                     color = .data[[colName]])
    ) + gglayers +
        ggplot2::xlab("Total UMI count") +
        ggplot2::ylab("Number of detected genes")

    ## Generate combined plot
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
